module Osc.fs
open System
open System.Collections.Concurrent
open System.IO
open System.Runtime.CompilerServices
open System.Text
open System.Text.RegularExpressions
open System.Threading
open System.Runtime.InteropServices
open FSharp.NativeInterop

module internal Async =
    let map f x = async {
        let! result = x
        return f result
    }

type MalformedMessageException =
    inherit Exception

    new(message: string) = { inherit Exception(message) }
    new(message: string, innerExn: Exception) = { inherit Exception(message, innerExn) }

let private raiseUnexpectedEof () = raise (MalformedMessageException("Unexpected end of stream"))
let private raiseImpossible () = raise (InvalidOperationException("This should never be thrown"))

// OSC spec: http://opensoundcontrol.org/spec-1_0.html

// TODO: time tags and bundles

//[<IsByRefLike; Struct>]
type OscAtom =
    /// 32-bit big-endian signed two's complement integer
    | OscInt32 of intValue:int32
    //| OscTimetag
    /// 32-bit big-endian IEEE 754 floating point number
    | OscFloat32 of floatValue:float32
    /// A sequence of non-null ASCII characters followed by a null, followed by 0-3 additional null characters to make
    /// the total number of bits a multiple of 32.
    | OscString of stringValue:string
    /// Arbitrary binary data
    | OscBlob of blobData:byte[]

type OscMessage = { addressPattern: string; arguments: OscAtom list }

// TODO: Check big-endian systems. This *should* work correctly, but I haven't tested it
let parseOscInt32Async (input: Stream) = async {
    let! bytes = input.AsyncRead 4
    let bytes' = if BitConverter.IsLittleEndian then Array.rev bytes else bytes
    return BitConverter.ToInt32 (bytes', 0)
}

let writeOscInt32Async (output: Stream) (value: int32) = async {
    let bytes = BitConverter.GetBytes value
    let bytes' = if BitConverter.IsLittleEndian then Array.rev bytes else bytes
    do! output.AsyncWrite bytes'
}

// TODO: see above
let parseOscFloat32Async (input: Stream) = async {
    let! bytes = input.AsyncRead 4
    let bytes' = if BitConverter.IsLittleEndian then Array.rev bytes else bytes
    return BitConverter.ToSingle (bytes', 0)
}

let writeOscFloat32Async (output: Stream) (value: float32) = async {
    let bytes = BitConverter.GetBytes value
    let bytes' = if BitConverter.IsLittleEndian then Array.rev bytes else bytes
    do! output.AsyncWrite bytes'
}

let parseOscStringAsync (input: Stream) = async {
    let strs = System.Collections.Generic.List<string>()
    let mutable cont = true
    let byteArr = Array.zeroCreate<byte> 4
    
    // FIXME: eliminiate any unnecessary extra allocations here
    while cont do
        let! bytesRead = input.AsyncRead byteArr
        if bytesRead <> 4 then raiseUnexpectedEof ()

        let len =
            if byteArr.[0] = 0uy then 0
            elif byteArr.[1] = 0uy then 1
            elif byteArr.[2] = 0uy then 2
            elif byteArr.[3] = 0uy then 3
            else 4
    
        let str = Encoding.ASCII.GetString (byteArr, 0, len)
        strs.Add str
    
        cont <- len >= 4
    
    return String.Concat(strs)
}

let internal strPaddingBuffers = [|
    [|0uy;0uy;0uy;0uy|]
    [|0uy;0uy;0uy|]
    [|0uy;0uy|]
    [|0uy|]
|]

let internal blobPaddingBuffers = [|
    [||]
    [|0uy;0uy;0uy|]
    [|0uy;0uy|]
    [|0uy|]
|]

let writeOscStringAsync (output: Stream) (value: string) = async {
    let bytes = Encoding.ASCII.GetBytes value
    do! output.AsyncWrite bytes
    do! output.AsyncWrite (strPaddingBuffers.[value.Length % 4])
}

let parseOscBlobAsync (input: Stream) = async {
    let! size = parseOscInt32Async input
    let! data = input.AsyncRead size
    let paddingSize = 3 - ((size + 3) % 4)
    if paddingSize > 0 then
        input.Seek (int64 paddingSize, SeekOrigin.Current) |> ignore
    return data
}

let writeOscBlobAsync (output: Stream) (value: byte[]) = async {
    do! writeOscInt32Async output value.Length
    do! output.AsyncWrite value
    do! output.AsyncWrite (blobPaddingBuffers.[value.Length % 4])
}

let parseOscTypeTagAsync (input: Stream) = async {
    let! str = parseOscStringAsync input
    if str.Length = 0 then
        return raise (MalformedMessageException($"Invalid type tag. Expected ',' but got null"))
    elif str.[0] <> ',' then
        return raise (MalformedMessageException($"Invalid type tag. Expected ',' but got '{str.[0]}'"))
    else
        return str.[1..str.Length-1]
}

let internal commaBuf = [|byte ','|]

let writeOscTypeTagAsync (output: Stream) (value: string) = async {
    let bytes = Encoding.ASCII.GetBytes value
    do! output.AsyncWrite commaBuf
    do! output.AsyncWrite bytes
    do! output.AsyncWrite (strPaddingBuffers.[(bytes.Length + 1) % 4])
}

let parseOscAddressPatternAsync (input: Stream) = async {
    let! str = parseOscStringAsync input
    if str.Length = 0 then
        return raise (MalformedMessageException($"Invalid address pattern. Expected '/' but got null"))
    elif str.[0] <> '/' then
        return raise (MalformedMessageException($"Invalid address pattern. Expected '/' but got '{str.[0]}'"))
    else return str
}

let writeOscAddressPatternAsync (input: Stream) (value: string) = async {
    do! writeOscStringAsync input value
}

let parseOscMessageAsync (input: Stream) = async {
    let! addr = parseOscAddressPatternAsync input
    if addr = "#bundle" then raise (NotImplementedException "Bundles not yet supported")
    let! typeTag = parseOscTypeTagAsync input
    let! args =
        typeTag
        |> Seq.map (fun tag ->
            match tag with
            | 'i' -> parseOscInt32Async input |> Async.map OscInt32
            | 'f' -> parseOscFloat32Async input |> Async.map OscFloat32
            | 's' | 'S' -> parseOscStringAsync input |> Async.map OscString
            | 'b' -> parseOscBlobAsync input |> Async.map OscBlob
            | _ -> raise (MalformedMessageException($"Unknown data type tag '{tag}'"))
        )
        |> Async.Sequential
    return { addressPattern = addr; arguments = Array.toList args }
}

let writeOscMessageAsync (output: Stream) (value: OscMessage) = async {
    do! writeOscAddressPatternAsync output value.addressPattern
    let (typesChars, writeFuncs) =
        value.arguments
        |> Seq.map (fun arg ->
            match arg with
            | OscInt32 x -> 'i', (fun () -> writeOscInt32Async output x)
            | OscFloat32 x -> 'f', (fun () -> writeOscFloat32Async output x)
            | OscString x -> 's', (fun () -> writeOscStringAsync output x)
            | OscBlob x -> 'b', (fun () -> writeOscBlobAsync output x)
        )
        |> Seq.toArray
        |> Array.unzip
    let typeTag = typesChars |> String
    do! writeOscTypeTagAsync output typeTag
    for writeFunc in writeFuncs do
        do! writeFunc ()
}

type DispatchTable =
    | Path of name:string * children: DispatchTable list
    | Method of name:string * (OscMessage -> Async<unit>)

let internal pathPartRegexes = ConcurrentDictionary<string, Regex>()

// matches the OSC address pattern {foo,bar,baz}
let internal orListPatternRegex = Regex("""\{.*\}""")

let internal getPathPartRegex (pattern: string) =
    match pathPartRegexes.TryGetValue pattern with
    | true, r -> r
    | false, _ ->
        let inner =
            pattern
                // escape unintended regex special symbols
                .Replace("\\", "\\\\").Replace("|", "\\|").Replace("+", "\\+")
                .Replace("$", "\\$").Replace("^", "\\^").Replace(".", "\\.").Replace("(", "\\(").Replace(")","\\)")
                // '*' '?' and '[!abc]' patterns
                .Replace("*", ".*").Replace("?", ".?").Replace("[!", "[^")
        // '{foo,bar,baz}' pattern which we translate into the regex group '(foo|bar|baz)'
        let inner' = orListPatternRegex.Replace (inner, (fun m -> m.Value.Replace(",", "|").Replace("{","(").Replace("}",")")))
        // char classes just fall thru; OSC class chars are valid regex class chars too
        let r = Regex($"^{inner'}$")
        pathPartRegexes.[pattern] <- r
        r

let dispatchMessage table (msg: OscMessage) = async {
    let mutable anyDispatched = false
    let rec dispatchMessageInner table msg path = async {
        match path, table with
        // represents the // wildcard, like in foo//bar. This will get split out to ["foo";"";"bar"], and we want to
        // allow the "//" (which becomes "") to match to multiple levels of paths
        | ""::partAfterMultilevelWildcard::rest, Path (name, children) ->
            let p = if (getPathPartRegex partAfterMultilevelWildcard).IsMatch name then rest else path
            for child in children do
                do! dispatchMessageInner child msg p
        // same as above
        | ""::partAfterMultilevelWildcard::_, Method (name, methodFunc) ->
            if (getPathPartRegex partAfterMultilevelWildcard).IsMatch name then
                match! Async.Catch (methodFunc msg) with
                | Choice1Of2 () -> ()
                | Choice2Of2 e -> eprintfn "Error during method dispatch: %O" e
        | part::rest, Path (name, children) when (getPathPartRegex part).IsMatch name ->
            for child in children do
                do! dispatchMessageInner child msg rest
        | part::_, Method (name, methodFunc) when (getPathPartRegex part).IsMatch name ->
            anyDispatched <- true
            match! Async.Catch (methodFunc msg) with
            | Choice1Of2 () -> ()
            | Choice2Of2 e -> eprintfn "Error during method dispatch: %O" e
        | _ -> ()
    }
    let parts = msg.addressPattern.TrimStart('/').Split('/') |> Array.toList
    for node in table do
        do! dispatchMessageInner node msg parts
    if not anyDispatched then
        eprintfn "%s did not match any methods" msg.addressPattern
}


open System.Net
open System.Net.Sockets

type IOscClient =
    inherit IDisposable
    abstract member SendMessageAsync : msg:OscMessage -> Async<unit>

type IOscServer =
    inherit IDisposable
    abstract member Run : unit -> unit
    abstract member RunInThreadPool : unit -> unit

let private resetMemoryStream (stream: MemoryStream) =
    let buffer = stream.GetBuffer ()
    Array.Clear (buffer, 0, buffer.Length)
    stream.Position <- 0L
    stream.SetLength 0L

type OscUdpClient(localEP: IPEndPoint) =
    let udpClient = new UdpClient(localEP)
    let tempStream = new MemoryStream()

    new(host: IPAddress, port: int) = new OscUdpClient(IPEndPoint(host, port))
    new(host: string, port: int) = new OscUdpClient(IPAddress.Parse host, port)

    member this.SendMessageAsync (msg: OscMessage) = async {
        resetMemoryStream tempStream

        do! writeOscMessageAsync tempStream msg
        do! Async.AwaitTask (tempStream.FlushAsync ())
        let buffer = tempStream.GetBuffer ()
        let! _ = Async.AwaitTask (udpClient.SendAsync (buffer, int tempStream.Length))
        return ()
    }

    member this.Dispose () =
        udpClient.Dispose ()
        tempStream.Dispose ()

    interface IOscClient with
        override this.SendMessageAsync msg = this.SendMessageAsync msg

    interface IDisposable with
        override this.Dispose () = ()

type OscUdpServer(host: IPAddress, port: int, methods: DispatchTable list) =
    let cts = new CancellationTokenSource()
    let mutable running = false

    new(host: string, port, methods) = new OscUdpServer(IPAddress.Parse host, port, methods)

    member val Methods = methods with get, set

    member private this.ReadAndProcessMessage (udpClient: UdpClient) = async {
        let! data = Async.AwaitTask (udpClient.ReceiveAsync ())
        let stream = new MemoryStream(data.Buffer)
        let! msg = parseOscMessageAsync stream
        
        dispatchMessage this.Methods msg
        |> Async.Catch
        |> Async.map (fun result -> match result with | Choice1Of2 () -> () | Choice2Of2 e -> eprintfn "Message dispatch failed: %O" e)
        |> Async.Start
    }

    member private this.MessageLoop (udpClient: UdpClient) = async {
        while true do
            match! Async.Catch (this.ReadAndProcessMessage udpClient) with
            | Choice1Of2 result -> return result
            | Choice2Of2 e -> eprintfn "Error processing packet: %s" (string e)
    }

    member private this.Start () = async {
        if running then raise (IOException("Server already listening"))
        use udpClient = new UdpClient()
        udpClient.Client.Bind (IPEndPoint (host, port))
        printfn "Listening for packets on %O:%d" host port
        do! this.MessageLoop udpClient
    }

    member this.Run () = Async.RunSynchronously (this.Start (), cancellationToken = cts.Token)
    member this.RunInThreadPool () = Async.Start (this.Start (), cts.Token)

    interface IOscServer with
        override this.Run () = this.Run ()
        override this.RunInThreadPool () = this.RunInThreadPool ()

    member _.Dispose () =
        try
            cts.Cancel ()
        finally
            cts.Dispose ()

    interface IDisposable with
        member this.Dispose () = this.Dispose ()

type OscTcpServer(host: IPAddress, port: int, methods: DispatchTable list) =
    let cts = new CancellationTokenSource()
    let mutable running = false

    new(host: string, port, methods) = new OscTcpServer(IPAddress.Parse host, port, methods)

    member val Methods = methods with get, set

    member private this.ReadAndProcessMessage (tcpClient: TcpClient) = async {
        use tmpStream = new MemoryStream()
        let! msg = parseOscMessageAsync tmpStream
        let netStream = tcpClient.GetStream ()
        let tmpStreamBuffer = tmpStream.GetBuffer ()
        // first, write a big-endian int32 indicating the size of the OSC packet
        do! writeOscInt32Async netStream tmpStreamBuffer.Length
        // then, write the packet itself
        do! Async.AwaitTask (tmpStream.CopyToAsync netStream)
        match! Async.Catch (dispatchMessage this.Methods msg) with
        | Choice1Of2 () -> ()
        | Choice2Of2 e -> eprintfn "Message dispatch failed: %O" e
    }

    member private this.MessageLoop (tcpClient: TcpClient) = async {
        while tcpClient.Connected do
            match! Async.Catch (this.ReadAndProcessMessage tcpClient) with
            | Choice1Of2 result -> return result
            | Choice2Of2 e -> eprintfn "Error processing packet: %s" (string e)
    }

    member private this.HandleConnections (tcpListener: TcpListener) = async {
        while true do
            try
                let! connection = Async.AwaitTask (tcpListener.AcceptTcpClientAsync ())
                Async.Start (this.MessageLoop (connection))
            with e -> eprintfn "Error handling connection: %O" e
    }

    member private this.Start () = async {
        if running then raise (IOException("Server already listening"))
        let tcpListener = new TcpListener(host, port)
        tcpListener.Start ()
        printfn "Listening at %O:%d" host port
        do! this.HandleConnections tcpListener
    }

    member this.Run () = Async.RunSynchronously (this.Start (), cancellationToken = cts.Token)
    member this.RunInThreadPool () = Async.Start (this.Start (), cts.Token)

    interface IOscServer with
        override this.Run () = this.Run ()
        override this.RunInThreadPool () = this.RunInThreadPool ()

    member _.Dispose () =
        try
            cts.Cancel ()
        finally
            cts.Dispose ()

    interface IDisposable with
        member this.Dispose () = this.Dispose ()

[<AutoOpen>]
module Extensions =
    type IOscClient with
        member this.SendMessage msg = Async.RunSynchronously (this.SendMessageAsync msg)
