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

[<assembly: InternalsVisibleTo("Tests")>]
do ()

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
    | OscBool of bool
    | OscNone
    | OscImpulse

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
            | 'T' -> async { return OscBool true }
            | 'F' -> async { return OscBool false }
            | 'N' -> async { return OscNone }
            | 'I' -> async { return OscImpulse }
            | _ -> raise (MalformedMessageException($"Unknown data type tag '{tag}'"))
        )
        |> Async.Sequential
    return { addressPattern = addr; arguments = Array.toList args }
}

let writeOscMessageAsync (output: Stream) (value: OscMessage) = async {
    do! writeOscAddressPatternAsync output value.addressPattern
    let noop () = async { () }
    let (typesChars, writeFuncs) =
        value.arguments
        |> Seq.map (fun arg ->
            match arg with
            | OscInt32 x -> 'i', (fun () -> writeOscInt32Async output x)
            | OscFloat32 x -> 'f', (fun () -> writeOscFloat32Async output x)
            | OscString x -> 's', (fun () -> writeOscStringAsync output x)
            | OscBlob x -> 'b', (fun () -> writeOscBlobAsync output x)
            | OscBool true -> 'T', noop
            | OscBool false -> 'F', noop
            | OscNone -> 'N', noop
            | OscImpulse -> 'I', noop
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
open System.Threading.Tasks

type FrameScheme =
    /// OSC 1.0 - Size framing
    | Osc1_0
    /// OSC 1.1 - SLIP framing
    | Osc1_1

type internal IUdpClient =
    inherit IDisposable
    abstract member Connect: hostname:string * port:int -> unit
    abstract member Connect: endPoint:IPEndPoint -> unit
    abstract member ReceiveAsync: unit -> Task<UdpReceiveResult>
    abstract member SendAsync: datagram:byte[] * bytes:int -> Task<int>

type internal ITcpClient =
    inherit IDisposable
    abstract member ConnectAsync: host:IPAddress*port:int -> Task
    abstract member ConnectAsync: host:string*port:int -> Task
    abstract member GetStream: unit -> Stream

type internal ITcpListener =
    abstract member Start: unit -> unit
    abstract member AcceptTcpClientAsync: unit -> Task<ITcpClient>

type internal TcpClientImpl(client: TcpClient) =
    interface ITcpClient with
        override _.ConnectAsync (host: IPAddress, port: int) = client.ConnectAsync (host, port)
        override _.ConnectAsync (host: string, port: int) = client.ConnectAsync (host, port)
        override _.GetStream () = client.GetStream () :> Stream
    interface IDisposable with
        override _.Dispose () = client.Dispose ()

type internal UdpClientImpl(client: UdpClient) =
    interface IUdpClient with
        override _.Connect (endPoint: IPEndPoint) = client.Connect endPoint
        override _.Connect (hostname: string, port: int) = client.Connect (hostname, port)
        override _.ReceiveAsync () = client.ReceiveAsync ()
        override _.SendAsync (datagram, bytes) = client.SendAsync (datagram, bytes)
    
    interface IDisposable with
        override _.Dispose () = client.Dispose ()

type TcpListenerImpl(listener: TcpListener) =
    interface ITcpListener with
        override _.AcceptTcpClientAsync () = 
            listener.AcceptTcpClientAsync().ContinueWith(fun (t: Task<TcpClient>) -> new TcpClientImpl(t.Result) :> ITcpClient)
        override _.Start () = listener.Start ()

type IOscClient =
    inherit IDisposable
    abstract member SendMessageAsync : msg:OscMessage -> Async<unit>

type IOscServer =
    inherit IDisposable
    abstract member Run: unit -> IDisposable
    abstract member RunSynchronously: unit -> unit

let private resetMemoryStream (stream: MemoryStream) =
    let buffer = stream.GetBuffer ()
    Array.Clear (buffer, 0, buffer.Length)
    stream.Position <- 0L
    stream.SetLength 0L

type OscUdpClient internal(localEP: IPEndPoint, udpClient: IUdpClient) =
    let tempStream = new MemoryStream()
    do
        udpClient.Connect localEP

    new(localEP: IPEndPoint) =
        new OscUdpClient(localEP, new UdpClientImpl(new UdpClient()))
    new(host: IPAddress, port: int) = new OscUdpClient(IPEndPoint(host, port))
    new(host: string, port: int) = new OscUdpClient(IPAddress.Parse host, port)

    internal new(host: string, port: int, makeUdpClient: IPEndPoint -> IUdpClient) =
        let localEP = IPEndPoint(IPAddress.Parse host, port)
        new OscUdpClient(localEP, makeUdpClient localEP)

    member this.SendMessageAsync (msg: OscMessage) = async {
        resetMemoryStream tempStream

        do! writeOscMessageAsync tempStream msg
        
        let data = tempStream.ToArray()
        do! Async.AwaitTask (tempStream.FlushAsync ())
        let! _ = Async.AwaitTask (udpClient.SendAsync (data, int tempStream.Length))
        return ()
    }

    member this.Dispose () =
        udpClient.Dispose ()
        tempStream.Dispose ()

    interface IOscClient with
        override this.SendMessageAsync msg = this.SendMessageAsync msg

    interface IDisposable with
        override this.Dispose () = ()

type OscUdpServer internal(host: IPAddress, port: int, makeUdpClient: IPEndPoint -> IUdpClient, dispatch: OscMessage -> Async<unit>, ?onError: Exception -> unit) =
    let cts = new CancellationTokenSource()
    let mutable running = false
    let mutable disposed = false

    let onError = defaultArg onError ignore

    internal new(host: string, port, makeUdpClient, dispatch, ?onError) = new OscUdpServer(IPAddress.Parse host, port, makeUdpClient, dispatch, ?onError = onError)

    member private this.ReadAndProcessMessage (udpClient: IUdpClient) = async {
        let! data = Async.AwaitTask (udpClient.ReceiveAsync ())
        let stream = new MemoryStream(data.Buffer)
        let! msg = parseOscMessageAsync stream
        
        dispatch msg
        |> Async.Catch
        |> Async.map (fun result ->
            match result with
            | Choice1Of2 () -> ()
            | Choice2Of2 e ->
                eprintfn "Message dispatch failed: %O" e
                try onError e with e' -> eprintfn "Error inside error handler %O" e'
        )
        |> Async.Start
    }

    member private this.MessageLoop (udpClient: IUdpClient) = async {
        while true do
            match! Async.Catch (this.ReadAndProcessMessage udpClient) with
            | Choice1Of2 result -> return result
            | Choice2Of2 e ->
                eprintfn "Error processing packet: %s" (string e)
                try onError e with e' -> eprintfn "Error inside error handler %O" e'
    }

    /// Asynchronous starts listening and processing packets, calling back when the server is shut down.
    member private this.RunAsync () = async {
        if running then raise (IOException("Server already listening"))
        let endPoint = IPEndPoint (host, port)
        use udpClient = makeUdpClient endPoint
        //udpClient.Connect (IPEndPoint (host, port))
        do! this.MessageLoop udpClient
    }

    /// Starts listening for and processing packets on the current thread.
    member this.RunSynchronously () = Async.RunSynchronously (this.RunAsync (), cancellationToken = cts.Token)

    /// Starts listening for and processing packets in the thread pool, returning an IDisposable that, when disposed, stops and cleans up the listener.
    member this.Run () =
        Async.Start (this.RunAsync (), cancellationToken = cts.Token)
        this :> IDisposable

    //member this.Run () = Async.RunSynchronously (this.Start (), cancellationToken = cts.Token)
    //member this.RunInThreadPool () = Async.Start (this.Start (), cts.Token)

    interface IOscServer with
        override this.Run () = this.Run ()
        override this.RunSynchronously () = this.RunSynchronously ()

    member _.Dispose () =
        if not disposed then
            try
                cts.Cancel ()
            finally
                cts.Dispose ()
            disposed <- true

    interface IDisposable with
        member this.Dispose () = this.Dispose ()

// I'm actually not sure if this works right, or if TouchOSC is broken (the former is unlikely) -- I've tried to test this against TouchOSC but nothing happens.... Hmm...
// The UDP client and server works against TouchOSC, though...
type OscTcpClient internal(tcpClient: ITcpClient, ?frameScheme) =
    let cts = new CancellationTokenSource()
    let mutable disposed = false

    let [<Literal>] END = 0xC0uy
    let [<Literal>] ESC = 0xDBuy
    let [<Literal>] ESC_END = 0xDCuy
    let [<Literal>] ESC_ESC = 0xDDuy

    let frameScheme = defaultArg frameScheme Osc1_0

    new(tcpClient: TcpClient, ?frameScheme) = new OscTcpClient(new TcpClientImpl(tcpClient), ?frameScheme = frameScheme)

    member this.ConnectAsync (host: IPAddress, port: int) = async { return! Async.AwaitTask (tcpClient.ConnectAsync (host, port)) }
    member this.ConnectAsync (host: string, port: int) = async { return! Async.AwaitTask (tcpClient.ConnectAsync (host, port)) }

    static member ConnectAsync (host: IPAddress, port: int, ?frameScheme, ?onError) = async {
        let tcpClient = new TcpClientImpl(new TcpClient()) :> ITcpClient
        do! Async.AwaitTask (tcpClient.ConnectAsync (host, port))
        return new OscTcpClient(tcpClient, ?frameScheme = frameScheme)
    }

    static member ConnectAsync (host: string, port: int, ?frameScheme, ?onError) = async {
        let tcpClient = new TcpClientImpl(new TcpClient()) :> ITcpClient
        do! Async.AwaitTask (tcpClient.ConnectAsync (host, port))
        return new OscTcpClient(tcpClient, ?frameScheme = frameScheme)
    }

    static member Connect (host: IPAddress, port: int, ?frameScheme, ?onError) = Async.RunSynchronously (OscTcpClient.ConnectAsync (host, port, ?frameScheme = frameScheme, ?onError = onError))
    static member Connect (host: string, port: int, ?frameScheme, ?onError) = Async.RunSynchronously (OscTcpClient.ConnectAsync (host, port, ?frameScheme = frameScheme, ?onError = onError))

    member this.SendMessageAsync (msg: OscMessage) = async {
        let ioStream = tcpClient.GetStream ()
        match frameScheme with
        | Osc1_0 ->
            use tmpStream = new MemoryStream()
            // Size framing - write the size of the stream first
            do! writeOscMessageAsync tmpStream msg
            tmpStream.Position <- 0L
            do! writeOscInt32Async ioStream (int tmpStream.Length)
            do! Async.AwaitTask (tmpStream.CopyToAsync ioStream)
        | Osc1_1 ->
            use tmpStream = new MemoryStream()
            do! writeOscMessageAsync tmpStream msg
            let arr = tmpStream.ToArray()

            // SLIP encoding with double-end bytes
            ioStream.WriteByte END
            // escaping the special bytes
            let mutable startI = 0

            let inline escapeSpecialBytes transposedByte i = async {
                do! ioStream.AsyncWrite (arr, startI, i - startI)
                ioStream.WriteByte ESC
                ioStream.WriteByte transposedByte
                startI <- i+1
            }

            for i in 0 .. arr.Length - 1 do
                match arr.[i] with
                | END -> do! escapeSpecialBytes ESC_END i
                | ESC -> do! escapeSpecialBytes ESC_ESC i
                | _ -> ()
            // write loop never had to do anything
            if startI < arr.Length then
                do! ioStream.AsyncWrite (arr, startI, arr.Length - startI)
            ioStream.WriteByte END
    }

    interface IOscClient with
        override this.SendMessageAsync msg = this.SendMessageAsync msg

    interface IDisposable with
        override this.Dispose () =
            if not disposed then
                cts.Dispose ()
                tcpClient.Dispose ()

type OscTcpServer internal(tcpListener: ITcpListener, dispatch: OscMessage -> Async<unit>, ?frameScheme, ?onError) =
    let cts = new CancellationTokenSource()
    let mutable disposed = false

    let onError = defaultArg onError ignore
    
    let frameScheme = defaultArg frameScheme Osc1_0
    do
    // TODO: implement OSC 1.1 SLIP framing
        if frameScheme = Osc1_1 then raise (NotImplementedException("OSC 1.1 frame scheme (SLIP encoding) not implemented yet"))

    internal new(host: IPAddress, port: int, dispatch, ?frameScheme, ?onError) =
        new OscTcpServer(new TcpListenerImpl(new TcpListener(host, port)), dispatch, ?frameScheme = frameScheme, ?onError = onError)

    member private this.HandleClient (clientStream: Stream) = async {
        try
            while true do
                // TODO: make sure we're handling clients closing connections correctly
                let! msg = parseOscMessageAsync clientStream
                do! dispatch msg
        with e ->
            eprintfn "Error handling client: %O" e
    }

    member private this.RunAsync () = async {
        try
            while true do
                let! client = Async.AwaitTask (tcpListener.AcceptTcpClientAsync ())
                Async.Start (this.HandleClient (client.GetStream ()))
        with e ->
            // Can't use reraise() inside async: https://github.com/fsharp/fslang-suggestions/issues/660
            System.Runtime.ExceptionServices.ExceptionDispatchInfo.Capture(e).Throw()
    }

    member this.Run () =
        Async.Start (this.RunAsync ())
        this :> IDisposable
    member this.RunSynchronously () =
        Async.RunSynchronously (this.RunAsync ())

    interface IOscServer with
        override this.Run () = this.Run ()
        override this.RunSynchronously () = this.RunSynchronously ()

    member _.Dispose () =
        try
            cts.Cancel ()
        finally
            cts.Dispose ()
            disposed <- true

    interface IDisposable with
        override this.Dispose () = this.Dispose ()

[<AutoOpen>]
module Extensions =
    type IOscClient with
        member this.SendMessage msg = Async.RunSynchronously (this.SendMessageAsync msg)
