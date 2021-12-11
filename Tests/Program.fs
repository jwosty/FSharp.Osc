module FSharp.Osc.Tests
open System
open System.Collections.Concurrent
open System.IO
open System.Threading
open System.Threading.Channels
open System.Threading.Tasks
open System.Net.Sockets
open Expecto
open Expecto.Flip
open FSharp.Osc
open System.Net
open System.Runtime.ExceptionServices

module Expect =
    // from https://github.com/haf/expecto/blob/6b63ec66af2572232738f016ca4e4d62c4e30403/Expecto/Expecto.fs#L58
    let inline failtest msg = raise <| AssertException msg
    let inline failtestf fmt = Printf.ksprintf failtest fmt
    
    let float32Equal message expected actual =
        let inline fail () = failtestf "%s. Actual value was %f but had expected it to be %f." message actual expected
        if Single.IsNaN expected then
            if not (Single.IsNaN actual) then
                fail ()
        else
            if actual <> expected then
                fail ()

type ChannelUdpClient(endPoint, channel: Channel<UdpReceiveResult>) =
    interface IUdpClient with
        override this.Connect (host, port) = ()
        override this.Connect endPoint = ()
        override this.ReceiveAsync () = task {
            let! result = channel.Reader.ReadAsync().AsTask()
            printfn "Received packet: %O" result
            return result
        }
        override this.SendAsync (datagram, bytes) = task {
            let result = UdpReceiveResult(datagram, endPoint)
            printfn "Sending packet"
            do! channel.Writer.WriteAsync result
            printfn "Sent packet"
            return bytes
        }

    interface IDisposable with
        override this.Dispose () = ()

let internal channelUdpClient channel endPoint = new ChannelUdpClient(endPoint, channel) :> IUdpClient

let internal mockUdpClient receiveAsync sendAsync =
    let mutable endPoint = Unchecked.defaultof<IPEndPoint>
    { 
        new IUdpClient with
            override this.Connect (_,_) = ()
            override this.Connect _ = ()
            override this.ReceiveAsync () = (defaultArg receiveAsync (fun _ -> raise (NotImplementedException()))) endPoint
            override this.SendAsync (x,y) = (defaultArg sendAsync (fun (_,_) -> raise (NotImplementedException()))) (x,y)
            override this.Dispose () = ()
    },
    (fun e -> endPoint <- e)

let internal mockTcpClient stream =
    {
        new ITcpClient with
            override _.ConnectAsync (host: IPAddress, port: int) = task { () } :> Task
            override _.ConnectAsync (host: string, port: int) = task { () } :> Task
            override _.GetStream () = stream
            override _.Dispose () = ()
    }

// stream 0 = connection 0, stream 1 = connection 1, etc.
let internal mockTcpListener (connectionStreams: Stream seq) =
    let mutable started = false
    let ss = connectionStreams.GetEnumerator ()
    {
        new ITcpListener with
            override _.AcceptTcpClientAsync () = task {
                ss.MoveNext() |> ignore
                return mockTcpClient ss.Current
            }
            override _.Start () = started <- true 
            override _.Dispose () = ()
    }

let writeOscMessageToArrayAsync msg = async {
    use memStream = new MemoryStream()
    do! writeOscMessageAsync memStream msg
    return memStream.ToArray()
}

let END = 0xC0uy
let ESC = 0xDBuy
let ESC_END = 0xDCuy
let ESC_ESC = 0xDDuy

// A wrapper around ManualResetEvent that allows us to "return" a result from it, asynchronously
type AsyncWaitHandle<'t>(?millisecondsTimeout, ?ct: CancellationToken) =
    let signal = new AutoResetEvent(false)
    let mutable result = None
    let mutable disposed = false

    member _.Continue (value: 't) =
        result <- Some value
        signal.Set() |> ignore

    member _.ResultAsync () = async {
        let! ct = Async.CancellationToken
        match! Async.AwaitWaitHandle (signal, ?millisecondsTimeout = millisecondsTimeout) with
        | true -> return result.Value
        | false -> return raise (new TimeoutException())
    }

    interface IDisposable with
        override _.Dispose () = if not disposed then signal.Dispose ()

let ascii (c: char) = byte c

let (|SomeOscFloat32|) x =
    match x with
    | Some (OscFloat32 x') -> SomeOscFloat32 x'
    | _ -> raise (AssertException($"Was not Ok({nameof(OscFloat32)})"))

let makeParseTest eq fParse data (bytes: byte[]) =
    testCaseAsync "Parse" (async {
        use stream = new MemoryStream(bytes)
        let! result = fParse stream
        result |> eq "Parsed data" data
    })

let makeWriteTest eq fWrite data (bytes: byte[]) =
    testCaseAsync "Write" (async {
        use stream = new MemoryStream()
        do! fWrite stream data
        do! Async.AwaitTask (stream.FlushAsync ())
        stream.Position <- 0L
        let buffer = Array.zeroCreate bytes.Length
        let! _ = stream.AsyncRead (buffer, 0, buffer.Length)
        buffer |> Expect.sequenceEqual ("Written bytes") bytes
    })

let makeParseAndWriteTests eq fParse fWrite name data bytes =
    testList name [
        makeParseTest eq fParse data bytes
        makeWriteTest eq fWrite data bytes
    ]

let inline testCaseAsyncTimeout timeout name test =
    TestLabel(name, TestCase (Test.timeout timeout (Async test), Normal), Normal)

let defaultTimeout = 100

[<Tests>]
let tests =
    testList "Osc_fs" [
        testList (nameof(parseOscInt32Async)) (
            [   0,              [| 0x00uy; 0x00uy; 0x00uy; 0x00uy |]
                10,             [| 0x00uy; 0x00uy; 0x00uy; 0x0Auy |]
                1_000,          [| 0x00uy; 0x00uy; 0x03uy; 0xE8uy |]
                1_000_000,      [| 0x00uy; 0x0Fuy; 0x42uy; 0x40uy |]
                1_000_000_023,  [| 0x3Buy; 0x9Auy; 0xCAuy; 0x17uy |]
            ] |> List.map (fun (value, bytes) ->
                makeParseAndWriteTests Expect.equal parseOscInt32Async writeOscInt32Async (string value) value bytes)
        )
        testList (nameof(parseOscFloat32Async)) (
            [   0.f,            [| 0x00uy; 0x00uy; 0x00uy; 0x00uy |]
                1.234567936E9f, [| 0x4Euy; 0x93uy; 0x2Cuy; 0x06uy |]
                -123.45f,       [| 0xC2uy; 0xF6uy; 0xE6uy; 0x66uy |]
                nanf,           [| 0xFFuy; 0xC0uy; 0x00uy; 0x00uy |]
                infinityf,      [| 0x7Fuy; 0x80uy; 0x00uy; 0x00uy |]
            ] |> List.map (fun (value, bytes) ->
                let str = value.ToString().Replace (".", " point ")
                makeParseAndWriteTests Expect.float32Equal parseOscFloat32Async writeOscFloat32Async str value bytes)
        )
        testList (nameof(parseOscStringAsync)) (
            [
                "",             [| 0x00uy;   0x00uy;   0x00uy;   0x00uy |]
                "a",            [| byte 'a'; 0x00uy;   0x00uy;   0x00uy |]
                "AB",           [| byte 'A'; byte 'B'; 0x00uy;   0x00uy |]
                "abc",          [| byte 'a'; byte 'b'; byte 'c'; 0x00uy |]
                "XYZA",         [| byte 'X'; byte 'Y'; byte 'Z'; byte 'A'
                                   0x00uy;   0x00uy;   0x00uy;   0x00uy |]
                "XYZABC",       [| byte 'X'; byte 'Y'; byte 'Z'; byte 'A'
                                   byte 'B'; byte 'C'; 0x00uy;   0x00uy |]
                "Hello, to the world of OSC!", [|
                                   byte 'H'; byte 'e'; byte 'l'; byte 'l'
                                   byte 'o'; byte ','; byte ' '; byte 't'
                                   byte 'o'; byte ' '; byte 't'; byte 'h'
                                   byte 'e'; byte ' '; byte 'w'; byte 'o'
                                   byte 'r'; byte 'l'; byte 'd'; byte ' '
                                   byte 'o'; byte 'f'; byte ' '; byte 'O'
                                   byte 'S'; byte 'C'; byte '!'; 0x00uy |]
                "The quick brown fox jumped over the lazy dog.", [|
                                   byte 'T'; byte 'h'; byte 'e'; byte ' '
                                   byte 'q'; byte 'u'; byte 'i'; byte 'c'
                                   byte 'k'; byte ' '; byte 'b'; byte 'r'
                                   byte 'o'; byte 'w'; byte 'n'; byte ' '
                                   byte 'f'; byte 'o'; byte 'x'; byte ' '
                                   byte 'j'; byte 'u'; byte 'm'; byte 'p'
                                   byte 'e'; byte 'd'; byte ' '; byte 'o'
                                   byte 'v'; byte 'e'; byte 'r'; byte ' '
                                   byte 't'; byte 'h'; byte 'e'; byte ' '
                                   byte 'l'; byte 'a'; byte 'z'; byte 'y'
                                   byte ' '; byte 'd'; byte 'o'; byte 'g'
                                   byte '.'; 0x00uy;   0x00uy;   0x00uy |]

            ] |> List.map (fun (value, bytes) ->
                makeParseAndWriteTests Expect.equal parseOscStringAsync writeOscStringAsync value value bytes
            )
        )
        testList (nameof(parseOscBlobAsync)) (
            [
                "8 bytes",
                    [| 1uy;    2uy;    3uy;    4uy
                       100uy;  99uy;   98uy;   97uy |],
                    [| 0x00uy; 0x00uy; 0x00uy; 0x08uy
                       1uy;    2uy;    3uy;    4uy
                       100uy;  99uy;   98uy;   97uy |]
                "9 bytes",
                    [| 1uy;    2uy;    3uy;    4uy
                       100uy;  99uy;   98uy;   97uy
                       23uy;                        |],
                    [| 0x00uy; 0x00uy; 0x00uy; 0x09uy
                       1uy;    2uy;    3uy;    4uy
                       100uy;  99uy;   98uy;   97uy;
                       23uy;   0uy;   0uy;    0uy   |]
                "10 bytes",
                    [| 1uy;    2uy;    3uy;    4uy
                       100uy;  99uy;   98uy;   97uy
                       23uy;   24uy                 |],
                    [| 0x00uy; 0x00uy; 0x00uy; 0x0Auy
                       1uy;    2uy;    3uy;    4uy
                       100uy;  99uy;   98uy;   97uy;
                       23uy;   24uy;   0uy;    0uy  |]
                "11 bytes",
                    [| 1uy;    2uy;    3uy;    4uy
                       100uy;  99uy;   98uy;   97uy
                       23uy;   24uy;   25uy         |],
                    [| 0x00uy; 0x00uy; 0x00uy; 0x0Buy
                       1uy;    2uy;    3uy;    4uy
                       100uy;  99uy;   98uy;   97uy;
                       23uy;   24uy;   25uy;    0uy  |]
            ] |> List.map (fun (name, value, bytes) ->
                makeParseAndWriteTests Expect.equal parseOscBlobAsync writeOscBlobAsync name value bytes
            )
        )
        testList (nameof(parseOscTypeTagAsync)) (
            [
                "",             [| byte ','; 0x00uy;   0x00uy;   0x00uy |]
                "i",            [| byte ','; byte 'i'; 0x00uy;   0x00uy |]
                "if",           [| byte ','; byte 'i'; byte 'f'; 0x00uy |]
                "fss",          [| byte ','; byte 'f'; byte 's'; byte 's'
                                   0x00uy;   0x00uy;   0x00uy;   0x00uy |]
                "fsis",         [| byte ','; byte 'f'; byte 's'; byte 'i'
                                   byte 's'; 0x00uy;   0x00uy;   0x00uy |]
            ]
            |> List.map (fun (value, bytes) ->
                makeParseAndWriteTests Expect.equal parseOscTypeTagAsync writeOscTypeTagAsync value value bytes
            )
        )
        testList (nameof(parseOscAddressPatternAsync)) (
            [
                "/foo/bar",     [| byte '/'; byte 'f'; byte 'o'; byte 'o'
                                   byte '/'; byte 'b'; byte 'a'; byte 'r'
                                   0x00uy;   0x00uy;   0x00uy;   0x00uy |]
            ]
            |> List.map (fun (value, bytes) ->
                makeParseAndWriteTests Expect.equal parseOscAddressPatternAsync writeOscAddressPatternAsync value value bytes
            )
        )
        testList (nameof(parseOscMessageAsync)) (
            ([
                "/foo with no args",
                    { addressPattern = "/foo"; arguments = [] },
                    [| byte '/'; byte 'f'; byte 'o'; byte 'o'
                       0x00uy;   0x00uy;   0x00uy;   0x00uy
                       byte ','; 0x00uy;   0x00uy;   0x00uy |]
                "/bar with no args",
                    { addressPattern = "/bar"; arguments = [] },
                    [| byte '/'; byte 'b'; byte 'a'; byte 'r'
                       0x00uy;   0x00uy;   0x00uy;   0x00uy
                       byte ','; 0x00uy;   0x00uy;   0x00uy |]
                "/foo/bar with an int arg",
                    { addressPattern = "/foo/bar"; arguments = [OscInt32 1_000] },
                    [| byte '/'; byte 'f'; byte 'o'; byte 'o'
                       byte '/'; byte 'b'; byte 'a'; byte 'r'
                       0x00uy;   0x00uy;   0x00uy;   0x00uy
                       byte ','; byte 'i'; 0x00uy;   0x00uy
                       0x00uy;   0x00uy;   0x03uy;   0xE8uy |]  // 1000
                "/foo/bar with two int args",
                    { addressPattern = "/foo/bar"; arguments = [OscInt32 2_000_000; OscInt32 1_000] },
                    [| byte '/'; byte 'f'; byte 'o'; byte 'o'  // /foo/bar
                       byte '/'; byte 'b'; byte 'a'; byte 'r'
                       0x00uy;   0x00uy;   0x00uy;   0x00uy
                       byte ','; byte 'i'; byte 'i'; 0x00uy    // ,ii
                       0x00uy;   0x1Euy;   0x84uy;   0x80uy    // 2_000_000
                       0x00uy;   0x00uy;   0x03uy;   0xE8uy |] // 1_000
                "/foo/bar with a string and float arg",
                    { addressPattern = "/foo/bar"; arguments = [OscString "hello!"; OscFloat32 42.f] },
                    [| byte '/'; byte 'f'; byte 'o'; byte 'o'  
                       byte '/'; byte 'b'; byte 'a'; byte 'r'  // /foo/bar
                       0x00uy;   0x00uy;   0x00uy;   0x00uy
                       byte ','; byte 's'; byte 'f'; 0x00uy    // ,sf
                       byte 'h'; byte 'e'; byte 'l'; byte 'l'  // hello!
                       byte 'o'; byte '!'; 0uy;      0uy
                       0x42uy;   0x28uy;   0x00uy;   0x00uy |] // 42.0
                "/foo/bar with a blob of size 4 and float arg",
                    { addressPattern = "/foo/*"; arguments = [OscBlob [|10uy;20uy;30uy;40uy|]; OscFloat32 42.f] },
                    [| byte '/'; byte 'f'; byte 'o'; byte 'o'  // address /foo /*
                       byte '/'; byte '*'; 0x00uy;   0x00uy    
                       byte ','; byte 'b'; byte 'f'; 0x00uy    // types blob and float
                       0x00uy;   0x00uy;   0x00uy;   0x04uy    // the blob is 4 bytes large
                       10uy;     20uy;     30uy;     40uy      // blob data
                       0x42uy;   0x28uy;   0x00uy;   0x00uy    // float value 42.f
                    |]
                "/foo/bar with a blob of size 5 and float arg",
                { addressPattern = "/foo/*"; arguments = [OscBlob [|10uy;20uy;30uy;40uy;50uy|]; OscFloat32 42.f] },
                [| byte '/'; byte 'f'; byte 'o'; byte 'o'  // address /foo /*
                   byte '/'; byte '*'; 0x00uy;   0x00uy    
                   byte ','; byte 'b'; byte 'f'; 0x00uy    // types blob and float
                   0x00uy;   0x00uy;   0x00uy;   0x05uy    // the blob is 4 bytes large
                   10uy;     20uy;     30uy;     40uy      // blob data
                   50uy;     0uy;      0uy;      0uy       
                   0x42uy;   0x28uy;   0x00uy;   0x00uy    // float value 42.f
                |] 
                "/foo/bar with a blob of size 6 and float arg",
                { addressPattern = "/foo/*"; arguments = [OscBlob [|10uy;20uy;30uy;40uy;50uy;60uy|]; OscFloat32 42.f] },
                [| byte '/'; byte 'f'; byte 'o'; byte 'o'  // address /foo /*
                   byte '/'; byte '*'; 0x00uy;   0x00uy    
                   byte ','; byte 'b'; byte 'f'; 0x00uy    // types blob and float
                   0x00uy;   0x00uy;   0x00uy;   0x06uy    // the blob is 4 bytes large
                   10uy;     20uy;     30uy;     40uy      // blob data
                   50uy;     60uy;     0uy;      0uy       
                   0x42uy;   0x28uy;   0x00uy;   0x00uy    // float value 42.f
                |] 
                "/foo/bar with a blob of size 7 and float arg",
                { addressPattern = "/foo/*"; arguments = [OscBlob [|10uy;20uy;30uy;40uy;50uy;60uy;70uy|]; OscFloat32 42.f] },
                [| byte '/'; byte 'f'; byte 'o'; byte 'o'  // address /foo /*
                   byte '/'; byte '*'; 0x00uy;   0x00uy    
                   byte ','; byte 'b'; byte 'f'; 0x00uy    // types blob and float
                   0x00uy;   0x00uy;   0x00uy;   0x07uy    // the blob is 4 bytes large
                   10uy;     20uy;     30uy;     40uy      // blob data
                   50uy;     60uy;     70uy;     0uy       
                   0x42uy;   0x28uy;   0x00uy;   0x00uy    // float value 42.f
                |] 
                // examples straight from http://opensoundcontrol.org/spec-1_0-examples.html#osc-message-examples
                "OSC spec example 1",
                    { addressPattern = "/oscillator/4/frequency"; arguments = [OscFloat32 440.f] },
                    [| byte '/'; byte 'o'; byte 's'; byte 'c'
                       byte 'i'; byte 'l'; byte 'l'; byte 'a'
                       byte 't'; byte 'o'; byte 'r'; byte '/'
                       byte '4'; byte '/'; byte 'f'; byte 'r'
                       byte 'e'; byte 'q'; byte 'u'; byte 'e'
                       byte 'n'; byte 'c'; byte 'y'; 0x00uy
                       byte ','; byte 'f'; 0x00uy;   0x00uy
                       0x43uy;   0xDCuy;   0x00uy;   0x00uy
                    |]
                "OSC spec example 2",
                    { addressPattern = "/foo"; arguments = [OscInt32 1_000; OscInt32 -1; OscString "hello"; OscFloat32 1.234f; OscFloat32 5.678f] },
                    [| byte '/'; byte 'f'; byte 'o'; byte 'o'
                       0x00uy;   0x00uy;   0x00uy;   0x00uy
                       byte ','; byte 'i'; byte 'i'; byte 's'
                       byte 'f'; byte 'f'; 0x00uy;   0x00uy
                       0x00uy;   0x00uy;   0x03uy;   0xE8uy    // 1_000
                       0xFFuy;   0xFFuy;   0xFFuy;   0xFFuy    // -1
                       byte 'h'; byte 'e'; byte 'l'; byte 'l'  // hello
                       byte 'o'; 0x00uy;   0x00uy;   0x00uy
                       0x3Fuy;   0x9Duy;   0xF3uy;   0xB6uy    // 1.234
                       0x40uy;   0xB5uy;   0xB2uy;   0x2Duy |] // 5.678
                // using the new required types in OSC 1.1 (T, F, None, and Impulse): http://opensoundcontrol.org/files/2009-NIME-OSC-1.1.pdf
                "OSC 1_1 types",
                    { addressPattern = "/foo"; arguments = [OscNone; OscBool true; OscImpulse; OscInt32 0xDEADBEEF; OscBool false; OscInt32 0xCAFEBABE] },
                    [| byte '/'; byte 'f'; byte 'o'; byte 'o'
                       0x00uy;   0x00uy;   0x00uy;   0x00uy
                       byte ','; byte 'N'; byte 'T'; byte 'I'
                       byte 'i'; byte 'F'; byte 'i'; 0x00uy     // True, False, None, and Impulse all do not allocate any data in the arg list
                       0xDEuy;   0xADuy;   0xBEuy;   0xEFuy
                       0xCAuy;   0xFEuy;   0xBAuy;   0xBEuy |]
            ] |> List.map (fun (testName, value, bytes) ->
                makeParseAndWriteTests Expect.equal parseOscMessageAsync writeOscMessageAsync testName value bytes
            )) @ ([
            // skip the write test for this one, since it will write as a lowercase s (which we already have a test for)
                "/foo/bar with a float string and two int args",
                { addressPattern = "/foo/bar"; arguments = [OscFloat32 -12.75f; OscString "Data."; OscInt32 -100; OscInt32 15] },
                [|
                   byte '/'; byte 'f'; byte 'o'; byte 'o'  // /foo/bar
                   byte '/'; byte 'b'; byte 'a'; byte 'r'  
                   0x00uy;   0x00uy;   0x00uy;   0x00uy;
                   byte ','; byte 'f'; byte 'S'; byte 'i'  // ,fSii
                   byte 'i'; 0x00uy;   0x00uy;   0x00uy;
                   0xC1uy;   0x4Cuy;   0x00uy;   0x00uy    // -12.75
                   byte 'D'; byte 'a'; byte 't'; byte 'a'  // Data.
                   byte '.'; 0x00uy;   0x00uy;   0x00uy
                   0xFFuy;   0xFFuy;   0xFFuy;   0x9Cuy    // -100
                   0x00uy;   0x00uy;   0x00uy;   0x0Fuy |] // 15
            ] |> List.map (fun (testName, value, bytes) ->
                testList testName [makeParseTest Expect.equal parseOscMessageAsync value bytes]
            ))
        )
        testList (nameof(dispatchMessage)) [
            let NoCallMethod name = Method (name, (fun _ -> async { failwith $"{name} should not be called!" }))
            testCaseAsync "/foo" (async {
                let mutable fooCalled = false
                let msg = { addressPattern = "/foo"; arguments = [OscInt32 10] }
                let table = [Method ("foo", (fun m -> async { Expect.equal "/foo msg" msg m; fooCalled <- true } ))]
                do! dispatchMessage table msg
                Expect.isTrue "/foo method called" fooCalled
            })
            testCaseAsync "/foo/bar" (async {
                let mutable fooBarCalled = false
                let msg = { addressPattern = "/foo/bar"; arguments = [OscInt32 42] }
                let table = [
                    Path ("foo", [
                        NoCallMethod "baz"
                        Method ("bar", (fun m -> async { Expect.equal "/foo/bar msg" msg m; fooBarCalled <- true } ))
                        NoCallMethod "qux"
                    ])
                ]
                do! dispatchMessage table msg
                Expect.isTrue "/foo/bar method called" fooBarCalled
            })
            testCaseAsync "/foo/*" (async {
                let mutable fooBarCalled = false
                let mutable fooBazCalled = false
                let mutable fooQuxCalled = false
                let msg = { addressPattern = "/foo/*"; arguments = [OscString "I am an argument"] }
                let table = [
                    Path ("foo", [
                        Method ("baz", (fun m -> async { Expect.equal "/foo/baz msg" msg m; fooBazCalled <- true } ))
                        Method ("bar", (fun m -> async { Expect.equal "/foo/bar msg" msg m; fooBarCalled <- true } ))
                        Method ("qux", (fun m -> async { Expect.equal "/foo/qux msg" msg m; fooQuxCalled <- true } ))
                    ])
                ]
                do! dispatchMessage table msg
                Expect.isTrue "/foo/baz method called" fooBazCalled
                Expect.isTrue "/foo/bar method called" fooBarCalled
                Expect.isTrue "/foo/qux method called" fooQuxCalled
            })
            testCaseAsync "/foo/ba?/1" (async {
                let mutable fooBaz1Called = false
                let mutable fooBar1Called = false
                let msg = { addressPattern = "/foo/ba?/1"; arguments = [OscString "I am an argument"] }
                let table = [
                    Path ("foo", [
                        Path ("bar", [
                            Method ("1", (fun m -> async { Expect.equal "/foo/bar msg" msg m; fooBar1Called <- true } ))
                            NoCallMethod "2"
                        ])
                        Path ("baz", [
                            Method ("1", (fun m -> async { Expect.equal "/foo/baz msg" msg m; fooBaz1Called <- true } ))
                            NoCallMethod "11"
                            NoCallMethod "2"
                        ])
                        Path ("qux", [
                            NoCallMethod "1"
                            NoCallMethod "2"
                        ])
                    ])
                ]
                do! dispatchMessage table msg
                Expect.isTrue "/foo/bar method called" fooBar1Called
                Expect.isTrue "/foo/baz method called" fooBaz1Called
            })
            testCaseAsync "/be[ae3]r/1" (async {
                let mutable bear1Called = false
                let mutable beer1Called = false
                let mutable be3r1Called = false
                let msg = { addressPattern = "/be[ae3]r/1"; arguments = [OscString "I am an argument"] }
                let table = [
                    Path ("ber", [
                        NoCallMethod "1"
                    ])
                    Path ("bear", [
                        Method ("1", (fun m -> async { Expect.equal "/bear/1 msg" msg m; bear1Called <- true } ))
                        NoCallMethod "11"
                    ])
                    Path ("beer", [
                        NoCallMethod "beer"
                        Method ("1", (fun m -> async { Expect.equal "/beer/1 msg" msg m; beer1Called <- true } ))
                    ])
                    Path ("be3r", [
                        NoCallMethod "2"
                        Method ("1", (fun m -> async { Expect.equal "/beer/1 msg" msg m; beer1Called <- true } ))
                    ])
                    Path ("bexr", [
                        NoCallMethod "1"
                    ])
                ]
                do! dispatchMessage table msg
                Expect.isTrue "/bear/1 method called" bear1Called
                Expect.isTrue "/beer/1 method called" beer1Called
            })
            testCaseAsync "/foo/bar/[2-5]" (async {
                let calls = ConcurrentBag()
                let f name msg m = async {
                    Expect.equal $"{name} msg" msg m
                    calls.Add name
                }
                let msg = { addressPattern = "/foo/bar/[2-5]"; arguments = [OscString "I am an argument"] }
                let table = [
                    Path ("foo", [
                        Path ("bar", [
                            NoCallMethod "1"
                            Method ("2", f "/foo/bar/2" msg)
                            Method ("3", f "/foo/bar/3" msg)
                            Method ("4", f "/foo/bar/4" msg)
                            Method ("5", f "/foo/bar/5" msg)
                            NoCallMethod "11"
                            NoCallMethod "20"
                            NoCallMethod "23"
                            NoCallMethod "abc"
                            NoCallMethod "[2-5]"
                        ])
                        Path ("baz", [
                            NoCallMethod "11"
                            NoCallMethod "3"
                            NoCallMethod "zoo"
                        ])
                        Path ("qux", [
                            NoCallMethod "1"
                            NoCallMethod "thing"
                            NoCallMethod "4"
                        ])
                    ])
                ]

                do! dispatchMessage table msg
                calls |> Expect.containsAll "" ["/foo/bar/2"; "/foo/bar/3"; "/foo/bar/4"; "/foo/bar/5"]
            })
            testCaseAsync "/foo/bar/[!2-5]*" (async {
                let calls = ConcurrentBag()
                let f name msg m = async {
                    Expect.equal $"{name} msg" msg m
                    calls.Add name
                }
                let msg = { addressPattern = "/foo/bar/[!2-5]*"; arguments = [OscString "I am an argument"] }
                let table = [
                    Path ("foo", [
                        Path ("bar", [
                            Method ("1", f "/foo/bar/1" msg)
                            NoCallMethod "2"
                            NoCallMethod "3"
                            NoCallMethod "4"
                            NoCallMethod "5"
                            Method ("11", f "/foo/bar/11" msg)
                            NoCallMethod "20"
                            NoCallMethod "23"
                            Method ("abc", f "/foo/bar/abc" msg)
                        ])
                        Path ("baz", [
                            NoCallMethod "11"
                            NoCallMethod "3"
                            NoCallMethod "zoo"
                        ])
                        Path ("qux", [
                            NoCallMethod "1"
                            NoCallMethod "thing"
                            NoCallMethod "4"
                        ])
                    ])
                ]

                do! dispatchMessage table msg
                calls |> Expect.containsAll "" ["/foo/bar/1"; "/foo/bar/11"; "/foo/bar/abc"]
            })
            testCaseAsync "/foo{bar,baz}/xyz" (async {
                let calls = ConcurrentBag()
                let f name msg m = async {
                    Expect.equal $"{name} msg" msg m
                    calls.Add name
                }
                let msg = { addressPattern = "/foo{bar,baz}/xyz"; arguments = [OscFloat32 -25.f; OscFloat32 -26.f] }
                let table = [
                    Path ("foobar", [
                        NoCallMethod "abc"
                        Method ("xyz", f "/foobar/xyz" msg)
                    ])
                    Path ("foobaz", [
                        Method ("xyz", f "/foobaz/xyz" msg)
                        NoCallMethod "xyza"
                        NoCallMethod "axyz"
                        NoCallMethod "abc"
                    ])
                    Path ("fooqux", [
                        NoCallMethod "1"
                        NoCallMethod "thing"
                        NoCallMethod "4"
                    ])
                ]

                do! dispatchMessage table msg
                calls |> Expect.containsAll "" ["/foobar/xyz"; "/foobaz/xyz"]
            })
            // OSC 1.1 http://opensoundcontrol.org/files/2009-NIME-OSC-1.1.pdf
            testCaseAsync "/foo//xyz" (async {
                let calls = ConcurrentBag()
                let f name msg m = async {
                    Expect.equal $"{name} msg" msg m
                    calls.Add name
                }
                let msg = { addressPattern = "/foo//xyz"; arguments = [OscFloat32 -25.f; OscFloat32 -26.f] }
                let table = [
                    Path ("foo", [
                        Path ("bar", [
                            Path ("baz", [
                                Method ("xyz", f "/foo/bar/baz/xyz" msg)
                                NoCallMethod "blah"
                            ])
                            Path ("qux", [
                                Method ("xyz", f "/foo/bar/qux/xyz" msg)
                                NoCallMethod "hmm"
                            ])
                        ])  
                        Path ("abc", [
                            Method ("xyz", f "/foo/abc/xyz" msg)
                            NoCallMethod "nope"
                            Path ("def", [
                                Method ("xyz", f "/foo/abc/def/xyz" msg)
                            ])
                        ])
                        Method ("xyz", f "/foo/xyz" msg)
                    ])
                ]

                do! dispatchMessage table msg
                calls |> Expect.containsAll "" ["/foo/bar/baz/xyz"; "/foo/bar/qux/xyz"; "/foo/abc/xyz"; "/foo/abc/def/xyz"; "/foo/xyz"]
            })
            // making sure to escape regex chars when they're not part of the OSC spec
            testCaseAsync "/fo|bar/baz" (async {
                let mutable fobarCalled = false
                let msg = { addressPattern = "/fo|bar/baz"; arguments = [OscString "I am an argument"] }
                let table = [
                    Path ("fo|bar", [
                         Method ("baz", (fun m -> async { Expect.equal "fo|bar/baz msg" msg m; fobarCalled <- true } ))
                    ])
                    NoCallMethod "fo"
                    Path ("bar", [
                        NoCallMethod "baz"
                    ])
                ]
                do! dispatchMessage table msg
                Expect.isTrue "/fo|bar method called" fobarCalled
            })
            testCaseAsync "/fo+bar" (async {
                let mutable fobarCalled = false
                let msg = { addressPattern = "/fo+bar"; arguments = [OscString "I am an argument"] }
                let table = [
                    NoCallMethod "foobar"
                    Method ("fo+bar", (fun m -> async { Expect.equal "fo+bar msg" msg m; fobarCalled <- true } ))
                    NoCallMethod "fooobar"
                ]
                do! dispatchMessage table msg
                Expect.isTrue "/fo+bar method called" fobarCalled
            })
            testCaseAsync "/fo$o" (async {
                let mutable fooCalled = false
                let msg = { addressPattern = "/fo$o"; arguments = [OscString "I am an argument"] }
                let table = [
                    NoCallMethod "ar"
                    Method ("fo$o", (fun m -> async { Expect.equal "/fo$o msg" msg m; fooCalled <- true } ))
                    NoCallMethod "fo"
                ]
                do! dispatchMessage table msg
                Expect.isTrue "/fo$ar method called" fooCalled
            })
            testCaseAsync "/fo^o" (async {
                let mutable fooCalled = false
                let msg = { addressPattern = "/fo^o"; arguments = [OscString "I am an argument"] }
                let table = [
                    NoCallMethod "ar"
                    Method ("fo^o", (fun m -> async { Expect.equal "/fo^o msg" msg m; fooCalled <- true } ))
                    NoCallMethod "fo"
                ]
                do! dispatchMessage table msg
                Expect.isTrue "/fo^ar method called" fooCalled
            })
            testCaseAsync "/foo dot bar" (async {
                let mutable foobarCalled = false
                let msg = { addressPattern = "/foo.bar"; arguments = [OscString "I am an argument"] }
                let table = [
                    NoCallMethod "fooxbar"
                    Method ("foo.bar", (fun m -> async { Expect.equal "/foo.bar msg" msg m; foobarCalled <- true } ))
                    NoCallMethod "foobar"
                ]
                do! dispatchMessage table msg
                Expect.isTrue "/foo.bar method called" foobarCalled
            })
            testCaseAsync "/foo\\dbar" (async {
                let mutable foobarCalled = false
                let msg = { addressPattern = "/foo\\dbar"; arguments = [OscString "I am an argument"] }
                let table = [
                    NoCallMethod "foo4bar"
                    Method ("foo\\dbar", (fun m -> async { Expect.equal "/foo\\dbar msg" msg m; foobarCalled <- true } ))
                    NoCallMethod "foobar"
                ]
                do! dispatchMessage table msg
                Expect.isTrue "/foo\\dbar method called" foobarCalled
            })
            testCaseAsync "/foo(bar)" (async {
                let mutable foobarCalled = false
                let msg = { addressPattern = "/foo(bar)"; arguments = [OscString "I am an argument"] }
                let table = [
                    Method ("foo(bar)", (fun m -> async { Expect.equal "/foo(bar) msg" msg m; foobarCalled <- true } ))
                    NoCallMethod "foobar"
                ]
                do! dispatchMessage table msg
                Expect.isTrue "/foo(bar) method called" foobarCalled
            })
        ]
        testList (nameof(OscUdpClient)) [
            testCaseAsyncTimeout defaultTimeout "One message" (async {
                let messageActual = { addressPattern = "/blah"; arguments = [OscString "foo"; OscFloat32 10.11f] }
                let! msgBytes = writeOscMessageToArrayAsync messageActual
                let bytesWritten = Channel.CreateUnbounded()

                let udpClient, setEndPoint =
                    mockUdpClient
                        (Some (fun endPoint -> raise (NotImplementedException())))
                        (Some (fun (data, bytes) -> bytesWritten.Writer.WriteAsync((data,bytes)).AsTask().ContinueWith(fun _ -> bytes)))
                use client = new OscUdpClient("127.0.0.1", 1234, (fun endPoint -> setEndPoint endPoint; udpClient))
                
                do! client.SendMessageAsync messageActual

                let! (data, count) = Async.AwaitTask (bytesWritten.Reader.ReadAsync().AsTask())
                data |> Expect.sequenceEqual (nameof(data)) msgBytes
            })
            testCaseAsyncTimeout defaultTimeout "Three messages" (async {
                let message1Actual = { addressPattern = "/foo"; arguments = [OscString "Hello, world"; OscInt32 10; OscFloat32 -123.45f] }
                let message2Actual = { addressPattern = "/bar/*"; arguments = [] }
                let message3Actual = { addressPattern = "/{foo,bar}"; arguments = [OscInt32 42] }
                let! msg1Bytes = writeOscMessageToArrayAsync message1Actual
                let! msg2Bytes = writeOscMessageToArrayAsync message2Actual
                let! msg3Bytes = writeOscMessageToArrayAsync message3Actual
                let bytesWritten = Channel.CreateUnbounded()

                let udpClient, setEndPoint =
                    mockUdpClient
                        (Some (fun endPoint -> raise (NotImplementedException())))
                        (Some (fun (data, bytes) -> bytesWritten.Writer.WriteAsync((data,bytes)).AsTask().ContinueWith(fun _ -> bytes)))
                use client = new OscUdpClient("127.0.0.1", 1234, (fun endPoint -> setEndPoint endPoint; udpClient))
                
                do! client.SendMessageAsync message1Actual
                do! client.SendMessageAsync message2Actual
                do! client.SendMessageAsync message3Actual

                let! (data1, _) = Async.AwaitTask (bytesWritten.Reader.ReadAsync().AsTask())
                data1 |> Expect.sequenceEqual (nameof(data1)) msg1Bytes
                
                let! (data2, _) = Async.AwaitTask (bytesWritten.Reader.ReadAsync().AsTask())
                data2 |> Expect.sequenceEqual (nameof(data2)) msg2Bytes

                let! (data3, _) = Async.AwaitTask (bytesWritten.Reader.ReadAsync().AsTask())
                data3 |> Expect.sequenceEqual (nameof(data1)) msg3Bytes
            })
        ]
        testList (nameof(OscUdpServer)) [
            testCaseAsyncTimeout defaultTimeout "One message" (async {
                let messageActual = { addressPattern = "/foo"; arguments = [OscString "Hello, world"; OscInt32 10; OscFloat32 -123.45f] }
                use msgReceived = new AsyncWaitHandle<_>()

                let! msgBytes = async {
                    use memStream = new MemoryStream()
                    do! writeOscMessageAsync memStream messageActual
                    return memStream.ToArray()
                }

                let makeUdpClient endPoint =
                    let udpClient, setEndPoint =
                        mockUdpClient
                            (Some (fun endPoint ->
                                Task.FromResult (UdpReceiveResult(msgBytes, endPoint))))
                            (Some (fun (data, bytes) -> raise (NotImplementedException())))
                    setEndPoint endPoint
                    udpClient
                let server =
                    new OscUdpServer(
                        "127.0.0.1", 1234,
                        makeUdpClient,
                        (fun m -> async { msgReceived.Continue m }))

                use _ = server.Run ()

                let! msg = msgReceived.ResultAsync ()
                msg |> Expect.equal (nameof(msg)) messageActual
            })
            testCaseAsync "Three messages" (async {
                let message1Actual = { addressPattern = "/foo"; arguments = [OscString "Hello, world"; OscInt32 10; OscFloat32 -123.45f] }
                let message2Actual = { addressPattern = "/bar/*"; arguments = [] }
                let message3Actual = { addressPattern = "/{foo,bar}"; arguments = [OscInt32 42] }
                let msgChannel = Channel.CreateUnbounded()

                let! msgBytes = [message1Actual; message2Actual; message3Actual] |> Seq.map writeOscMessageToArrayAsync |> Async.Sequential
                let msgBytesEnumerator = (msgBytes :> _ seq).GetEnumerator()

                let xs = ConcurrentStack<_>()

                let makeUdpClient endPoint =
                    let udpClient, setEndPoint =
                        mockUdpClient
                            (Some (fun endPoint ->
                                task {
                                    msgBytesEnumerator.MoveNext () |> ignore
                                    let x = msgBytesEnumerator.Current
                                    xs.Push x
                                    return UdpReceiveResult(x, endPoint)
                                }
                            ))
                            (Some (fun (data, bytes) -> raise (NotImplementedException())))
                    setEndPoint endPoint
                    udpClient
                let server =
                    new OscUdpServer(
                        "127.0.0.1", 1234,
                        makeUdpClient,
                        (fun m -> Async.AwaitTask ((msgChannel.Writer.WriteAsync m).AsTask())))

                use _ = server.Run ()

                let read () = Async.AwaitTask (msgChannel.Reader.ReadAsync().AsTask())
                let! msg1 = read ()
                let! msg2 = read ()
                let! msg3 = read ()

                msg1 |> Expect.equal (nameof(msg1)) message1Actual
                msg2 |> Expect.equal (nameof(msg2)) message2Actual
                msg3 |> Expect.equal (nameof(msg3)) message3Actual
            })
        ]
        testList (nameof(OscTcpClient)) [
            testCaseAsyncTimeout defaultTimeout "One message with OSC 1_0 style size framing" (async {
                let messageExpected = { addressPattern = "/blah"; arguments = [OscString "foo"; OscFloat32 10.11f] }
                let! msgBytesExpected = writeOscMessageToArrayAsync messageExpected
                
                use tcpStream = new MemoryStream()
                use client = new OscTcpClient(mockTcpClient tcpStream, Osc1_0)
                
                do! client.SendMessageAsync messageExpected

                tcpStream.Position <- 0L
                let! lb = tcpStream.AsyncRead 4
                // the client is supposed to first write a big-endian int32 indicating the length of the OSC packet
                let lenActual = ((int lb.[0] <<< 16) + (int lb.[1] <<< 16) + (int lb.[2] <<< 8) + int lb.[3])
                lenActual |> Expect.equal "Frame size" msgBytesExpected.Length
                let! bytesActual = tcpStream.AsyncRead msgBytesExpected.Length
                bytesActual |> Expect.sequenceEqual (nameof(bytesActual)) msgBytesExpected
            })
            testCaseAsyncTimeout defaultTimeout "Three messages with OSC 1_0 style size framing" (async {
                let message1Expected = { addressPattern = "/foo"; arguments = [OscString "foo"; OscFloat32 10.11f] }
                let message2Expected = { addressPattern = "/x"; arguments = [] }
                let message3Expected = { addressPattern = "/somewhat/longer/message/path/than/usual/*"; arguments = [OscString "The quick brown fox jumped over the lazy dog."; OscString "This is another string!"; OscFloat32 123.4f; OscInt32 10] }
                let! msg1BytesExpected = writeOscMessageToArrayAsync message1Expected
                let! msg2BytesExpected = writeOscMessageToArrayAsync message2Expected
                let! msg3BytesExpected = writeOscMessageToArrayAsync message3Expected
                
                use tcpStream = new MemoryStream()
                use client = new OscTcpClient(mockTcpClient tcpStream, Osc1_0)
                
                do! client.SendMessageAsync message1Expected
                do! client.SendMessageAsync message2Expected
                do! client.SendMessageAsync message3Expected

                let check name (bytesExpected: byte[]) = async {
                    let! lb = tcpStream.AsyncRead 4
                    // the client is supposed to first write a big-endian int32 indicating the length of the OSC packet
                    let lenActual = ((int lb.[0] <<< 16) + (int lb.[1] <<< 16) + (int lb.[2] <<< 8) + int lb.[3])
                    lenActual |> Expect.equal $"{name} frame size" bytesExpected.Length
                    let! bytesActual = tcpStream.AsyncRead lenActual
                    bytesActual |> Expect.sequenceEqual (nameof(bytesActual)) bytesExpected
                }

                tcpStream.Position <- 0L
                do! check (nameof(msg1BytesExpected)) msg1BytesExpected
                do! check (nameof(msg2BytesExpected)) msg2BytesExpected
                do! check (nameof(msg3BytesExpected)) msg3BytesExpected
            })
            testCaseAsyncTimeout defaultTimeout "One message with OSC 1_1 style SLIP framing" (async {
                let messageExpected = { addressPattern = "/blah"; arguments = [OscString "foo"; OscFloat32 10.11f] }
                let! msgBytesExpected = writeOscMessageToArrayAsync messageExpected
                
                use tcpStream = new MemoryStream()
                use client = new OscTcpClient(mockTcpClient tcpStream, Osc1_1)
                
                do! client.SendMessageAsync messageExpected

                tcpStream.Position <- 0L
                let! bytesActual = tcpStream.AsyncRead (msgBytesExpected.Length + 2)
                bytesActual.[0] |> Expect.equal "Should begin with END byte" 0xC0uy
                bytesActual.[1 .. (bytesActual.Length - 2)] |> Expect.sequenceEqual (nameof(bytesActual)) msgBytesExpected
                bytesActual.[bytesActual.Length - 1] |> Expect.equal "Should end with END byte" 0xC0uy
            })
            testCaseAsyncTimeout defaultTimeout "Three messages with OSC 1_1 style SLIP framing" (async {
                let message1Expected = { addressPattern = "/x"; arguments = [] }
                let message2Expected = { addressPattern = "/blah"; arguments = [OscString "foo"; OscFloat32 10.11f] }
                let message3Expected = { addressPattern = "/blah"; arguments = [OscString "Open Sound Control (OSC) is an open, transport-independent, message-based protocol developed for communication among computers, sound synthesizers, and other multimedia devices."; OscFloat32 -1111111.125f] }
                let! msg1BytesExpected = writeOscMessageToArrayAsync message1Expected
                let! msg2BytesExpected = writeOscMessageToArrayAsync message2Expected
                let! msg3BytesExpected = writeOscMessageToArrayAsync message3Expected
                
                use tcpStream = new MemoryStream()
                use client = new OscTcpClient(mockTcpClient tcpStream, Osc1_1)
                
                do! client.SendMessageAsync message1Expected
                do! client.SendMessageAsync message2Expected
                do! client.SendMessageAsync message3Expected

                tcpStream.Position <- 0L

                let check name (msgBytesExpected: byte[]) = async {
                    let! bytesActual = tcpStream.AsyncRead (msgBytesExpected.Length + 2)
                    bytesActual.[0] |> Expect.equal "Should begin with END byte" 0xC0uy
                    bytesActual.[1 .. (bytesActual.Length - 2)] |> Expect.sequenceEqual name msgBytesExpected
                    bytesActual.[bytesActual.Length - 1] |> Expect.equal "Should end with END byte" 0xC0uy
                }

                do! check (nameof(msg1BytesExpected)) msg1BytesExpected
                do! check (nameof(msg2BytesExpected)) msg2BytesExpected
                do! check (nameof(msg3BytesExpected)) msg3BytesExpected
            })
            testCaseAsyncTimeout defaultTimeout "One message with OSC 1_1 style SLIP framing and an END byte in the payload" (async {
                // 0xC0 = SLIP END bytes
                let messageExpected = { addressPattern = "/blah"; arguments = [OscString "string1"; OscInt32 (int END); OscString "string2"] }
                let msgBytesExpected =
                  [|0x2Fuy; 0x62uy; 0x6Cuy; 0x61uy
                    0x68uy; 0x00uy; 0x00uy; 0x00uy
                    0x2Cuy; 0x73uy; 0x69uy; 0x73uy
                    0x00uy; 0x00uy; 0x00uy; 0x00uy
                    0x73uy; 0x74uy; 0x72uy; 0x69uy
                    0x6Euy; 0x67uy; 0x31uy; 0x00uy
                                         // END gets escaped to ESC_END, END
                    0x00uy; 0x00uy; 0x00uy; ESC; ESC_END
                    0x73uy; 0x74uy; 0x72uy; 0x69uy
                    0x6Euy; 0x67uy; 0x32uy; 0x00uy|]

                use tcpStream = new MemoryStream()
                use client = new OscTcpClient(mockTcpClient tcpStream, Osc1_1)
                
                do! client.SendMessageAsync messageExpected

                tcpStream.Position <- 0L
                let! bytesActual = tcpStream.AsyncRead (msgBytesExpected.Length + 2)
                bytesActual.[0] |> Expect.equal "Should begin with END byte" 0xC0uy
                bytesActual.[1 .. (bytesActual.Length - 2)] |> Expect.sequenceEqual (nameof(bytesActual)) msgBytesExpected
                bytesActual.[bytesActual.Length - 1] |> Expect.equal "Should end with END byte" 0xC0uy
            })
            testCaseAsyncTimeout defaultTimeout "One message with OSC 1_1 style SLIP framing and an ESC byte in the payload" (async {
                // 0xC0 = SLIP END bytes
                let messageExpected = { addressPattern = "/blah"; arguments = [OscString "string1"; OscInt32 (int ESC); OscString "string2"] }
                //writeOscMessageToArrayAsync messageExpected |> Async.RunSynchronously |> Seq.map (fun b -> $"0x%02X{b}uy") |> Seq.chunkBySize 4 |> Seq.map (String.concat "; ") |> String.concat "\n"
                let msgBytesExpected =
                  [|0x2Fuy; 0x62uy; 0x6Cuy; 0x61uy
                    0x68uy; 0x00uy; 0x00uy; 0x00uy
                    0x2Cuy; 0x73uy; 0x69uy; 0x73uy
                    0x00uy; 0x00uy; 0x00uy; 0x00uy
                    0x73uy; 0x74uy; 0x72uy; 0x69uy
                    0x6Euy; 0x67uy; 0x31uy; 0x00uy
                                         // END gets escaped to ESC_END, END_ESC
                    0x00uy; 0x00uy; 0x00uy; ESC; ESC_ESC
                    0x73uy; 0x74uy; 0x72uy; 0x69uy
                    0x6Euy; 0x67uy; 0x32uy; 0x00uy|]

                use tcpStream = new MemoryStream()
                use client = new OscTcpClient(mockTcpClient tcpStream, Osc1_1)
                
                do! client.SendMessageAsync messageExpected

                tcpStream.Position <- 0L
                let! bytesActual = tcpStream.AsyncRead (msgBytesExpected.Length + 2)
                bytesActual.[0] |> Expect.equal "Should begin with END byte" 0xC0uy
                bytesActual.[1 .. (bytesActual.Length - 2)] |> Expect.sequenceEqual (nameof(bytesActual)) msgBytesExpected
                bytesActual.[bytesActual.Length - 1] |> Expect.equal "Should end with END byte" 0xC0uy
            })
            testCaseAsyncTimeout defaultTimeout "One message with OSC 1_1 style SLIP framing and various ESC and END bytes in the payload" (async {
                let messageExpected = {
                    addressPattern = "/blah"
                    arguments = [
                        OscInt32 (int ESC)
                        OscString "string1"
                        OscInt32 ((int ESC <<< 8) + int ESC)
                        OscString "string2"
                        OscInt32 0xDEADBEEF
                        OscInt32 ((int END <<< 24) + (int END <<< 8) + int ESC)
                    ]
                }
                //writeOscMessageToArrayAsync messageExpected |> Async.RunSynchronously |> Seq.map (fun b -> $"0x%02X{b}uy") |> Seq.chunkBySize 4 |> Seq.map (String.concat "; ") |> String.concat "\n"
                // in hindsight, now that I've already written this, I could have just injected the writeOscMessageAsync and not had to couple these tests to
                // the actual message parsing/writing, making them not really unit tests... Oh well. I don't feel like rewriting these tests.
                let msgBytesExpected =
                  [|0x2Fuy; 0x62uy; 0x6Cuy; 0x61uy
                    0x68uy; 0x00uy; 0x00uy; 0x00uy
                    0x2Cuy; 0x69uy; 0x73uy; 0x69uy
                    0x73uy; 0x69uy; 0x69uy; 0x00uy
                    0x00uy; 0x00uy; 0x00uy; ESC; ESC_ESC
                    0x73uy; 0x74uy; 0x72uy; 0x69uy
                    0x6Euy; 0x67uy; 0x31uy; 0x00uy
                    0x00uy; 0x00uy; ESC; ESC_ESC;
                                            ESC; ESC_ESC
                    0x73uy; 0x74uy; 0x72uy; 0x69uy
                    0x6Euy; 0x67uy; 0x32uy; 0x00uy
                    0xDEuy; 0xADuy; 0xBEuy; 0xEFuy
                    ESC; ESC_END;
                            0x00uy; ESC; ESC_END;
                                    ESC; ESC_ESC|]

                use tcpStream = new MemoryStream()
                use client = new OscTcpClient(mockTcpClient tcpStream, Osc1_1)
                
                do! client.SendMessageAsync messageExpected

                tcpStream.Position <- 0L
                let! bytesActual = tcpStream.AsyncRead (msgBytesExpected.Length + 2)
                bytesActual.[0] |> Expect.equal "Should begin with END byte" 0xC0uy
                bytesActual.[1 .. (bytesActual.Length - 2)] |> Expect.sequenceEqual (nameof(bytesActual)) msgBytesExpected
                bytesActual.[bytesActual.Length - 1] |> Expect.equal "Should end with END byte" 0xC0uy
            })
        ]
        testList (nameof(OscTcpServer)) [
            testCaseAsyncTimeout defaultTimeout "One message with OSC 1_1 style SLIP framing and various ESC and END bytes in the payload" (async {
                let messageExpected = {
                    addressPattern = "/blah"
                    arguments = [
                        OscInt32 (int ESC)
                        OscString "string1"
                        OscInt32 ((int ESC <<< 8) + int ESC)
                        OscString "string2"
                        OscInt32 0xDEADBEEF
                        OscInt32 ((int END <<< 24) + (int END <<< 8) + int ESC)
                    ]
                }
                //writeOscMessageToArrayAsync messageExpected |> Async.RunSynchronously |> Seq.map (fun b -> $"0x%02X{b}uy") |> Seq.chunkBySize 4 |> Seq.map (String.concat "; ") |> String.concat "\n"
                let msgBytesExpected =
                  [|0x2Fuy; 0x62uy; 0x6Cuy; 0x61uy
                    0x68uy; 0x00uy; 0x00uy; 0x00uy
                    0x2Cuy; 0x69uy; 0x73uy; 0x69uy
                    0x73uy; 0x69uy; 0x69uy; 0x00uy
                    0x00uy; 0x00uy; 0x00uy; ESC; ESC_ESC
                    0x73uy; 0x74uy; 0x72uy; 0x69uy
                    0x6Euy; 0x67uy; 0x31uy; 0x00uy
                    0x00uy; 0x00uy; ESC; ESC_ESC;
                                            ESC; ESC_ESC
                    0x73uy; 0x74uy; 0x72uy; 0x69uy
                    0x6Euy; 0x67uy; 0x32uy; 0x00uy
                    0xDEuy; 0xADuy; 0xBEuy; 0xEFuy
                    ESC; ESC_END;
                            0x00uy; ESC; ESC_END;
                                    ESC; ESC_ESC|]

                use tcpStream = new MemoryStream()
                use client = new OscTcpClient(mockTcpClient tcpStream, Osc1_1)
                
                do! client.SendMessageAsync messageExpected

                tcpStream.Position <- 0L
                let! bytesActual = tcpStream.AsyncRead (msgBytesExpected.Length + 2)
                bytesActual.[0] |> Expect.equal "Should begin with END byte" 0xC0uy
                bytesActual.[1 .. (bytesActual.Length - 2)] |> Expect.sequenceEqual (nameof(bytesActual)) msgBytesExpected
                bytesActual.[bytesActual.Length - 1] |> Expect.equal "Should end with END byte" 0xC0uy
            })
        ]
        // these are more of integration tests
        testList ("OscUdpServer can read from OscUdpClient") [
            testCaseAsyncTimeout defaultTimeout "One connection with one message with OSC1_0 style size framing" (async {
                let messageExpected = { addressPattern = "/foo"; arguments = [OscString "Hello, world"; OscInt32 10; OscFloat32 -123.45f] }
                use msgReceived = new AsyncWaitHandle<_>()
                
                let msgAsMemStream msg =
                    let s = new MemoryStream()
                    Async.RunSynchronously (writeOscMessageAsync s msg)
                    s.Position <- 0L
                    s
                
                let tcpClient =
                    mockTcpListener [
                        // connection 0
                        msgAsMemStream messageExpected
                    ]
                let server =
                    new OscTcpServer(
                        tcpClient,
                        (fun m -> async { msgReceived.Continue m }))

                use _ = server.Run ()

                let! msg = msgReceived.ResultAsync ()
                msg |> Expect.equal (nameof(msg)) messageExpected
            })
            testCaseAsyncTimeout defaultTimeout "One connection with three messages with OSC1_0 style size framing" (async {
                let message1Expected = { addressPattern = "/foo"; arguments = [OscInt32 10; OscInt32 20] }
                let message2Expected = {
                    addressPattern = "/hmm/hmm/hmmmmmmmmmmmmmmmmmmmmmmmmmm"
                    arguments = [   OscString "This is a message that is longer than most, but isn't so long that it has a big ego yet";
                                    OscInt32 10; OscFloat32 -123.45f] }
                let message3Expected = { addressPattern = "/bar"; arguments = [OscString "Hello, world"; OscInt32 10; OscFloat32 -123.45f] }
                let msgChannel = Channel.CreateUnbounded()

                let msgsAsStream msgs =
                    let s = new MemoryStream()
                    Async.RunSynchronously (async {
                        for msg in msgs do
                            do! writeOscMessageAsync s msg
                    })
                    s.Position <- 0L
                    s
                
                let tcpClient =
                    mockTcpListener [
                        // connection 0
                        msgsAsStream [message1Expected; message2Expected; message3Expected]
                    ]
                let server =
                    new OscTcpServer(
                        tcpClient,
                        (fun m -> Async.AwaitTask (msgChannel.Writer.WriteAsync(m).AsTask()) ))

                use _ = server.Run ()

                let check name msg = async {
                    let! m = Async.AwaitTask (msgChannel.Reader.ReadAsync().AsTask())
                    m |> Expect.equal name msg
                }

                do! check (nameof(message1Expected)) message1Expected
                do! check (nameof(message2Expected)) message2Expected
                do! check (nameof(message3Expected)) message3Expected
            })
            testCaseAsyncTimeout defaultTimeout "Three connections with two messages each with OSC1_0 style size framing" (async {
                let c1MessagesExpected =
                    [{ addressPattern = "/foo"; arguments = [OscInt32 10; OscInt32 20] }
                     { addressPattern = "/yay"; arguments = [OscString "connection 1 message 2"] }]
                let c2MessagesExpected =
                    [{  addressPattern = "/hmm/hmm/hmmmmmmmmmmmmmmmmmmmmmmmmmm"
                        arguments = [   OscString "This is a message that is longer than most, but isn't so long that it has a big ego yet";
                                        OscInt32 10; OscFloat32 -123.45f] }
                     { addressPattern = "/woo/hoo"; arguments = [OscString "connection 3 message 2"] }]
                let c3MessagesExpected =
                    [{ addressPattern = "/yep"; arguments = [] }
                     { addressPattern = "/bar"; arguments = [OscString "howdy"; OscInt32 10; OscFloat32 -123.45f] }
                    ]
                let msgChannel = Channel.CreateUnbounded()

                let msgsAsStream msgs =
                    let s = new MemoryStream()
                    Async.RunSynchronously (async {
                        for msg in msgs do
                            do! writeOscMessageAsync s msg
                    })
                    s.Position <- 0L
                    s
                
                let tcpClient =
                    mockTcpListener [
                        // connection 1
                        msgsAsStream c1MessagesExpected
                        // connection 2
                        msgsAsStream c2MessagesExpected
                        // connection 3
                        msgsAsStream c3MessagesExpected
                    ]
                let server =
                    new OscTcpServer(
                        tcpClient,
                        (fun m -> Async.AwaitTask (msgChannel.Writer.WriteAsync(m).AsTask()) ))

                use _ = server.Run ()

                // not sure how to enforce that messages are received in the right order or anything... Eh, just chuck all the
                // results into a list and sort 'em, why not

                // (why not? we're not enforcing via test that we can handle multiple clients at once. But I think this is close enough.)
                let msgsExpectedSorted = [yield! c1MessagesExpected; yield! c2MessagesExpected; yield! c3MessagesExpected] |> List.sort

                let! msgsActual =
                    Seq.init 6 (fun _ ->
                        Async.AwaitTask (msgChannel.Reader.ReadAsync().AsTask())
                    )
                    |> Async.Sequential
                
                let msgsSorted = msgsActual |> Seq.toArray |> Array.sort
                msgsSorted |> Expect.sequenceEqual "All messages sorted" msgsExpectedSorted
            })
        ]
    ]

//"/oscillator/4/frequency" |> Seq.chunkBySize 4 |> Seq.map (fun cs -> cs |> Array.map (fun c -> $"byte '{c}'") |> String.concat "; ") |> String.concat ";\n" |> printfn "%s"

[<EntryPoint>]
let main args =
    runTestsInAssembly defaultConfig args
