module Osc.fs.Tests
open System
open System.Collections.Concurrent
open System.IO
open System.Threading
open System.Threading.Channels
open System.Threading.Tasks
open System.Net.Sockets
open Expecto
open Expecto.Flip
open Osc.fs
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

type MockUdpClient(endPoint, channel: Channel<UdpReceiveResult>) =
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

let internal mockUdpClient channel endPoint = new MockUdpClient(endPoint, channel) :> IUdpClient

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
    testList "Osc.fs" [
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
        testList ("OscUdpServer can read from OscUdpClient") [
            testCaseAsync "One message" (async {
                use messageReceived = new ManualResetEventSlim()
                let mutable e = None
                let c = Channel.CreateBounded 1

                let message = { addressPattern = "/foo"; arguments = [OscString "Hello, world"; OscInt32 10; OscFloat32 -123.45f] }
                let server = new OscUdpServer("127.0.0.1", 1234, mockUdpClient c, (fun msg -> async {
                    try Expect.equal "message" message msg with ex -> e <- Some ex
                    messageReceived.Set ()
                }))
                use _ = server.Run ()
                
                let client = new OscUdpClient("127.0.0.1", 1234, mockUdpClient c)
                
                do! client.SendMessageAsync message

                let! result = Async.AwaitWaitHandle messageReceived.WaitHandle
                e |> Option.iter (fun e -> ExceptionDispatchInfo.Capture(e).Throw())
            })
        ]
    ]

//"/oscillator/4/frequency" |> Seq.chunkBySize 4 |> Seq.map (fun cs -> cs |> Array.map (fun c -> $"byte '{c}'") |> String.concat "; ") |> String.concat ";\n" |> printfn "%s"

[<EntryPoint>]
let main args =
    runTestsInAssembly defaultConfig args
