module Osc.fs.Tests
open System
open System.IO
open Expecto
open Expecto.Flip
open Osc.fs

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

let ascii (c: char) = byte c

let (|SomeOscFloat32|) x =
    match x with
    | Some (OscFloat32 x') -> SomeOscFloat32 x'
    | _ -> raise (AssertException($"Was not Ok({nameof(OscFloat32)})"))

let makeParseAndWriteTests eq fParse fWrite name data (bytes: byte[]) =
    testList name [
        testCaseAsync "Parse" (async {
            use stream = new MemoryStream(bytes)
            let! result = fParse stream
            result |> eq "Parsed data" data
        })
        testCaseAsync "Write" (async {
            use stream = new MemoryStream()
            do! fWrite stream data
            do! Async.AwaitTask (stream.FlushAsync ())
            stream.Position <- 0L
            let buffer = Array.zeroCreate bytes.Length
            let! _ = stream.AsyncRead (buffer, 0, buffer.Length)
            buffer |> Expect.sequenceEqual ("Written bytes") bytes
        })
    ]

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
        testList (nameof(parseOscMessageAsync)) [
            testCaseAsync "/foo with no args" (async {
                use stream = new MemoryStream([|
                    byte '/'; byte 'f'; byte 'o'; byte 'o'
                    0uy;      0uy;      0uy;      0uy
                    byte ','; 0uy;      0uy;      0uy
                |])

                let! result = parseOscMessageAsync stream
                result |> Expect.equal (nameof(result)) ({ addressPattern = "/foo"; arguments = [] })
            })
            testCaseAsync "/bar with no args" (async {
                use stream = new MemoryStream([|
                    byte '/'; byte 'b'; byte 'a'; byte 'r'
                    0uy;      0uy;      0uy;      0uy
                    byte ','; 0uy;      0uy;      0uy
                |])

                let! result = parseOscMessageAsync stream
                result |> Expect.equal (nameof(result)) ({ addressPattern = "/bar"; arguments = [] })
            })
            testCaseAsync "/foo/bar with an int arg" (async {
                use stream = new MemoryStream([|
                    byte '/'; byte 'f'; byte 'o'; byte 'o'
                    byte '/'; byte 'b'; byte 'a'; byte 'r'
                    0uy;      0uy;      0uy;      0uy
                    byte ','; byte 'i'; 0uy;      0uy
                    0x00uy;   0x00uy;   0x03uy;   0xE8uy    // 1000
                |])

                let! result = parseOscMessageAsync stream
                result |> Expect.equal (nameof(result)) ({ addressPattern = "/foo/bar"; arguments = [OscInt32 1000] })
            })
            testCaseAsync "/foo/bar with two int args" (async {
                use stream = new MemoryStream([|
                    byte '/'; byte 'f'; byte 'o'; byte 'o'  
                    byte '/'; byte 'b'; byte 'a'; byte 'r'  // /foo/bar
                    0uy;      0uy;      0uy;      0uy
                    byte ','; byte 'i'; byte 'i'; 0uy       // ,ii
                    0x00uy;   0x1Euy;   0x84uy;   0x80uy    // 2_000_000
                    0x00uy;   0x00uy;   0x03uy;   0xE8uy    // 1000
                |])

                let! result = parseOscMessageAsync stream
                result |> Expect.equal (nameof(result)) ({ addressPattern = "/foo/bar"; arguments = [OscInt32 2_000_000; OscInt32 1000] })
            })
            testCaseAsync "/foo/bar with a string and float arg" (async {
                use stream = new MemoryStream([|
                    byte '/'; byte 'f'; byte 'o'; byte 'o'  
                    byte '/'; byte 'b'; byte 'a'; byte 'r'  // /foo/bar
                    0uy;      0uy;      0uy;      0uy
                    byte ','; byte 's'; byte 'f'; 0uy       // ,sf
                    byte 'h'; byte 'e'; byte 'l'; byte 'l'  // hello!
                    byte 'o'; byte '!'; 0uy;      0uy
                    0x42uy;   0x28uy;   0x00uy;   0x00uy    // 42.0
                |])

                let! result = parseOscMessageAsync stream
                result |> Expect.equal (nameof(result)) ({ addressPattern = "/foo/bar"; arguments = [OscString "hello!"; OscFloat32 42.f] })
            })
            testCaseAsync "/foo/bar with a float string and two int args" (async {
                use stream = new MemoryStream([|
                    byte '/'; byte 'f'; byte 'o'; byte 'o'  // /foo/bar
                    byte '/'; byte 'b'; byte 'a'; byte 'r'  
                    0uy;      0uy;      0uy;      0uy
                    byte ','; byte 'f'; byte 'S'; byte 'i'  // ,fSii
                    byte 'i'; 0uy;      0uy;      0uy
                    0xC1uy;   0x4Cuy;   0x00uy;   0x00uy    // -12.75
                    byte 'D'; byte 'a'; byte 't'; byte 'a'  // Data.
                    byte '.'; 0uy;      0uy;      0uy
                    0xFFuy;   0xFFuy;   0xFFuy;   0x9Cuy    // -100
                    0x00uy;   0x00uy;   0x00uy;   0x0Fuy    // 15
                |])

                let! result = parseOscMessageAsync stream
                result |> Expect.equal (nameof(result)) ({ addressPattern = "/foo/bar"; arguments = [OscFloat32 -12.75f; OscString "Data."; OscInt32 -100; OscInt32 15] })
            })
        ]
    ]

[<EntryPoint>]
let main args =
    runTestsInAssembly defaultConfig args
