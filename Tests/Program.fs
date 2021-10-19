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

[<Tests>]
let tests =
    testList ("Osc.fs") [
        testList (nameof(parseOscInt32Async)) [
            testCaseAsync "0" (async {
                use stream = new MemoryStream([|
                    0uy; 0uy; 0uy; 0uy
                |])
                
                let! result = parseOscInt32Async stream
                result |> Expect.equal "result" 0
            })
            testCaseAsync "10" (async {
                use stream = new MemoryStream([|
                    0x0uy; 0x0uy; 0x0uy; 0xAuy
                |])
                
                let! result = parseOscInt32Async stream
                result |> Expect.equal "result" 10
            })
            testCaseAsync "1000" (async {
                use stream = new MemoryStream([|
                    0x00uy; 0x00uy; 0x03uy; 0xE8uy
                |])
                    
                let! result = parseOscInt32Async stream
                result |> Expect.equal "result" 1000
            })
            testCaseAsync "1_000_000" (async {
                use stream = new MemoryStream([|
                    0x00uy; 0x0Fuy; 0x42uy; 0x40uy
                |])
                
                let! result = parseOscInt32Async stream
                result |> Expect.equal "result" 1_000_000
            })
            testCaseAsync "1_000_000_023" (async {
                use stream = new MemoryStream([|
                    0x3Buy; 0x9Auy; 0xCAuy; 0x17uy 
                |])
                
                let! result = parseOscInt32Async stream
                result |> Expect.equal "result" 1_000_000_023
            })
            testCaseAsync "-1_000_000_023" (async {
                use stream = new MemoryStream([|
                    0xC4uy; 0x65uy; 0x35uy; 0xE9uy 
                |])
                
                let! result = parseOscInt32Async stream
                result |> Expect.equal "result" -1_000_000_023
            })
        ]
        testList (nameof(parseOscFloat32Async)) [
            testCaseAsync "0" (async {
                use stream = new MemoryStream([|
                    0uy; 0uy; 0uy; 0uy
                |])
                
                let! result = parseOscFloat32Async stream
                result |> Expect.equal "result" 0.f
            })
            testCaseAsync "1 point 234567936E9" (async {
                use stream = new MemoryStream([|
                    0x4Euy; 0x93uy; 0x2Cuy; 0x06uy
                |])
                let! result = parseOscFloat32Async stream
                result |> Expect.float32Equal (nameof(result)) 1.234567936E9f
            })
            testCaseAsync "-123 point 45" (async {
                use stream = new MemoryStream([|
                    0xC2uy; 0xF6uy; 0xE6uy; 0x66uy
                |])
                let! result = parseOscFloat32Async stream
                result |> Expect.float32Equal (nameof(result)) -123.45f
            })
            testCaseAsync "nan" (async {
                use stream = new MemoryStream([|
                    0x7Fuy; 0xFFuy; 0xFFuy; 0xFFuy
                |])
                let! result = parseOscFloat32Async stream
                result |> Expect.float32Equal (nameof(result)) nanf
            })
            testCaseAsync "infinity" (async {
                use stream = new MemoryStream([|
                    0x7Fuy; 0x80uy; 0uy; 0uy
                |])

                let! result = parseOscFloat32Async stream
                result |> Expect.float32Equal (nameof(result)) infinityf
            })
        ]
        testList (nameof(parseOscStringAsync)) [
            testCaseAsync "empty string" (async {
                use stream = new MemoryStream([|
                    0uy; 0uy; 0uy; 0uy
                |])

                let! result = parseOscStringAsync stream
                result |> Expect.equal (nameof(result)) ""
            })
            testCaseAsync "a" (async {
                use stream = new MemoryStream([|
                    byte 'a'; 0uy; 0uy; 0uy
                |])

                let! result = parseOscStringAsync stream
                result |> Expect.equal (nameof(result)) "a"
            })
            testCaseAsync "AB" (async {
                use stream = new MemoryStream([|
                    byte 'A'; byte 'B'; 0uy; 0uy
                |])

                let! result = parseOscStringAsync stream
                result |> Expect.equal (nameof(result)) "AB"
            })
            testCaseAsync "abc" (async {
                use stream = new MemoryStream([|
                    byte 'a'; byte 'b'; byte 'c'; 0uy
                |])

                let! result = parseOscStringAsync stream
                result |> Expect.equal (nameof(result)) "abc"
            })
            testCaseAsync "XYZA" (async {
                use stream = new MemoryStream([|
                    byte 'X'; byte 'Y'; byte 'Z'; byte 'A'
                    0uy; 0uy; 0uy; 0uy
                |])

                let! result = parseOscStringAsync stream
                result |> Expect.equal (nameof(result)) "XYZA"
            })
            testCaseAsync "XYZABC" (async {
                use stream = new MemoryStream([|
                    byte 'X'; byte 'Y'; byte 'Z'; byte 'A'
                    byte 'B'; byte 'C'; 0uy;      0uy
                |])

                let! result = parseOscStringAsync stream
                result |> Expect.equal (nameof(result)) "XYZABC"
            })
            testCaseAsync "Hello, to the world of OSC!" (async {
                use stream = new MemoryStream([|
                    byte 'H'; byte 'e'; byte 'l'; byte 'l'
                    byte 'o'; byte ','; byte ' '; byte 't'
                    byte 'o'; byte ' '; byte 't'; byte 'h'
                    byte 'e'; byte ' '; byte 'w'; byte 'o'
                    byte 'r'; byte 'l'; byte 'd'; byte ' '
                    byte 'o'; byte 'f'; byte ' '; byte 'O'
                    byte 'S'; byte 'C'; byte '!'; 0uy
                |])

                let! result = parseOscStringAsync stream
                result |> Expect.equal (nameof(result)) "Hello, to the world of OSC!"
            })
            testCaseAsync "The quick brown fox jumped over the lazy dog." (async {
                use stream = new MemoryStream([|
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
                    byte '.'; 0uy;      0uy;      0uy
                |])

                let! result = parseOscStringAsync stream
                result |> Expect.equal (nameof(result)) "The quick brown fox jumped over the lazy dog."
            })
        ]
        testList (nameof(parseOscTypeTagAsync)) [
            testCaseAsync "(no args)" (async {
                use stream = new MemoryStream([|
                    byte ','; 0uy;      0uy;      0uy
                |])

                let! result = parseOscTypeTagAsync stream
                result |> Expect.equal (nameof(result)) ""
            })
            testCaseAsync "i" (async {
                use stream = new MemoryStream([|
                    byte ','; byte 'i'; 0uy;      0uy
                |])

                let! result = parseOscTypeTagAsync stream
                result |> Expect.equal (nameof(result)) "i"
            })
            testCaseAsync "if" (async {
                use stream = new MemoryStream([|
                    byte ','; byte 'i'; byte 'f';      0uy
                |])

                let! result = parseOscTypeTagAsync stream
                result |> Expect.equal (nameof(result)) "if"
            })
            testCaseAsync "fss" (async {
                use stream = new MemoryStream([|
                    byte ','; byte 'f'; byte 's'; byte 's'
                    0uy;      0uy;      0uy;      0uy
                |])

                let! result = parseOscTypeTagAsync stream
                result |> Expect.equal (nameof(result)) "fss"
            })
            testCaseAsync "fsis" (async {
                use stream = new MemoryStream([|
                    byte ','; byte 'f'; byte 's'; byte 'i'
                    byte 's'; 0uy;      0uy;      0uy
                |])

                let! result = parseOscTypeTagAsync stream
                result |> Expect.equal (nameof(result)) "fsis"
            })
        ]
        testList (nameof(parseOscAddressPatternAsync)) [
            testCaseAsync "/foo/bar" (async {
                use stream = new MemoryStream([|
                    byte '/'; byte 'f'; byte 'o'; byte 'o'
                    byte '/'; byte 'b'; byte 'a'; byte 'r'
                    0uy;      0uy;      0uy;      0uy
                |])

                let! result = parseOscAddressPatternAsync stream
                result |> Expect.equal (nameof(result)) "/foo/bar"
            })
        ]
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
