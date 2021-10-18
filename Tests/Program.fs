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

let (|OkOscFloat32|) x =
    match x with
    | Ok (OscFloat32 x') -> OkOscFloat32 x'
    | _ -> raise (AssertException($"Was not Ok({nameof(OscFloat32)})"))

[<Tests>]
let tests =
    testList (nameof(OscAtom)) [
        testList (nameof(OscAtom.tryParseAsync)) [
            testList "int32" [
                testCaseAsync "0" (async {
                    use stream = new MemoryStream([|
                        byte 'i'
                        0uy; 0uy; 0uy; 0uy
                    |])
                
                    let! result = OscAtom.tryParseAsync stream
                    result |> Expect.equal "result" (Ok (OscAtom.OscInt32 0))
                })
                testCaseAsync "10" (async {
                    use stream = new MemoryStream([|
                        byte 'i'
                        0x0uy; 0x0uy; 0x0uy; 0xAuy
                    |])
                
                    let! result = OscAtom.tryParseAsync stream
                    result |> Expect.equal "result" (Ok (OscAtom.OscInt32 10))
                })
                testCaseAsync "1000" (async {
                    use stream = new MemoryStream([|
                        byte 'i'
                        0x00uy; 0x00uy; 0x03uy; 0xE8uy
                    |])
                    
                    let! result = OscAtom.tryParseAsync stream
                    result |> Expect.equal "result" (Ok (OscAtom.OscInt32 1000))
                })
                testCaseAsync "1_000_000" (async {
                    use stream = new MemoryStream([|
                        byte 'i'
                        0x00uy; 0x0Fuy; 0x42uy; 0x40uy
                    |])
                
                    let! result = OscAtom.tryParseAsync stream
                    result |> Expect.equal "result" (Ok (OscAtom.OscInt32 1_000_000))
                })
                testCaseAsync "1_000_000_023" (async {
                    use stream = new MemoryStream([|
                        byte 'i'
                        0x3Buy; 0x9Auy; 0xCAuy; 0x17uy 
                    |])
                
                    let! result = OscAtom.tryParseAsync stream
                    result |> Expect.equal "result" (Ok (OscAtom.OscInt32 1_000_000_023))
                })
                testCaseAsync "-1_000_000_023" (async {
                    use stream = new MemoryStream([|
                        byte 'i'
                        0xC4uy; 0x65uy; 0x35uy; 0xE9uy 
                    |])
                
                    let! result = OscAtom.tryParseAsync stream
                    result |> Expect.equal "result" (Ok (OscAtom.OscInt32 -1_000_000_023))
                })
            ]
            testList "float32" [
                testCaseAsync "0" (async {
                    use stream = new MemoryStream([|
                        byte 'f'
                        0uy; 0uy; 0uy; 0uy
                    |])
                
                    let! result = OscAtom.tryParseAsync stream
                    result |> Expect.equal "result" (Ok (OscAtom.OscFloat32 0.f))
                })
                testCaseAsync "1 point 234567936E9" (async {
                    use stream = new MemoryStream([|
                        byte 'f'
                        0x4Euy; 0x93uy; 0x2Cuy; 0x06uy
                    |])
                    let! (OkOscFloat32 result) = OscAtom.tryParseAsync stream
                    result |> Expect.float32Equal (nameof(result)) 1.234567936E9f
                })
                testCaseAsync "-123 point 45" (async {
                    use stream = new MemoryStream([|
                        byte 'f'
                        0xC2uy; 0xF6uy; 0xE6uy; 0x66uy
                    |])
                    let! (OkOscFloat32 result) = OscAtom.tryParseAsync stream
                    result |> Expect.float32Equal (nameof(result)) -123.45f
                })
                testCaseAsync "nan" (async {
                    use stream = new MemoryStream([|
                        byte 'f'
                        0x7Fuy; 0xFFuy; 0xFFuy; 0xFFuy
                    |])
                    let! (OkOscFloat32 result) = OscAtom.tryParseAsync stream
                    result |> Expect.float32Equal (nameof(result)) nanf
                })
                testCaseAsync "infinity" (async {
                    use stream = new MemoryStream([|
                        byte 'f'
                        0x7Fuy; 0x80uy; 0uy; 0uy
                    |])

                    let! (OkOscFloat32 result) = OscAtom.tryParseAsync stream
                    result |> Expect.float32Equal (nameof(result)) infinityf
                })
            ]
            testList "string" [
                testCaseAsync "empty string" (async {
                    use stream = new MemoryStream([|
                        byte 's'
                        0uy; 0uy; 0uy; 0uy
                    |])

                    let! result = OscAtom.tryParseAsync stream
                    result |> Expect.equal (nameof(result)) (Ok (OscString ""))
                })
                testCaseAsync "a" (async {
                    use stream = new MemoryStream([|
                        byte 's'
                        byte 'a'; 0uy; 0uy; 0uy
                    |])

                    let! result = OscAtom.tryParseAsync stream
                    result |> Expect.equal (nameof(result)) (Ok (OscString "a"))
                })
                testCaseAsync "AB" (async {
                    use stream = new MemoryStream([|
                        byte 's'
                        byte 'A'; byte 'B'; 0uy; 0uy
                    |])

                    let! result = OscAtom.tryParseAsync stream
                    result |> Expect.equal (nameof(result)) (Ok (OscString "AB"))
                })
                testCaseAsync "abc" (async {
                    use stream = new MemoryStream([|
                        byte 's'
                        byte 'a'; byte 'b'; byte 'c'; 0uy
                    |])

                    let! result = OscAtom.tryParseAsync stream
                    result |> Expect.equal (nameof(result)) (Ok (OscString "abc"))
                })
                testCaseAsync "XYZA" (async {
                    use stream = new MemoryStream([|
                        byte 's'
                        byte 'X'; byte 'Y'; byte 'Z'; byte 'A'
                        0uy; 0uy; 0uy; 0uy
                    |])

                    let! result = OscAtom.tryParseAsync stream
                    result |> Expect.equal (nameof(result)) (Ok (OscString "XYZA"))
                })
                testCaseAsync "XYZABC" (async {
                    use stream = new MemoryStream([|
                        byte 's'
                        byte 'X'; byte 'Y'; byte 'Z'; byte 'A'
                        byte 'B'; byte 'C'; 0uy;      0uy
                    |])

                    let! result = OscAtom.tryParseAsync stream
                    result |> Expect.equal (nameof(result)) (Ok (OscString "XYZABC"))
                })
                testCaseAsync "Hello, to the world of OSC!" (async {
                    use stream = new MemoryStream([|
                        byte 's'
                        byte 'H'; byte 'e'; byte 'l'; byte 'l'
                        byte 'o'; byte ','; byte ' '; byte 't'
                        byte 'o'; byte ' '; byte 't'; byte 'h'
                        byte 'e'; byte ' '; byte 'w'; byte 'o'
                        byte 'r'; byte 'l'; byte 'd'; byte ' '
                        byte 'o'; byte 'f'; byte ' '; byte 'O'
                        byte 'S'; byte 'C'; byte '!'; 0uy
                    |])

                    let! result = OscAtom.tryParseAsync stream
                    result |> Expect.equal (nameof(result)) (Ok (OscString "Hello, to the world of OSC!"))
                })
                testCaseAsync "The quick brown fox jumped over the lazy dog." (async {
                    use stream = new MemoryStream([|
                        byte 's'
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

                    let! result = OscAtom.tryParseAsync stream
                    result |> Expect.equal (nameof(result)) (Ok (OscString "The quick brown fox jumped over the lazy dog."))
                })
            ]
        ]
    ]

[<EntryPoint>]
let main args =
    runTestsInAssembly defaultConfig args
