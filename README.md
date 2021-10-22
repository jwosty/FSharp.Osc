An [OSC 1.1](http://opensoundcontrol.org/files/2009-NIME-OSC-1.1.pdf) library for F#.

A server:

```fsharp
#load "Osc.fs"
open Osc.fs

// listen for messages at /thing/volume on port 12345
let server =
    new OscUdpServer("127.0.0.1", 12345, [
        Path ("thing", [
            Method ("volume", (fun msg -> async { printfn "%O" msg }))
        ])
    ])

server.Run ()

```

A client:

```fsharp
#load "Osc.fs"
open Osc.fs
open System

let client = new OscUdpClient("127.0.0.1", 12345)
let rand = System.Random()

// send some random values to /thing/volume at port 12345
for _ in 0..9 do
    let msg = { addressPattern = "/thing/volume"; arguments = [OscFloat32 (float32 (rand.NextDouble()))] }
    printfn "Sending message"
    client.SendMessage msg |> ignore
    System.Threading.Thread.Sleep 1_000
```

Currently, the following is supported:

* Strings, 32 bit ints, 32 bit floats, booleans, None, and Impulse data types
* UDP - client and server
* TCP - client and server
    * Choose between OSC 1.0 and OSC 1.1 frame encodings

The following is not yet implemented ([PRs are welcome!](https://github.com/jwosty/Osc.fs/pulls)):

* Timetag data types
* Bundles
* TCP server SLIP encoding (aka OSC 1.1 frame encoding)
