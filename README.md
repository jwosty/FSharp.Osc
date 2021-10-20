An implementation of most of [OSC 1.0](http://opensoundcontrol.org/spec-1_0.html) in F#.

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
