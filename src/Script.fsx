#load "FSharp.Osc.fs"
open FSharp.Osc
open System
open System.Threading

let touchosc : IOscClient =
    //upcast new OscUdpClient("127.0.0.1", 12346)
    upcast OscTcpClient.Connect ("127.0.0.1", 5001)

//let c = Channels.Channel.CreateUnbounded ()

let l = obj()

touchosc.SendMessage { addressPattern = "/things/1/value"; arguments = [OscFloat32 0.5f] }
touchosc.SendMessage { addressPattern = "/foo/bar"; arguments = [OscString "Hello, world!"] }

let vary addr n =
    Async.Start <| async {
        for i in 0..n do
            let m = { addressPattern = addr; arguments = [OscFloat32 (sin ((float32 i / float32 n) * float32 Math.PI)) ] }
            lock l (fun () -> touchosc.SendMessage m |> ignore)
            printfn "%O" m
            do! Async.Sleep 1
    }

vary "/things/1/value" 50; Thread.Sleep 250; vary "/things/2/value" 50

vary "/things/*/value" 50
