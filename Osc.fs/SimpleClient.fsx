#load "Osc.fs"
open Osc.fs
open System
open System.Threading

let client = new OscUdpClient("127.0.0.1", 12345)

client.SendMessage { addressPattern = "/button1"; arguments = [OscString "Hello, world!"]  }


let rand = Random()

for _ in 0..9 do
    let msg = { addressPattern = "/filter"; arguments = [OscFloat32 (float32 (rand.NextDouble()))] }
    printfn "Sending message"
    client.SendMessage msg |> ignore
    Thread.Sleep 1_000
