#load "Osc.fs"
open Osc.fs
open System
open System.Threading

let client = new OscUdpClient("127.0.0.1", 12345)

client.SendMessage { addressPattern = "/window/openurl"; arguments = [OscString "https://github.com/jwosty/Osc.fs"] }

let rand = Random()

for _ in 0..9 do
    let msg = { addressPattern = "/oscillator/{1,2}/frequency"; arguments = [OscFloat32 (float32 (rand.NextDouble()))] }
    printfn "Sending message"
    client.SendMessage msg |> ignore
    Thread.Sleep 1_000
