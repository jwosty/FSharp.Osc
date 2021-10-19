#load "Osc.fs"
open Osc.fs
open System.Runtime

let (|OscNumber|_|) arg =
    match arg with
    | OscFloat32 x -> Some x
    | OscInt32 x -> Some (float32 x)
    | _ -> None

let argsStr args = args |> Seq.map (fun arg -> string arg) |> String.concat "," |> sprintf "[%s]"

let server =
    new OscUdpServer("127.0.0.1", 12345, [
        Method ("things/1/value", (fun msg -> async {
            match msg.arguments with
            | [OscNumber value] -> printfn "New value for thing 1: %f" value
            | _ -> eprintfn "%s : wrong data type" msg.addressPattern
        }))
        Method ("things/2/value", (fun msg -> async {
            match msg.arguments with
            | [OscNumber value] -> printfn "New value for thing 2: %f" value
            | _ -> eprintfn "%s : wrong data type" msg.addressPattern
        }))
        Method ("button1", (fun msg -> async {
            printfn "Button 1 pressed! (args: %s)" (argsStr msg.arguments)
        }))
    ])

server.Run ()
