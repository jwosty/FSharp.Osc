#load "FSharp.Osc"
open FSharp.Osc
open System
open System.Diagnostics

let (|OscNumber|_|) arg =
    match arg with
    | OscFloat32 x -> Some x
    | OscInt32 x -> Some (float32 x)
    | _ -> None

let argsStr args = args |> Seq.map (fun arg -> string arg) |> String.concat "," |> sprintf "[%s]"

let oscillator oscillatorId =
    Path (oscillatorId, [
        Method ("frequency", (fun msg -> async {
            match msg.arguments with
            | [OscNumber value] -> printfn "oscillator %s frequency: %f" oscillatorId value
            | _ -> eprintfn "%s : wrong data type" msg.addressPattern
        }))
        Method ("amplitude", (fun msg -> async {
            match msg.arguments with
            | [OscNumber value] -> printfn "oscillator %s amplitude: %f" oscillatorId value
            | _ -> eprintfn "%s : wrong data type" msg.addressPattern
        }))
    ])

let methods = [
    Path ("oscillator", [
        oscillator "1"
        oscillator "2"
        oscillator "3"
    ])
    Path ("window", [
        Method ("openurl", (fun msg -> async {
            match msg.arguments with
            | [OscString urlStr] ->
                let uri = Uri urlStr
                if uri.Scheme = "http" || uri.Scheme = "https" then
                    Process.Start (ProcessStartInfo(uri.ToString(), UseShellExecute = true, Verb = "Open")) |> ignore
                else
                    eprintfn "Url scheme must be http or https"
            | _ -> ()
        }))
    ])
]

let port = 12345
let useUdp = false

let server : IOscServer =
    if useUdp then upcast new OscUdpServer("127.0.0.1", port, methods)
    else raise (NotImplementedException()) (*upcast new OscTcpServer(System.Net.IPAddress.Any, port, methods)*)

server.Run ()
