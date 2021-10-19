﻿module Osc.fs
open System
open System.IO
open System.Runtime.CompilerServices
open System.Text
open System.Runtime.InteropServices
open FSharp.NativeInterop

type MalformedMessageException =
    inherit Exception

    new(message: string) = { inherit Exception(message) }
    new(message: string, innerExn: Exception) = { inherit Exception(message, innerExn) }

let private raiseUnexpectedEof () = raise (MalformedMessageException("Unexpected end of stream"))
let private raiseImpossible () = raise (InvalidOperationException("This should never be thrown"))

// OSC spec: http://opensoundcontrol.org/spec-1_0.html

//[<IsByRefLike; Struct>]
type OscAtom =
    /// 32-bit big-endian signed two's complement integer
    | OscInt32 of intValue:int32
    //| OscTimetag
    /// 32-bit big-endian IEEE 754 floating point number
    | OscFloat32 of floatValue:float32
    /// A sequence of non-null ASCII characters followed by a null, followed by 0-3 additional null characters to make
    /// the total number of bits a multiple of 32.
    | OscString of stringValue:string
    //| OscBlob of blobData:Span<byte>

type OscTypeTag = string

// TODO: Check big-endian systems. This *should* work correctly, but I haven't tested it
let parseOscInt32Async (input: Stream) = async {
    let! bytes = input.AsyncRead 4
    let bytes' = if BitConverter.IsLittleEndian then Array.rev bytes else bytes
    return BitConverter.ToInt32 (bytes', 0)
}

// TODO: see above
let parseOscFloat32Async (input: Stream) = async {
    let! bytes = input.AsyncRead 4
    let bytes' = if BitConverter.IsLittleEndian then Array.rev bytes else bytes
    return BitConverter.ToSingle (bytes', 0)
}

//let private parseOscStringAsyncStartingWith firstCharWithErr (input: Stream) = async {

//}

let parseOscStringAsync (input: Stream) = async {
    let strs = System.Collections.Generic.List<string>()
    let mutable cont = true
    let byteArr = Array.zeroCreate<byte> 4
    
    // FIXME: eliminiate any unnecessary extra allocations here
    

    while cont do
        let! bytesRead = input.AsyncRead byteArr
        if bytesRead <> 4 then raiseUnexpectedEof ()

        let len =
            if byteArr.[0] = 0uy then 0
            elif byteArr.[1] = 0uy then 1
            elif byteArr.[2] = 0uy then 2
            elif byteArr.[3] = 0uy then 3
            else 4
    
        let str = Encoding.ASCII.GetString (byteArr, 0, len)
        strs.Add str
    
        cont <- len >= 4
    
    return String.Concat(strs)
}

let parseOscTypeTagAsync (input: Stream) = async {
    let! str = parseOscStringAsync input
    if str.Length = 0 then
        return raise (MalformedMessageException($"Failed to parse type tag. Expected ',' but got null."))
    elif str.[0] <> ',' then
        return raise (MalformedMessageException($"Failed to parse type tag. Expected ',' but got '{str.[0]}'"))
    else
        return str.[1..str.Length-1]
}


