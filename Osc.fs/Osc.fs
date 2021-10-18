namespace Osc.fs
open System
open System.IO
open System.Runtime.CompilerServices
open System.Text
open System.Runtime.InteropServices
open FSharp.NativeInterop

//[<AutoOpen>]
//module StreamExtensions =
//    type System.IO.Stream with
//        member this.AsyncRead ()



// OSC spec: https://hangar.org/wp-content/uploads/2012/01/The-Open-Sound-Control-1.0-Specification-opensoundcontrol.org_.pdf

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

// TODO: Check big-endian systems. This *should* work correctly, but I haven't tested it
module OscAtom =
    let parseAsync (input: Stream) = async {
        let! typeTagByte = input.AsyncRead 1
        match char typeTagByte.[0] with
        | 'i' ->
            let! bytes = input.AsyncRead 4
            let bytes' = if BitConverter.IsLittleEndian then Array.rev bytes else bytes
            return OscInt32 (BitConverter.ToInt32 (bytes', 0))
        | 'f' ->
            let! bytes = input.AsyncRead 4
            let bytes' = if BitConverter.IsLittleEndian then Array.rev bytes else bytes
            return OscFloat32 (BitConverter.ToSingle (bytes', 0))
        | 's' ->

            let strs = System.Collections.Generic.List<string>()

            let mutable cont = true
            let byteArr = Array.zeroCreate<byte> 4

            // FIXME: eliminiate any unnecessary extra allocations here

            while cont do
                let! bytesRead = input.AsyncRead byteArr
                if bytesRead <> 4 then raise (IOException("Unexpected end of stream"))

                let len =
                    if byteArr.[0] = 0uy then 0
                    elif byteArr.[1] = 0uy then 1
                    elif byteArr.[2] = 0uy then 2
                    elif byteArr.[3] = 0uy then 3
                    else 4

                let str = Encoding.ASCII.GetString (byteArr, 0, len)
                strs.Add str

                cont <- len >= 4

            return OscString (String.Concat(strs))
    }

    let tryParseAsync (input: Stream) = async {
        let! result = parseAsync input
        return Ok result
    }
