namespace Osc.fs
#nowarn "9"

module internal Span =
    open System
    open FSharp.NativeInterop

    // FS0009: Uses of this construct may result in the generation of unverifiable IL code.

    let inline stackalloc<'a when 'a: unmanaged> size =
        let p = NativePtr.stackalloc<'a> size |> NativePtr.toVoidPtr
        Span<'a>(p, size)
