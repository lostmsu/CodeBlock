namespace CodeBlock.GC

module private Externals =
    [<System.Runtime.InteropServices.DllImportAttribute("kernel32.dll")>]
    extern nativeint malloc(nativeint size)

[<Struct>]
type DummyGC =
    static member Alloc(size: nativeint, align: nativeint) =
        Externals.malloc(size)