namespace CodeBlock

type ManagedArray<'a> =
    struct
        val length: unativeint
        val data: System.IntPtr
    end


