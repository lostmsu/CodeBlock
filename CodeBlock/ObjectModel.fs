namespace CodeBlock.ObjectModel

open CodeBlock

type IObjectModel =
    abstract LoadVirtualFunction: StackItem list -> StackItem list * bool

[<Struct>]
type DummyObjectModel =
    struct
        val TypeHandle: nativeint
    end

[<Sealed>]
type ObjectBox<'a>(obj) = class
        let mutable value: 'a = obj
    end
