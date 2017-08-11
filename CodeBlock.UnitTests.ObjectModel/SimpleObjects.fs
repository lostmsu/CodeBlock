namespace CodeBlock.UnitTests.ObjectModel

open Microsoft.VisualStudio.TestTools.UnitTesting    

open TestCore

[<AllowNullLiteral>]
type private SimpleNode(value) =
    let mutable next = null
    member this.Value = value
    member this.Next
        with get() = next
        and set v = next <- v

[<TestClass>]
type SimpleObjects() =
    static member TakeObjMethod(o: obj) = ()

    [<TestMethod>]
    member this.TakeObject() =
        assertTest <@ SimpleObjects.TakeObjMethod(null) @>

    static member ListTestMethod() =
        let list = SimpleNode(10)
        let list = SimpleNode(5, Next = list)
        let list = SimpleNode(-1, Next = list)
        let mutable sum = 0
        let mutable current = list
        while not <| obj.ReferenceEquals(current, null) do
            sum <- sum + current.Value
            current <- current.Next
        sum

    [<TestMethod>]        
    member this.LinkedList() =
        assertTest <@ SimpleObjects.ListTestMethod() @>