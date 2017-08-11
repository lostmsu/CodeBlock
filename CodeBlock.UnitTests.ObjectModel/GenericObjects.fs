namespace CodeBlock.UnitTests.ObjectModel

open Microsoft.VisualStudio.TestTools.UnitTesting

open TestCore

[<AllowNullLiteral>]
type private LinkedListNode<'a>(value: 'a) =
    [<DefaultValue>]
    val mutable internal Next: LinkedListNode<'a>

    member this.Value = value
    

type private LinkedList<'a>() =
    let mutable count = 0u
    let mutable root = null
    let mutable tail = null

    member this.First = root

    member this.Add(value: 'a) =
        let node = LinkedListNode(value)
        if isNull root then
            root <- node
            tail <- node
            count <- 1u
        else
            tail.Next <- node
            tail <- node
            count <- count + 1u

    member this.Count = count

type ArrayList<'a>() =
    let mutable data = Array.zeroCreate 16
    let mutable count = 0

    member this.Count = count
    member this.Add(value: 'a) =
        if data.Length >= count then
            let newSize = if data.Length < 8 then 16
                          else Checked.(*) data.Length 2
            data <- Array.resize newSize data
        data.[count] <- value
        count <- count + 1

    member this.ToArray() = Array.resize count data

[<TestClass>]
type GenericObjects() =
    static member ListTestMethod() =
        let list = ArrayList()
        list.Add(10)
        list.Add(-1)
        list.Count

    [<TestMethod>]        
    member this.List() =
        assertTest <@ GenericObjects.ListTestMethod() @>

    static member LinkedListMethod() =
        // let hashset = System.Collections.Generic.Dictionary<int, int>()
        let list = LinkedList<_>()
        list.Add(10)
        list.Add(20)
        let mutable sum = 0
        let mutable head = list.First
        list.Count
//        while not <| obj.ReferenceEquals(head, null) do
//            sum <- sum + head.Value
//            head <- head.Next
//        sum

    [<TestMethod>]
    member this.LinkedList() =
        assertTest <@ GenericObjects.LinkedListMethod() @>