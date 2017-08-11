namespace CodeBlock.UnitTests.ObjectModel

open Microsoft.VisualStudio.TestTools.UnitTesting    

open TestCore

[<TestClass>]
type Arrays() =
    static member LengthMethod() =
        let arr = [| 22; 10; -35; 1 |]
        arr.Length

    [<TestMethod>]        
    member this.LengthTest() =
        assertTest <@ Arrays.LengthMethod() @>

    static member SumMethod() =
        let arr = [| 20; -15; 11 |]
        
        let mutable sum = 0
        for i = 0 to 2 do
            sum <- sum + arr.[i]

        sum

    [<TestMethod>]
    member this.SumTest() =
        assertTest <@ Arrays.SumMethod() @>