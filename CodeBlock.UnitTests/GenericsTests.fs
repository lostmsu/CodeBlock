namespace CodeBlock.UnitTests

open System

open Microsoft.VisualStudio.TestTools.UnitTesting    

open TestCore

[<Struct>]
type TestNullable<'a> =
    val Value: 'a
    val HasValue: bool

    new (value: 'a) = { Value = value; HasValue = true }

[<TestClass>]
type Generics() =
    static member NullableTestMethod() =
        let nullable = TestNullable(10)
        if nullable.HasValue then nullable.Value else -1

    [<TestMethod>]        
    member this.NullableTest() =
        assertTest <@ Generics.NullableTestMethod() @>