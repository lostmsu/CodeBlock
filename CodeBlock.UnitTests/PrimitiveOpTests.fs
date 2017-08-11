namespace CodeBlock.UnitTests

open System

open Microsoft.VisualStudio.TestTools.UnitTesting    

open TestCore 

[<TestClass>]
type PrimitiveOps() =
    static member Ret42Method() = 42
    
    [<TestMethod>]
    member this.Ret42() =
        assertTest <@ PrimitiveOps.Ret42Method() @>

    //#region Factorial
    static member FactorialMethod(n) =
        let mutable result = 1
        let mutable i = 2
        while i <= n do
            result <- result * i
            i <- i + 1
        result

    [<TestMethod>]
    member this.FactorialNeg() =
        [ -2; -1; Int32.MinValue ]
        |> List.iter(fun n -> assertTest <@ PrimitiveOps.FactorialMethod(n) @>)

    [<TestMethod>]
    member this.FactorialZero() =
        assertTest <@ PrimitiveOps.FactorialMethod(0) @>

    [<TestMethod>]
    member this.FactorialPositive() =
        [ 1; 2; 3; 11; 25 ]
        |> List.iter(fun n -> assertTest <@ PrimitiveOps.FactorialMethod(n) @>)

    static member FactorialRecMethod(n) =
        if n <= 1 then 1
        else PrimitiveOps.FactorialRecMethod(n - 1) * n

    [<TestMethod>]
    member this.FactorialRecNeg() =
        [ -2; -1; Int32.MinValue ]
        |> List.iter(fun n -> assertTest <@ PrimitiveOps.FactorialRecMethod(n) @>)

    [<TestMethod>]
    member this.FactorialRecZero() =
        assertTest <@ PrimitiveOps.FactorialRecMethod(0) @>

    [<TestMethod>]
    member this.FactorialRecPositive() =
        [ 1; 2; 3; 11; 25 ]
        |> List.iter(fun n -> assertTest <@ PrimitiveOps.FactorialRecMethod(n) @>)
    //#endregion

    static member ToInt32(value: int64) = int value

    [<TestMethod>]
    member this.ToInt32() =
        [ -38L; -38000000000L; Int64.MinValue; Int64.MaxValue; 1L; 0L ]
        |> List.iter(fun n -> assertTest <@ PrimitiveOps.ToInt32(n) @>)

    static member Add(a: int, b: int64) = int64 a + b

    [<TestMethod()>]
    member this.AddI32I64() =
        assertTest <@ PrimitiveOps.Add(-1, -10000000000L) @>
