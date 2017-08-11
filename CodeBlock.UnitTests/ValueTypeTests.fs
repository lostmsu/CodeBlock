namespace CodeBlock.UnitTests

open System

open Microsoft.VisualStudio.TestTools.UnitTesting    

open TestCore

[<StructAttribute>]
type Vector =
    val mutable X: int
    val mutable Y: int

    new(x, y) = { X = x; Y = y; }

    override this.ToString() = sprintf "%d, %d (0x%x, 0x%x)" this.X this.Y this.X this.Y

[<StructAttribute>]
type SmallVector =
    val mutable X: int16
    val mutable Y: int16

    override this.ToString() = sprintf "%d, %d (0x%x, 0x%x)" this.X this.Y this.X this.Y

[<Struct>]
type ComplexStruct =
    val mutable I16: int16
    val mutable Vect: Vector
    val mutable I32: int32

[<TestClass>]
type ValueTypes() =
    static member SmallAddMethod(v1: SmallVector, v2: SmallVector) =
        let mutable result = SmallVector()
        result.X <- v1.X + v2.X
        result.Y <- v1.Y + v2.Y
        result

    [<TestMethod>]        
    member this.SmallAdd() =
        let testVector1 = SmallVector(X = 1s, Y = 2s)
        let testVector2 = SmallVector(X = 3s, Y = 5s)
        assertTest <@ ValueTypes.SmallAddMethod(testVector1, testVector2) @>

    static member AddMethod(v1: Vector, v2: Vector) =
        let mutable result = Vector()
        result.X <- v1.X + v2.X
        result.Y <- v1.Y + v2.Y
        result

    [<TestMethod>]        
    member this.AddVectors() =
        let testVector1 = Vector(X = 1, Y = 2)
        let testVector2 = Vector(X = 3, Y = 5)
        assertTest <@ ValueTypes.AddMethod(testVector1, testVector2) @>

    static member ComplexStructHashMethod(value: ComplexStruct) =
        int value.I16 ^^^ (value.Vect.X + value.Vect.Y) ^^^ int value.I32

    [<TestMethod>]
    member this.ComplexStructHash() =
        let structure = ComplexStruct(I16 = 0xBADCs, Vect = Vector(0x1FFF2EEE, 0xF1110000), I32 = 0xDEADBADD)
        assertTest <@ ValueTypes.ComplexStructHashMethod(structure) @>

    static member ByRef(v1: Vector byref, v2: Vector) =
        v1.X <- v1.X + v2.X
        v1.Y <- v1.Y + v2.Y

    [<TestMethod>]
    member this.ByRef() =
        let orig = box <| Vector(X = 1, Y = 1)
        let toAdd = Vector(X = -1, Y = -2)
        let args = [| orig; box toAdd |]
        let native = compile <| typeof<ValueTypes>.GetMethod("ByRef", [| typeof<Vector>.MakeByRefType(); typeof<Vector> |])
        native.DynamicInvoke args |> ignore
        let expected = Vector(0, -1)
        Assert.AreEqual(expected, args.[0])
