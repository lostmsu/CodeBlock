namespace CodeBlock.UnitTests.ObjectModel

open Microsoft.VisualStudio.TestTools.UnitTesting

open CodeBlock
open CodeBlock.GC
open CodeBlock.ObjectModel
open CodeBlock.TypeSystem.Cecil

open TestCore

[<TestClass>]
type ObjectModelTestCore private() =
    [<AssemblyInitialize>]
    static member Load(context: TestContext) =
        LLVM.NativeLibrary.LLVMDLL.Load()

        LLVM.Target.InitializeNative()

        jit <- JIT()

        let thisAssembly = System.Reflection.Assembly.GetExecutingAssembly()
        registerResolverAssembly thisAssembly

        jit.ObjectModel <- typeof<DummyObjectModel>.Definition
        jit.GC <- typeof<DummyGC>.Definition

        let basicObjectModel = BasicObjectModel()
        jit.RegisterTranslator(basicObjectModel)
