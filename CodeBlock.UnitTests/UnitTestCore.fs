namespace CodeBlock.UnitTests

open Microsoft.VisualStudio.TestTools.UnitTesting   

open CodeBlock

open TestCore

[<TestClass>]
type UnitTestCore private() =
    [<AssemblyInitialize>]
    static member Load(context: TestContext) =
        LLVM.NativeLibrary.LLVMDLL.Load()

        LLVM.Target.InitializeNative()

        jit <- JIT()

        let thisAssembly = System.Reflection.Assembly.GetExecutingAssembly()
        registerResolverAssembly thisAssembly
