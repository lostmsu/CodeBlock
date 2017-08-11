module TestCore

open System
open System.Reflection

open Microsoft.FSharp.Linq.QuotationEvaluation
open Microsoft.FSharp.Quotations.Patterns

open Microsoft.VisualStudio.TestTools.UnitTesting

open Mono.Cecil

open CodeBlock
open CodeBlock.TypeSystem.Cecil
open CodeBlock.RuntimeUtils

let private resolverModules = ResizeArray()

let registerResolverAssembly(assembly: Assembly) =
    let path = Uri(assembly.CodeBase).LocalPath
    let assemblyDefinition = AssemblyDefinition.ReadAssembly(path)
    resolverModules.AddRange(assemblyDefinition.Modules)

let assemblyTypeResolver(assembly: Assembly) =
    let path = Uri(assembly.CodeBase).LocalPath
    let assemblyDefinition = AssemblyDefinition.ReadAssembly(path)
    let module0 = assemblyDefinition.Modules.[0]
    fun (cilType: Type) ->
        module0.Types
        |> Seq.find(fun t -> t.FullName = cilType.FullName)

let testAssemblyResolver =
    let thisAssembly = Assembly.GetExecutingAssembly()
    registerResolverAssembly thisAssembly
    fun (cilType: Type) ->
        resolverModules
        |> Seq.collect (fun m -> m.Types)
        |> Seq.find(fun t -> t.FullName = cilType.FullName)

let mutable jit = JIT()

open System.Linq.Expressions

let makeDelegate func =
    let delegateType = DelegateTypes.Get func
    Delegate.CreateDelegate(delegateType, func)

let eval arg =
    match arg with
    | Value(value, _) ->
        value
    | _ ->
        arg.EvalUntyped()

let compile (func: MethodInfo) =
        let typeDef = testAssemblyResolver func.DeclaringType
        let methodDef = typeDef.Method(func.Name)
        let nfunc = jit.Compile(methodDef)

        let delegateType = DelegateTypes.Get func
        let enableDebugMode = System.Diagnostics.Debugger.IsAttached
        jit.GetWrapper(nfunc, delegateType, enableDebugMode)

let compileAndCheck = function
    | Call(None, func, args) ->
        let funcDelegate = makeDelegate func

        let ndelegate = compile func

        let argValues = args |> Seq.map eval |> Array.ofSeq

        let expected = funcDelegate.DynamicInvoke(argValues)
        let actual = ndelegate.DynamicInvoke(argValues)

        expected, actual

    | expr ->
        NotSupportedException(expr.ToString())
        |> raise

let runTest expr =
    let expected, actual = compileAndCheck expr
    printfn "%A: expected: %A; actual: %A" expr expected actual

let assertTest expr =
    let expected, actual = compileAndCheck expr
    Assert.AreEqual(expected, actual)