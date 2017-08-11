namespace CodeBlock.ObjectModel

open Mono.Cecil
open Mono.Cecil.Cil

open LLVM

open Lost.FSharp

open CodeBlock
open CodeBlock.Translation
open CodeBlock.TypeSystem
open CodeBlock.TypeSystem.Cecil
open CodeBlock.Utils

type BasicObject =
    struct
        val TypeMethods: nativeptr<nativeint>
    end

module private BasicModelData =
    let basicObjectType = typeof<BasicObject>.Definition
    let typeMethodsField = findByName "TypeMethods" basicObjectType.Fields

open BasicModelData

type BasicObjectModel() =
    let methodTables = Dict.create()

    member private this.GetMethodTable(actualType: TypeDefinition) =
        match Dict.tryFind actualType methodTables with
        | Some(table) -> table
        | None ->
            let baseTable =
                if BuiltIns.isObject actualType then
                    // TODO: GetHashCode, both Equals and ReferenceEquals
                    []
                else
                    let baseType = actualType.BaseType.ResolveFixed()
                    this.GetMethodTable(baseType)
            let declaredMethods =
                actualType.Methods
                |> Seq.filter (fun m -> m.DeclaringType = actualType)
            let methodTable =
                declaredMethods
                |> Seq.fold(fun table m -> m :: table) baseTable
            methodTable

    member private this.GetMethodTableIndex(actualType, func) =
        let methodTable = this.GetMethodTable(actualType)
        let reversedIndex = List.findIndex((=) func) methodTable
        methodTable.Length - reversedIndex - 1

    member private this.Ldvirtftn context state (instruction: Cil.Instruction) =
        match state with
        | { Stack = Object(t) :: stack;
            NativeStack = _ :: nstack; } ->
            //TODO: provide calling convention wrapping
            let actualType = t.ResolveFixed()

            let func = Generics.resolveMethodReference context.TypeMap (downcast instruction.Operand)
            let declaringType = func.DeclaringType.ResolveFixed()
            if declaringType.IsInterface then
                notImplemented()

            let il = context.Method.Body.GetILProcessor()
            let loadTypeMethods = il.Create(OpCodes.Ldfld, typeMethodsField)
            match context.ParentTranslator.Translate context state loadTypeMethods with
            | Some{ Stack = Native :: stack;
                NativeStack = methodTableAddress :: nstack; } ->

                let funcDefinition = func.Resolve()
                let index = this.GetMethodTableIndex(actualType, funcDefinition)
                let nativeIndex = IntegerType.GetInt32(context.NativeContext).Constant(uint64 index, false)
                notImplemented()

            | Some state ->
                state.Error(LoweringFailedException(instruction, "Unexpected stack state after translating lowered instruction"))
            | None ->
                state.Error(LoweringFailedException(instruction, "Don't know how to translate lowered instruction"))

        | state ->
            { state with Errors = invalidArgs instruction :: state.Errors }

    interface IInstructionTranslator with
        member this.Translate context state instruction =
            match instruction.OpCode.Code with
            | Code.Ldvirtftn ->
                Some(this.Ldvirtftn context state instruction)

            | _ ->
                None

