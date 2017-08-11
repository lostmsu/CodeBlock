namespace CodeBlock.TypeSystem

open System.Collections.Generic

open Mono.Cecil

open CodeBlock.TypeSystem.Cecil

module Generics =
    let rec getTypeMap(methodRef: MethodReference) =
        match methodRef with
        | :? MethodDefinition -> id
        | :? GenericInstanceMethod as generic -> generic.ResolveGenerics
        | _ when methodRef.HasGenericParameters || methodRef.IsGenericInstance ->
            System.NotSupportedException("This type of generic method is not supported")
            |> raise

        | hlmethod ->
            // TODO: generalize this code with compile proc
            let definition = hlmethod.Resolve()
            if definition = null then
                System.NotSupportedException(hlmethod.GetType().Name) |> raise
            else

            let map = Dictionary<TypeReference, _>()
            match hlmethod.DeclaringType with
            | :? GenericInstanceType as genType ->
                let baseType = genType.Resolve()
                genType.GenericArguments
                |> Seq.iteri (fun i genArg -> map.[baseType.GenericParameters.[i]] <- genArg)

                let typeMap =
                    typeMapRec (flip Dict.tryFind map)

                typeMap

            | _ -> getTypeMap definition

    let resolveMethodReference typeMap (func: MethodReference) =
        let methodTypeMap = getTypeMap(func)
        let resolutionTypeMap = methodTypeMap >> typeMap
        if func.HasThis then
            let declarer = typeMap func.DeclaringType
            let newFunc = MethodReference(func.Name, typeMap func.ReturnType, declarer)
            for arg in func.Parameters do
                let argType = resolutionTypeMap arg.ParameterType
                let newArg = ParameterDefinition(arg.Name, arg.Attributes, argType)
                newFunc.Parameters.Add(arg)
            newFunc.HasThis <- func.HasThis
            newFunc
        else
            func