namespace CodeBlock.TypeSystem

open Mono.Cecil

open CodeBlock.TypeSystem.Common

[<AutoOpen>]
module TypeMaps =
    let rec typeMapRec typeMap (typeRef: TypeReference) =
        System.Runtime.CompilerServices.RuntimeHelpers.EnsureSufficientExecutionStack()
        match typeMap typeRef with
        | Some mapped when mapped = typeRef ->
            //RuntimeUtils.notImplemented "typeMap is not implemented correctly"
            mapped
        | Some mapped ->
            typeMapRec typeMap mapped
        | None ->
            match typeRef with
            | :? GenericInstanceType as argtype ->
                let typedef = argtype.Resolve()
                let result = GenericInstanceType(typedef)
                for param in argtype.GenericArguments do
                    result.GenericArguments.Add(typeMapRec typeMap param)
                result :> TypeReference

            | :? GenericParameter as argtype ->
                upcast argtype

            | :? ByReferenceType as byref ->
                upcast ByReferenceType(typeMapRec typeMap byref.ElementType)

            | :? Mono.Cecil.PointerType as ptr ->
                upcast Mono.Cecil.PointerType(typeMapRec typeMap ptr.ElementType)
            
            | :? Mono.Cecil.ArrayType as arr ->
                let elemType = typeMapRec typeMap arr.ElementType
                upcast Mono.Cecil.ArrayType(elemType, arr.Rank)

            | _ when typeRef.GenericParameters.Count > 0 ->
                let result = GenericInstanceType(typeRef)
                for param in typeRef.GenericParameters do
                    result.GenericArguments.Add(typeMapRec typeMap param)
                result :> TypeReference

            | _ -> typeRef
