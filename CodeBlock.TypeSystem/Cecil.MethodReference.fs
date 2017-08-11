namespace CodeBlock.TypeSystem.Cecil

open Mono.Cecil

open CodeBlock.TypeSystem
open CodeBlock.TypeSystem.Common

[<AutoOpen>]
module MethodReferenceHelpers =
    type MethodReference with
        member this.ActualParameters =
            let paramTypes =
                this.Parameters
                |> Seq.map (fun param -> param.ParameterType)
                |> List.ofSeq
            if this.HasThis then
                this.DeclaringType :: paramTypes
            else
                paramTypes

        member this.ResolveGenerics(hlType: TypeReference) =
            let getParam(param: TypeReference) =
                    if param.FullName.StartsWith "!!" then
                        let index = int <| param.FullName.Substring(2)
                        match this with
                        | :? GenericInstanceMethod as generic ->
                            generic.GenericArguments.[index]
                        | _ ->
                            let genericT = this.DeclaringType :?> GenericInstanceType
                            genericT.GenericArguments.[index]
                    elif param.FullName.StartsWith "!" then
                        let index = int <| param.FullName.Substring(1)
                        match this.DeclaringType with
                        | :? GenericInstanceType as generic ->
                            generic.GenericArguments.[index]
                        | _ ->
                            assert(false)
                            param
                    else
                        assert(not <| param.FullName.StartsWith "!")
                        param

            match hlType with
            | :? GenericInstanceType as argtype ->
                let typedef = argtype.Resolve()
                let result = GenericInstanceType(typedef)
                for param in argtype.GenericArguments do
                    result.GenericArguments.Add(getParam param)
                result :> TypeReference

            | :? GenericParameter as argtype ->
                getParam argtype

            | :? ByReferenceType as byref ->
                upcast ByReferenceType(this.ResolveGenerics byref.ElementType)

            | :? Mono.Cecil.PointerType as ptr ->
                upcast Mono.Cecil.PointerType(this.ResolveGenerics ptr.ElementType)

            | _ -> hlType

        member this.ActualParametersResolved =
            this.ActualParameters
            |> List.map this.ResolveGenerics

        member this.ActualReturnType =
            this.ResolveGenerics(this.ReturnType)
