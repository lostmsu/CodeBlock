namespace CodeBlock.TypeSystem.Cecil

open Mono.Cecil

open CodeBlock.TypeSystem
open CodeBlock.TypeSystem.Common

[<AutoOpen>]
module TypeReferenceHelpers =
    type TypeReference with
        member this.CorLib =
            this.Module.TypeSystem.Object.Module

        member this.ResolveFixed() =
            match this with
            | :? ArrayType as arrayType ->
                let objType = arrayType.Module.TypeSystem.Object
                let corlib = objType.Module
                let arrayTypeRef = TypeReference("System", "Array", corlib, objType.Scope)
                arrayTypeRef.Resolve()

            | _ -> this.Resolve()

        member this.TypeMap =
            let typedef = this.ResolveFixed()

            let typeparams = typedef.GenericParameters
                                        |> Array.ofSeq |> Array.map (fun p -> p.FullName)
            let types = if this :? TypeDefinition then [||]
                        else (this :?> GenericInstanceType).GenericArguments |> Array.ofSeq
            let typeparams = Array.zip typeparams types

            let getParamByName name =
                let _,result = typeparams |> Array.find(fun (n,p) -> n = name)
                result

            let getParam (param: GenericParameter) =
                getParamByName param.FullName

            let instantiate (genericType: TypeReference) =
                if genericType.IsGenericParameter then
                    Some(getParamByName genericType.FullName)
                else
                //let genericType = genericType.Resolve() :> TypeReference
                if genericType.GenericParameters.Count = 0 then
                    None
                else
                let concrete = GenericInstanceType(genericType)
                for param in genericType.GenericParameters do
                    concrete.GenericArguments.Add(getParam param)
                Some(upcast concrete)

            typeMapRec instantiate
