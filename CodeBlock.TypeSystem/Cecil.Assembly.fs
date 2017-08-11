namespace CodeBlock.TypeSystem.Cecil

open Mono.Cecil

[<AutoOpen>]
module AssemblyHelpers =
    type System.Reflection.Assembly with
        member this.Definition =
            let path = System.Uri(this.CodeBase).LocalPath
            AssemblyDefinition.ReadAssembly(path)

    type AssemblyDefinition with
        member this.AllTypes =
            this.Modules
            |> Seq.collect (fun mdef -> mdef.Types)

        member this.GetType(reference: #TypeReference) =
            let result =
                this.AllTypes
                |> Seq.tryFind(fun t -> reference.FullName = t.FullName)
            match result with
            | None ->
                System.Collections.Generic.KeyNotFoundException(
                    sprintf "Type %O was not found in %O" reference this)
                |> raise
            | Some(value) -> value

        member this.GetType<'a>() =
            let selectTypes (mdef: ModuleDefinition) =
                mdef.Types

            this.Modules
            |> Seq.collect selectTypes
            |> Seq.find(fun t -> t.FullName = typeof<'a>.FullName)
