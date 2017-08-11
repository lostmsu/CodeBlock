namespace CodeBlock.TypeSystem.Cecil

open Mono.Cecil

open CodeBlock.TypeSystem.Common

[<AutoOpen>]
module TypeDefinitionHelpers =
    type TypeDefinition with
        /// Looks for a field with specified name
        member this.Field(name) =
            this.Fields
            |> Seq.find(fun f -> f.Name = name)

        /// Looks for a method with specified name
        member this.Method(name) =
            this.Methods
            |> Seq.find(fun f -> f.Name = name)

    type System.Type with
        /// Gets Cecil definition for the assembly
        member this.Definition =
            let clrAssembly = this.Assembly
            let assembly =
                try
                    defaultResolver.Resolve clrAssembly.FullName
                with
                    | :? AssemblyResolutionException ->
                        clrAssembly.Definition
            let typeName =
                if this.IsGenericType then
                    this.GetGenericTypeDefinition().FullName
                else this.FullName
            assembly.AllTypes |> Seq.find (fun t -> t.FullName = typeName)

    open System

    open CodeBlock

    let private i32 = typeof<int>.Definition
    let private i64 = typeof<int64>.Definition
    let private native = typeof<System.IntPtr>.Definition
    let private double = typeof<System.Double>.Definition

    /// Looks for a member with specified name
    let findByName memberName (members: seq<#MemberReference>) =
        members |> Seq.find(fun m -> m.Name = memberName)

    let GetTypeReference = function
        | Object(t) -> t
        | I32 -> upcast i32
        | I64 -> upcast i64
        | Native -> upcast native
        | Float -> upcast double
        | Reference(t) -> null