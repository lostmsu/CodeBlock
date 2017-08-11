#light

module CodeBlock.TypeResolver

open System
open System.Linq

open Mono.Cecil

type BaseAssemblyResolver with
    member private this.FindType(name, amodule: ModuleDefinition)=
        amodule.Types.SingleOrDefault(fun t -> t.FullName = name)
    
    member private this.FindType(name, assembly: AssemblyDefinition) =
        let inModules = assembly.Modules |> Seq.map (fun amodule -> this.FindType(name, amodule))
        inModules.Single(fun t -> t <> null)
    
    member this.ResolveType (refer: TypeReference)=
        match refer with
        | null -> raise(ArgumentNullException("refer"))
        | _ when not <| refer.GetType().Equals(typeof<TypeReference>) ->
            refer
        | _ ->
            let assemblyName = refer.Scope :?> AssemblyNameReference
            let assembly = this.Resolve(assemblyName)
            upcast this.FindType(refer.FullName, assembly)
//        match refer with
//        | null -> raise(ArgumentNullException("refer"))
//        | :? TypeDefinition as refer -> refer
////        | :? ArrayType as refer ->
//////            output {    Entry = NotImplementedException("array type resolving");
//////                        Position = null, [] }
////            failwith "fatal error: can't resolve array type"
//        | :? ReferenceType as refer ->
//            CoreReferenceType.Type
//            // this.ResolveType refer.ElementType
//        
//        | _ when refer.GetType().Equals(typeof<TypeReference>) ->
//            
//        
//        | _ ->
////            output {    Entry = NotImplementedException(sprintf "can't resolve type %s" refer.FullName);
////                        Position = null, [] }
//            failwith (sprintf "fatal error: can't resolve type %A of type %A" refer (refer.GetType()))
