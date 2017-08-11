module CodeBlock.TypeSystem

open Mono.Cecil

let (|Prime|_|) value =
    let composite = List.exists (fun n -> value % n = 0) [2 .. int (sqrt (float value))]
    if not composite then Some(value)
    else None

let typeEquals (type1: #TypeReference) (type2: #TypeReference) = type1.FullName = type2.FullName

let InstantiateGeneric genericType parameters =
    genericType

// assignment compatibility
let (!=) target source =
    target = source

let rec isBasedOn (baseType: TypeDefinition) (thisType: TypeDefinition) =
    if thisType = baseType then true
    else
        let parent = thisType.BaseType
        if parent <> null then isBasedOn baseType (parent.Resolve())
        else false

