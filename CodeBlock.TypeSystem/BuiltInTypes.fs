module CodeBlock.TypeSystem.BuiltIns

open Mono.Cecil

open Lost.FSharp

open CodeBlock.TypeSystem
open CodeBlock.TypeSystem.Common
open CodeBlock.TypeSystem.Cecil

let (|ReferenceType|_|) (value: #TypeReference) =
    if not value.IsValueType then Some(value) else None

let isInt32 (typeref: #TypeReference) = typeref.FullName = "System.Int32"
let isUInt32 (typeref: #TypeReference) = typeref.FullName = "System.UInt32"
let isInt16 (typeref: #TypeReference) = typeref.FullName = "System.Int16"
let isUInt16 (typeref: #TypeReference) = typeref.FullName = "System.UInt16"
let isByte (typeref: #TypeReference) = typeref.FullName = "System.Byte"
let isSByte (typeref: #TypeReference) = typeref.FullName = "System.SByte"
let isInt64 (typeref: #TypeReference) = typeref.FullName = "System.Int64"
let isUInt64 (typeref: #TypeReference) = typeref.FullName = "System.UInt64"
let isVoid (typeref: #TypeReference) = typeref.FullName = "System.Void"
let isIntPtr (typeref: #TypeReference) = typeref.FullName = "System.IntPtr"
let isUIntPtr (typeref: #TypeReference) = typeref.FullName = "System.UIntPtr"
let isBool (typeref: #TypeReference) = typeref.FullName = "System.Boolean"
let isObject (typeref: #TypeReference) = typeref.FullName = "System.Object"
let isChar (typeref: #TypeReference) = typeref.FullName = "System.Char"
let isSingle (typeref: #TypeReference) = typeref.FullName = "System.Single"
let isDouble (typeref: #TypeReference) = typeref.FullName = "System.Double"

let isNullable (typeref: #TypeReference) =
    typeref.FullName = "System.Nullable<'1>"
let isEnum (typeRef: #TypeReference) =
    let typeDef = typeRef.ResolveFixed()
    notNull typeDef && typeDef.IsEnum

let (|NullableType|_|) (value : #TypeReference): TypeReference option =
    if not(isNullable value) then None
    else
    raise <| System.NotImplementedException()
