namespace CodeBlock

open System

open Mono.Cecil
open Mono.Cecil.Cil

type StackItem = 
    | I32
    | I64
    | Native
    | Float
    | Reference of TypeReference
    | Object of TypeReference

module ExecutionStack =
    let GetStackItem (ciltype: TypeReference) =
        warning "stack type detected by type name"
        match ciltype with
        | _ when ciltype.FullName = "System.Int32" || ciltype.FullName = "System.UInt32"-> I32
        | _ when ciltype.FullName = "System.Int16" || ciltype.FullName = "System.UInt16"-> I32
        | _ when ciltype.FullName = "System.Byte" || ciltype.FullName = "System.SByte"-> I32
        | _ when ciltype.FullName = "System.Boolean"-> I32
        | _ when ciltype.FullName = "System.Int64" || ciltype.FullName = "System.UInt64"-> I64
        | _ when ciltype.FullName = "System.IntPtr" || ciltype.FullName = "System.UIntPtr" -> Native
        | :? ByReferenceType as ciltype ->
            Reference(ciltype.ElementType)
        | _ -> Object(ciltype)
    
    module StackItems =
        let (|IsInteger|_|) input =
            match input with
            | I32 | I64 | Native -> Some(input)
            | _ -> None

        let (|IsIndex|_|) input =
            match input with
            | I32 | Native -> Some(input)
            | _ -> None

        let (|IsBig|_|) input = 
            match input with
            | I64 | Native | Reference(_) -> Some(input)
            | _ -> None

        let (|IsNumeric|_|) input =
            match input with
            | IsInteger i -> Some(i)
            | Float -> Some(Float)
            | _ -> None

        let (|IsPointer|_|) input =
            match input with
            | Reference(_) | Native -> Some(input)
            | _ -> None

        let (|CanBeZero|_|) value =
            match value with
            | I32 | I64 | Native | Object(_) | Reference(_) -> Some(value)
            | _ -> None

        let NativeMix a b =
            match a, b with
            | Native, I32 | I32, Native -> true
            | _ -> false


    open StackItems

    let intbinary = function
        | I32 :: I32 :: stack ->
            Some(I32 :: stack)
        | I32 :: Native :: stack | Native :: I32 :: stack ->
            Some(Native :: stack)
        | I64 :: I64 :: stack ->
            Some(I64 :: stack)
        | _ ->
            None

    let private comparison result = function
        | a :: b :: stack when a = b ->
            Some(result @ stack)
        | I32 :: Native :: stack | Native :: I32 :: stack ->
            Some(result @ stack)
        | Native :: Reference(t) :: stack | Reference(t) :: Native :: stack ->
            Some(result @ stack)
        | _ ->
            None

    let private boolUnary result = function
        | I32 :: stack | I64 :: stack | Native :: stack -> Some(result @ stack)
        | Reference(t) :: stack | Object(t) :: stack -> Some(result @ stack)
        | _ -> None

    let private conv result = function
        | I32 :: stack | I64 :: stack | Native :: stack | Float :: stack -> Some(result :: stack)
        | _ -> None

    let private binaryNumeric = function
        | value when intbinary value = None ->
            match value with
            | Float :: Float :: stack -> Some(Float :: stack)
            | _ -> None
        | value -> intbinary value

    let private unaryInteger = function
        | IsInteger(a) :: stack  -> Some(a :: stack)
        | _ -> None

    let private unaryNumeric = function
        | IsNumeric(x) :: stack -> Some(x :: stack)
        | _ -> None

    let private ldind result = function
        | Native :: stack | Reference(_) :: stack -> Some(result :: stack)
        | _ -> None

    let private removeTop = function
        | [] -> None
        | x :: stack -> Some(stack)

    let private stind arg = function
        | value :: IsPointer _ :: stack when value = arg -> Some(stack)
        | _ -> None
