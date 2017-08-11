module CodeBlock.Instructions

open System

open Mono.Cecil
open Mono.Cecil.Cil

let (|LogicBinary|_|) = function
    | item when item = Code.And || item = Code.Or || item = Code.Xor -> Some(item)
    | _ -> None

let (|Starg|_|) (input: Instruction) =
    match input.OpCode.Code with
    | Code.Starg | Code.Starg_S ->
        warning "starg: this?"
        Some((input.Operand :?> ParameterDefinition).Index + 1)
    | _ -> None
    
let (|Ldarga|_|) (input: Instruction) =
    match input.OpCode.Code with
    | Code.Ldarga | Code.Ldarga_S ->
        warning "ldarga: this?"
        Some((input.Operand :?> ParameterDefinition).Index + 1)
    | _ -> None

let (|Comparison|_|) = function
    | item when item = Code.Ceq || item = Code.Cgt || item = Code.Cgt_Un
        || item = Code.Clt || item = Code.Clt_Un -> Some(item)
    | _ -> None

let (|Arithmetic|_|) value =
    match value with
    | Code.Add | Code.Sub | Code.Div | Code.Mul | Code.Rem -> Some(value)
    | _ -> None

let (|DivUnArithmetic|_|) value =
    match value with
    | Code.Div_Un | Code.Rem_Un -> Some(value)
    | _ -> None

let (|ComparisonBranch|_|) value =
    match value with
    | Code.Beq | Code.Beq_S
    | Code.Bge | Code.Bge_S | Code.Bge_Un | Code.Bge_Un_S
    | Code.Bgt | Code.Bgt_S | Code.Bgt_Un | Code.Bgt_Un_S
    | Code.Ble | Code.Ble_S | Code.Ble_Un | Code.Ble_Un_S
    | Code.Blt | Code.Blt_S | Code.Blt_Un | Code.Blt_Un_S
    | Code.Bne_Un | Code.Bne_Un_S -> Some(value)
    
    | _ -> None

let (|ConditionalBranch|_|) value =
    match value with
    | Code.Beq | Code.Beq_S
    | Code.Bge | Code.Bge_S | Code.Bge_Un | Code.Bge_Un_S
    | Code.Bgt | Code.Bgt_S | Code.Bgt_Un | Code.Bgt_Un_S
    | Code.Ble | Code.Ble_S | Code.Ble_Un | Code.Ble_Un_S
    | Code.Blt | Code.Blt_S | Code.Blt_Un | Code.Blt_Un_S
    | Code.Bne_Un | Code.Bne_Un_S 
    | Code.Brtrue | Code.Brtrue_S
    | Code.Brfalse | Code.Brfalse_S -> Some(value)
    
    | _ -> None


let IsControlFlow = function
    | Code.Beq | Code.Beq_S
    | Code.Bge | Code.Bge_S | Code.Bge_Un | Code.Bge_Un_S
    | Code.Bgt | Code.Bgt_S | Code.Bgt_Un | Code.Bgt_Un_S
    | Code.Ble | Code.Ble_S | Code.Ble_Un | Code.Ble_Un_S
    | Code.Blt | Code.Blt_S | Code.Blt_Un | Code.Blt_Un_S
    | Code.Bne_Un | Code.Bne_Un_S 
    
    | Code.Br | Code.Br_S

    | Code.Brtrue | Code.Brfalse
    | Code.Brtrue_S | Code.Brfalse_S
    
    | Code.Switch

    | Code.Throw | Code.Rethrow

    | Code.Ret -> true
    | _ -> false

let (|Conv4|_|) value =
    match value with
    | Code.Conv_I1 | Code.Conv_I2 | Code.Conv_I4
    | Code.Conv_U1 | Code.Conv_U2 | Code.Conv_U4 -> Some(value)
    | _ -> None

let (|ConvI|_|) value =
    match value with
    | Code.Conv_I -> Some(0, true)
    | Code.Conv_U -> Some(0, false)
    | Code.Conv_I1 -> Some(8, true)
    | Code.Conv_I2 -> Some(16, true)
    | Code.Conv_I4 -> Some(32, true)
    | Code.Conv_I8 -> Some(64, true)
    | Code.Conv_U1 -> Some(8, false)
    | Code.Conv_U2 -> Some(16, false)
    | Code.Conv_U4 -> Some(32, false)
    | Code.Conv_U8 -> Some(64, false)
    | _ -> None

let (|ConvNative|_|) value =
    match value with
    | Code.Conv_I | Code.Conv_U -> Some(value)
    | _ -> None

let (|ConvFloat|_|) value =
    match value with
    | Code.Conv_R_Un | Code.Conv_R4 | Code.Conv_R8 -> Some( value)
    | _ -> None

let (|Shift|_|) value =
    match value with
    | Code.Shl | Code.Shr | Code.Shr_Un -> Some(value)
    | _ -> None
