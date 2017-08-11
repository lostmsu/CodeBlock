namespace CodeBlock

open System
open System.Diagnostics
open System.Linq

open Mono.Cecil

[<AutoOpen>]
module public Utils =
    let private warned = System.Collections.Generic.HashSet<string>()

    let warning message =
        Printf.kprintf
            (fun message ->
                if warned.Add(message) then
                    Debug.Print("warning: " + message))
            message
    
    type Warnings =
        static member NoVerification subj =
            warning "%s: no verification" subj
        static member NoTypeMixing subj =
            warning "%s: no type mixing" subj

    type Mono.Cecil.Cil.Instruction with
        member this.ILasm =
            if this.Operand <> null then
                sprintf "%A %A" this.OpCode.Code this.Operand
            else
                sprintf "%A" this.OpCode.Code

    let make_value opt value =
        match opt with
        | Some(value) -> value
        | None -> value
    
    let inline bound value low high name =
        if (value < low) || (value > high) then raise(ArgumentOutOfRangeException(name))
    
    let inline int_size value =
        let value = int64(value)
        if value = int64(sbyte(value)) then 1
        elif value = int64(int16(value)) then 2
        elif value = int64(int(value)) then 4
        else 8
    
    let inline uint_size value =
        let value = uint64(value)
        if value = uint64(byte(value)) then 1
        elif value = uint64(uint16(value)) then 2
        elif value = uint64(uint32(value)) then 4
        else 8

    let fpow times func =
        let rec fpow times result =
            if times = 1 then
                fun value -> func (result value)
            elif times = 0 then
                fun value -> value
            elif times < 0 then
                ArgumentOutOfRangeException() |> raise
            else
                fpow (times - 1) (fun value -> func (result value))

        fpow times (fun value -> value)

    let match_fail() =
        InvalidOperationException("match failure") |> raise

