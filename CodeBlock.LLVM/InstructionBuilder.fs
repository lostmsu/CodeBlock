module CodeBlock.InstructionBuilderExtensions

open System.Linq

open LLVM

open CodeBlock
open RuntimeUtils


type InstructionBuilder with
    member this.add = function a :: b :: rest -> this.Add(b, a, "add") :: rest | _ -> invalid_code "invalid stack state on add" |> raise
    member this.sub = function a :: b :: rest -> this.Subtract(b, a, "sub") :: rest | _ -> invalid_code "invalid stack state on sub" |> raise
    member this.mul = function a :: b :: rest -> this.Multiply(b, a, "mul") :: rest | _ -> invalid_code "invalid stack state on mul" |> raise
    member this.div signed = function a :: b :: rest -> this.Divide(signed, b, a, "div") :: rest | _ -> invalid_code "invalid stack state on div" |> raise
    member this.rem signed = function a :: b :: rest -> this.Reminder(signed, b, a, "rem") :: rest | _ -> invalid_code "invalid stack state on rem" |> raise
    member this.neg = function value :: rest -> this.Negate(value, "neg") :: rest | _ -> invalid_code "invalid stack state on neg" |> raise

    member this.``and`` = function a :: b :: rest -> this.And(b, a, "and") :: rest | _ -> invalid_code "invalid stack state on and" |> raise
    member this.``or`` = function a :: b :: rest -> this.Or(b, a, "or") :: rest | _ -> invalid_code "invalid stack state on or" |> raise
    member this.xor = function a :: b :: rest -> this.Xor(b, a, "xor") :: rest | _ -> invalid_code "invalid stack state on xor" |> raise
    member this.not = function value :: rest -> this.Not(value, "not") :: rest | _ -> invalid_code "invalid stack state on not" |> raise

    member this.shl = function a :: b :: rest -> this.ShiftLeft(b, a, "") :: rest | _ -> invalid_code "invalid stack state on shl" |> raise
    member this.shr signed = function a :: b :: rest -> this.ShiftRight(signed, b, a, "") :: rest | _ -> invalid_code "invalid stack state on shr" |> raise

    member this.``if`` ontrue onfalse = 
        function
          | cond :: rest ->
            this.If(cond, ontrue, onfalse) :> Value |> ignore
            rest
          | _ -> invalid_code "invalid stack state on if" |> raise
    member this.goto target =
        function 
        stack ->
            this.GoTo(target) |> ignore
            []

    member this.extract fid =
        function
          | obj :: rest -> this.Extract(obj, fid, "") :: rest
          | _ -> invalid_code "invalid stack state on extract" |> raise


