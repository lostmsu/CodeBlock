module CodeBlock.HighLevel

open System
open System.Linq
open System.Diagnostics
open System.Collections.Generic

open Mono.Cecil
open Mono.Cecil.Cil
open ExecutionStack

open RuntimeUtils

open ExecutionStack.StackItems
open Instructions

//type VerificationViolationException = Exception
//type CorrectnessViolationException = Exception
//
//type TranslatorState = {
//        Original: MethodDefinition;
//        Result: MethodDefinition;
//        Next: Instruction;
//        Stack: StackItem list;
//    }
//
//let translate state =
//    if state.Next = null then InvalidOperationException() |> raise
//
//    let instr = state.Next
//
//    match instr.OpCode.Code, state.Stack with
//    // arithmetic
//    | Code.Add, I32 :: I32 :: stack ->
//        { state with
//            Next = instr.Next;
//            Stack = I32 :: stack;   }
//    | Code.Add, I32 :: Native :: stack ->
//        
//    | _ ->
//        InvalidProgramException("unknown instruction") |> raise

type HighLevel()=
    let instantiate genericType typeParameter =
        // TODO: instantiate
        genericType
    
    let sizeof valueType =
        // TODO: sizeof
        8
    
    let nop = []
    
//    let getOverloads func (typedef: TypeDefinition) =
//        Seq.cast<MethodDefinition> typedef.Methods
//        |> Seq.filter(fun f -> f.Name = func)
//    let getStaticMethods (typedef: TypeDefinition) =
//        Seq.cast<MethodDefinition> typedef.Methods
//        |> Seq.filter(fun f -> f.IsStatic)
    
    member this.Morf (hltype: TypeReference): TypeDefinition=
        match hltype with
        | _ when hltype.FullName = "System.Object" ->
            let result = TypeDefinition(hltype.Name, hltype.Namespace,
                            TypeAttributes.AutoLayout ||| TypeAttributes.Public, null)
            
            let vmt = FieldDefinition(".VMT", TypeReference("IntPtr", "System", null, true),
                        FieldAttributes.Private ||| FieldAttributes.RTSpecialName ||| FieldAttributes.SpecialName)
            let lock = FieldDefinition(".LOCK", TypeReference("IntPtr", "System", null, true),
                        FieldAttributes.Private ||| FieldAttributes.RTSpecialName ||| FieldAttributes.SpecialName)
            result.Fields.Add vmt
            result.Fields.Add lock
            
            result
        
        | _ when hltype.IsValueType ->
            downcast hltype
        
        | :? TypeDefinition ->
            let hltype = hltype :?> TypeDefinition
            if hltype.IsClass then
                let result = TypeDefinition(hltype.Name, hltype.Namespace,
                                TypeAttributes.AutoLayout ||| TypeAttributes.Public, null)
                
                result.IsValueType <- true
                
                let baseType = FieldDefinition(".Base", hltype.BaseType,
                                FieldAttributes.Public ||| FieldAttributes.RTSpecialName ||| FieldAttributes.SpecialName)
                result.Fields.Add baseType
                
                for field in hltype.Fields |> Seq.cast<FieldDefinition> |> Seq.filter (fun f -> not f.IsStatic) do
                    result.Fields.Add(field.Clone())
                
                result
            
            elif hltype.IsInterface then
                hltype
            
            else
                not_implemented "type morfing" |> raise
            
        | _ ->
            not_implemented "type morfing" |> raise
    
    member this.Morf (hlmethod: MethodDefinition)=
        Debug.Print(sprintf "morfing %s..." hlmethod.Name)
        let result = MethodDefinition(hlmethod.Name, MethodAttributes.Static, this.Morf hlmethod.ReturnType.ReturnType)
        
        if hlmethod.HasThis then
            let thisParam = ParameterDefinition(this.Morf hlmethod.DeclaringType)
            // TODO make a reference
            result.Parameters.Add thisParam
        
        for hlparam in hlmethod.Parameters do
            let param = ParameterDefinition(this.Morf hlparam.ParameterType)
            param.Name <- hlparam.Name
            param.IsIn <- hlparam.IsIn
            param.IsOut <- hlparam.IsOut
            param.IsOptional <- hlparam.IsOptional
            result.Parameters.Add(param)
        
        for hlvar in hlmethod.Body.Variables do
            let var = VariableDefinition(this.Morf hlvar.VariableType)
            var.Index <- hlvar.Index
            var.Name <- hlvar.Name
            result.Body.Variables.Add(var)
        
        let cil = result.Body.CilWorker
        let emit = cil.Emit >> ignore
        
        let translate stack (instr: Instruction) =
            if instr = null then ArgumentNullException("instr") |> raise
            
            // helpers
            let ldarg num =
                NotImplementedException() |> raise
            let ldloc num =
                NotImplementedException() |> raise
            let stloc num =
                NotImplementedException() |> raise

            let ldc4 value =
                NotImplementedException() |> raise
            // helpers

            match instr.OpCode.Code, stack with
            // arithmetics
            | Arithmetic(op), IsNumeric(a) :: IsNumeric(b) :: stack when a = b->
                emit(op)
                a :: stack, true
            | Arithmetic(op), NativeMix(stack) ->
                emit(op)
                Native :: stack, true
            
            | Code.Add, I32 :: Reference(t) :: stack | Code.Add, Native :: Reference(t) :: stack 
            | Code.Add, Reference(t) :: I32 :: stack | Code.Add, Reference(t) :: Native :: stack ->
                emit(OpCodes.Add)
                Reference(t) :: stack, false
            
            | Code.Sub, Reference(t) :: I32 :: stack | Code.Sub, Reference(t) :: Native :: stack ->
                emit(OpCodes.Sub)
                Reference(t) :: stack, false
            | Code.Sub, Reference(_) :: Reference(_) :: stack ->
                emit(OpCodes.Sub)
                // ???
                Native :: stack, true

            | Arithmetic(op), _ ->
                InvalidProgramException() |> raise
            
            | DivUnArithmetic(op), IsInteger(a) :: IsInteger(b) :: stack when a = b->
                emit(op)
                a :: stack, true
            | DivUnArithmetic(op), NativeMix(stack) ->
                emit(op)
                Native :: stack, true
            | DivUnArithmetic(op), _ ->
                InvalidProgramException() |> raise


            | Code.Neg, IsNumeric(v) :: stack ->
                emit(OpCodes.Neg)
                v :: stack, true
            | Code.Neg, _ ->
                InvalidProgramException() |> raise
            // arithmetics

            // logics
            | LogicBinary(op), IsInteger(a) :: IsInteger(b) :: stack when a = b->
                emit(op)
                a :: stack, true
            | LogicBinary(op), NativeMix(stack) ->
                emit(op)
                Native :: stack, true
            | LogicBinary(op), _ ->
                InvalidProgramException() |> raise

            | Code.Not, IsInteger(a) :: stack ->
                emit(OpCodes.Not)
                a :: stack, true
            | Code.Not, _ ->
                InvalidProgramException() |> raise
            // logics

            // bits
            | Shift(op), Native :: IsInteger(v) :: stack
            | Shift(op), I32 :: IsInteger(v) :: stack ->
                emit(op)
                v :: stack, true
            | Shift(op), _ ->
                InvalidProgramException() |> raise
            // bits

            // compare
            | Comparison(op), IsNumeric(a) :: IsNumeric(b) :: stack when a = b ->
                emit op
                I32 :: stack, true
            | Comparison(op), Reference(_) :: Reference(_) :: stack ->
                emit op
                I32 :: stack, true
            | Comparison(op), NativeMix(stack) ->
                emit op
                I32 :: stack, true
            | Code.Ceq, Object(_) :: Object(_) :: stack ->
                emit OpCodes.Ceq
                I32 :: stack, true
            | Code.Cgt_Un, Object(_) :: Object(_) :: stack ->
                emit OpCodes.Cgt_Un
                I32 :: stack, true
            | Code.Ceq, Native :: Object(_) :: stack
            | Code.Ceq, Object(_) :: Native :: stack ->
                emit OpCodes.Ceq
                // ???
                I32 :: stack, true
            | Comparison(op), _ ->
                InvalidProgramException() |> raise
            // compare

            // branches
            | ConditionalBranch(op), IsNumeric(a) :: IsNumeric(b) :: stack when a = b ->
                NotImplementedException() |> raise
            | ConditionalBranch(op), Reference(_) :: Reference(_) :: stack ->
                NotImplementedException() |> raise
            | ConditionalBranch(op), NativeMix(stack) ->
                NotImplementedException() |> raise
            | Code.Beq, Object(_) :: Object(_) :: stack
            | Code.Beq_S, Object(_) :: Object(_) :: stack ->
                NotImplementedException() |> raise
            | Code.Beq, Native :: Reference(_) :: stack
            | Code.Beq, Native :: Reference(_) :: stack
            | Code.Beq_S, Reference(_) :: Native :: stack
            | Code.Beq_S, Reference(_) :: Native :: stack ->
                NotImplementedException() |> raise
            | ConditionalBranch(op), _ ->
                InvalidProgramException() |> raise

            | Code.Br, stack | Code.Br_S, stack ->
                NotImplementedException() |> raise

            | Code.Brfalse, CanBeZero(a) :: stack
            | Code.Brfalse_S, CanBeZero(a) :: stack
            | Code.Brtrue, CanBeZero(a) :: stack
            | Code.Brtrue_S, CanBeZero(a) :: stack ->
                NotImplementedException() |> raise
            
            | Code.Brfalse, _ | Code.Brfalse_S, _
            | Code.Brtrue, _ | Code.Brtrue_S, _ ->
                InvalidProgramException() |> raise
            // branches

            | Code.Break, stack ->
                emit(OpCodes.Break)
                stack, true

            | Code.Call, stack ->
                NotImplementedException() |> raise

            | Code.Calli, stack ->
                NotImplementedException() |> raise

            | Code.Ckfinite, Float :: stack ->
                emit(OpCodes.Ckfinite)
                Float :: stack, true
            | Code.Ckfinite, _ ->
                InvalidProgramException() |> raise

            // conv
            | Conv4(op), IsNumeric(_) :: stack ->
                emit(op)
                I32 :: stack, true
            | ConvNative(op), IsNumeric(_) :: stack ->
                emit(op)
                Native :: stack, true
            | Code.Conv_I8, IsNumeric(_) :: stack
            | Code.Conv_U8, IsNumeric(_) :: stack ->
                emit(OpCodes.Conv_I8)
                I64 :: stack, true
            | ConvFloat(op), IsNumeric(_) :: stack ->
                emit(op)
                Float :: stack, true
            // conv
            
            | Code.Localloc, Native :: stack
            | Code.Localloc, I32 :: stack ->
                emit(OpCodes.Localloc)
                Native :: stack, false
            
            | Code.Cpblk, I32 :: IsPointer(s) :: IsPointer(d) :: stack ->
                emit(OpCodes.Cpblk)
                stack, false
            
            | Code.Initblk, I32 :: I32 :: IsPointer(d) :: stack ->
                emit(OpCodes.Initblk)
                stack, false
            
            | Code.Dup, some :: stack ->
                emit(OpCodes.Dup)
                some :: some :: stack, true
            | Code.Dup, [] ->
                InvalidProgramException() |> raise
            
            | Code.Pop, some :: stack ->
                emit(OpCodes.Pop)
                stack, true
            | Code.Pop, [] ->
                InvalidProgramException() |> raise
            
            | Code.Jmp, [] ->
                NotImplementedException() |> raise
            
            | Code.Jmp, _ ->
                InvalidProgramException() |> raise
            
            | Code.Ldarg, _ | Code.Ldarg_S, _ ->
                Convert.ToInt32(instr.Operand) |> ldarg
            | Code.Ldarg_0, _ | Code.Ldarg_1, _ | Code.Ldarg_2, _ | Code.Ldarg_3, _ ->
                instr.OpCode.Code - Code.Ldarg_0 |> ldarg
            
            | Code.Starg, _ | Code.Starg_S, _ ->
                NotImplementedException() |> raise
            
            | Code.Ldloc, _ | Code.Ldloc_S, _ ->
                Convert.ToInt32 instr.Operand |> ldloc
            | Code.Ldloc_0, _ | Code.Ldloc_1, _ | Code.Ldloc_2, _ | Code.Ldloc_3, _ ->
                instr.OpCode.Code - Code.Ldloc_0 |> ldloc
            
            | Code.Stloc, item :: stack | Code.Stloc_S, item :: stack ->
                Convert.ToInt32 instr.Operand |> stloc item
            | Code.Stloc_0, item :: stack | Code.Stloc_1, item :: stack
            | Code.Stloc_2, item :: stack | Code.Stloc_3, item :: stack ->
                instr.OpCode.Code - Code.Stloc_0 |> stloc item
            
            | Code.Ldarga, _ | Code.Ldarga_S, _ ->
                let num = Convert.ToInt32(instr.Operand)
                NotImplementedException() |> raise
            
            | Code.Ldloca, _ | Code.Ldloca_S, _ ->
                let num = Convert.ToInt32 instr.Operand
                NotImplementedException() |> raise
            
            // loading constants
            | Code.Ldc_I4_0, _ | Code.Ldc_I4_1, _ | Code.Ldc_I4_2, _ | Code.Ldc_I4_3, _
            | Code.Ldc_I4_4, _ | Code.Ldc_I4_5, _ | Code.Ldc_I4_6, _ | Code.Ldc_I4_7, _
            | Code.Ldc_I4_8, _ ->
                instr.OpCode.Code - Code.Ldc_I4_0 |> ldc4
            
            | Code.Ldc_I4_M1, _ ->
                ldc4 -1
            
            | Code.Ldc_I4, _ | Code.Ldc_I4_S, _ ->
                Convert.ToInt32(instr.Operand) |> ldc4
            
            | Code.Ldc_R4, _ ->
                cil.Emit(OpCodes.Ldc_R4, Convert.ToSingle instr.Operand) |> ignore
                Float :: stack, true
            
            | Code.Ldc_R8, _ ->
                cil.Emit(OpCodes.Ldc_R8, Convert.ToDouble instr.Operand) |> ignore
                Float :: stack, true
            
            | Code.Ldc_I8, _ ->
                cil.Emit(OpCodes.Ldc_I8, Convert.ToInt64 instr.Operand) |> ignore
                I64 :: stack, true
            
            | Code.Ldnull, _ ->
                emit(OpCodes.Ldnull)
                StackItem.Object(null) :: stack, true
            // loading constants
            
            | Code.Ldftn, _ ->
                NotImplementedException() |> raise
            
            | Code.Ldind_I, Reference(t) :: stack ->
                NotImplementedException() |> raise
            | Code.Ldind_I1, Reference(t) :: stack
            | Code.Ldind_U1, Reference(t) :: stack ->
                NotImplementedException() |> raise
            | Code.Ldind_I2, Reference(t) :: stack
            | Code.Ldind_U2, Reference(t) :: stack ->
                NotImplementedException() |> raise
            | Code.Ldind_I4, Reference(t) :: stack
            | Code.Ldind_U4, Reference(t) :: stack ->
                NotImplementedException() |> raise
            | Code.Ldind_I8, Reference(t) :: stack ->
                NotImplementedException() |> raise
            | Code.Ldind_R4, Reference(t) :: stack ->
                NotImplementedException() |> raise
            | Code.Ldind_R8, Reference(t) :: stack ->
                NotImplementedException() |> raise
            | Code.Ldind_Ref, Reference(t) :: stack ->
                NotImplementedException() |> raise

            | Code.Stind_I, Native :: Reference(t) :: stack ->
                NotImplementedException() |> raise
            | Code.Stind_I1, I32 :: Reference(t) :: stack
            | Code.Stind_I2, I32 :: Reference(t) :: stack
            | Code.Stind_I4, I32 :: Reference(t) :: stack ->
                NotImplementedException() |> raise
            | Code.Stind_I8, I64 :: Reference(t) :: stack ->
                NotImplementedException() |> raise
            | Code.Stind_Ref, Object(a) :: Reference(t) :: stack ->
                NotImplementedException() |> raise
            | Code.Stind_R4, Float :: Reference(t) :: stack
            | Code.Stind_R8, Float :: Reference(t) :: stack ->
                NotImplementedException() |> raise
            
            | Code.Nop, _ ->
                emit(OpCodes.Nop)
                stack, true
            
            | Code.Ret, _ ->
                emit(OpCodes.Ret)
                NotImplementedException() |> raise
            
            | Code.Switch, I32 :: stack ->
                NotImplementedException() |> raise
            
            
            | _ ->
                InvalidProgramException() |> raise
        
        let vtranslate (stack, verifiable) instr =
            let stack, verstatus = translate stack instr
            stack, verstatus && verifiable

        Seq.cast<Instruction> hlmethod.Body.Instructions
        |> Seq.fold vtranslate ([], true)
