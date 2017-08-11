namespace CodeBlock

open Mono.Cecil

open LLVM

type InstructionTranslationException(instruction: Cil.Instruction, message) =
    inherit TranslationException(message, null)

    member this.Instruction = instruction

type InvalidInstructionArgumentsException(instruction: Cil.Instruction) =
    inherit InstructionTranslationException(instruction, "")

type LoweringFailedException(instruction: Cil.Instruction, message) =
    inherit InstructionTranslationException(instruction, message)

type TranslationState =
    {   
        /// Represents state of managed stack
        Stack: StackItem list;
        /// Represents state of native stack
        NativeStack: Value list;
        /// List of translation errors occured so far
        Errors: TranslationException list;
        /// True if so far code was verifiable
        Verifiable: bool   
    }

    /// Adds the error to the translation state
    member this.Error(exn) = { this with Errors = exn :: this.Errors }

type TranslationContext =
    {
        /// Type mapping
        TypeMap: TypeReference -> TypeReference
        /// LLVM context
        NativeContext: LLVM.Context
        /// Lower level translator
        ParentTranslator: IInstructionTranslator
        /// CIL method definition
        Method: MethodDefinition
    }

and IInstructionTranslator =
    abstract Translate: TranslationContext -> TranslationState -> Cil.Instruction -> TranslationState option

module Translation =
    let invalidArgs instruction = InvalidInstructionArgumentsException(instruction) :> TranslationException