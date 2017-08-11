namespace CodeBlock

open System

type TranslationException(message, cause: exn) =
    inherit Exception(message, cause)

type TypeTranslationException(type': Mono.Cecil.TypeReference, cause) =
    inherit TranslationException(sprintf "Cannot translate type %O" type', cause)

type MethodTranslationException(method', cause) =
    inherit TranslationException(sprintf "Cannot translate function '%O'" method', cause)

type TranslatorAssertionFailed(message: string, cause: exn) =
    inherit TranslationException(message, cause)

    new (message) = TranslatorAssertionFailed(message, null)

[<AutoOpen>]
module ExceptionHelpers =
    let inline typeTranslationFailed type' reason =
        TypeTranslationException(type', reason)
        |> raise