#light

namespace CodeBlock

open System
open System.Linq
open System.Reflection

open Mono.Cecil

module RuntimeUtils=
    let void_type = typeof<System.Void>
    
    let inline not_implemented feature = NotImplementedException(feature)
    
    let invalid_code message = InvalidProgramException(message)

    let notSupported message = NotSupportedException(message) |> raise
    
    let inline notImplemented message = NotImplementedException(message) |> raise
    
    let bytes_to_ushort b0 b1 =
        let result= uint16(b0)
        let result = result ||| (uint16(b1) <<< 8)
        result
    
    let bytes_to_int b0 b1 b2 b3 =
        let result = int(b0)
        let result = result ||| (int(b1) <<< 8)
        let result = result ||| (int(b2) <<< 16)
        let result = result ||| (int(b3) <<< 24)
        result
    
    let bytes_to_long b0 b1 b2 b3 b4 b5 b6 b7 =
        let low = int64(bytes_to_int b0 b1 b2 b3)
        let hi = int64(bytes_to_int b4 b5 b6 b7)
        low ||| (hi <<< 32)
    
    let rec byte_arr_to_str (arr: byte list) =
        match arr with
        | byte :: tail -> byte.ToString("X2") + (byte_arr_to_str tail)
        | _ -> ""
    
    let int_type = "System.Int32"
    let uint_type = "System.UInt32"
    let float_type = "System.Single"
    let double_type = "System.Double"
    let long_type = "System.Int64"
    let ulong_type = "System.UInt64"
    let byte_type = "System.Byte"
    let sbyte_type = "System.SByte"
    let short_type = "System.Int16"
    let ushort_type = "System.UInt16"
    let native_type = "System.IntPtr"
    let unative_type = "System.UIntPtr"
    let bool_type = "System.Boolean"
    
    let size_align alignment oldsize =
        ((oldsize + alignment - 1u) / alignment) * alignment
    
    let align alignment value =
        match alignment with
        | 0u ->
            raise(ArgumentOutOfRangeException("alignment", "Alignment must be greater than zero"))
        | 1u -> value
        | _ ->
            ((alignment + value - 1u) / alignment) * alignment

//    type MethodReference with
//        member this.Reflected =
//            let declaringTypeName = this.DeclaringType.FullName
//            let t = System.Type.GetType(declaringTypeName)
//            let typeEquals reflected (cecil: #TypeReference) =
//                reflected = System.Type.GetType(cecil.FullName)
//            let methods = t.GetMethods()
//            methods
//            |> Seq.find (fun func ->
//                if func.Name <> this.Name then false
//                else
//                if func.GetParameters().Length <> this.Parameters.Count then false
//                else
//                let rparamTypes = 
//                    func.GetParameters()
//                    |> Seq.map (fun param -> param.ParameterType)
//                let cparamTypes =
//                    this.Parameters |> Seq.map (fun param -> param.ParameterType)
//                if not <| Seq.forall2 typeEquals rparamTypes cparamTypes then false
//                else
//                typeEquals func.ReturnType this.ReturnType)
