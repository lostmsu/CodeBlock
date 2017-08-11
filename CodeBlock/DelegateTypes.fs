namespace CodeBlock

open System
open System.Collections.Generic
open System.Reflection
open System.Reflection.Emit
open System.Runtime.InteropServices

type DelegateTypes private() =
    static let cache = Dictionary<string, Type>()

    static let builder =
        let name = AssemblyName()
        name.Version <- Version(1, 0)
        name.Name <- "CodeBlock.DelegateTypes"

        let assembly = AppDomain.CurrentDomain.DefineDynamicAssembly(name, AssemblyBuilderAccess.Run)

        assembly.DefineDynamicModule("DelegateTypes")

    static let callingConvention =
        let attrType = typeof<UnmanagedFunctionPointerAttribute>
        let ctor = attrType.GetConstructor[| typeof<CallingConvention> |]
        let builder = CustomAttributeBuilder(ctor, [| CallingConvention.Cdecl |])
        builder

    static member Get(func: MethodInfo) =
        let typeName = func.DeclaringType.FullName + "." + String.Join(" * ", func.GetParameters() |> Seq.map(fun p -> p.ParameterType.FullName)) + " -> " + func.ReturnType.ToString()
        let cached, t = cache.TryGetValue(typeName)
        if cached then t
        else
        let delegateType = 
            builder.DefineType("DELEGATE_" + Guid.NewGuid().ToString().Replace("-", ""),
                TypeAttributes.Class ||| TypeAttributes.Public ||| TypeAttributes.Sealed ||| TypeAttributes.AnsiClass ||| TypeAttributes.AutoClass, typeof<System.MulticastDelegate>)

        let constructorBuilder = delegateType.DefineConstructor(MethodAttributes.RTSpecialName ||| MethodAttributes.HideBySig ||| MethodAttributes.Public, CallingConventions.Standard, [| typeof<System.Object>; typeof<System.IntPtr> |])
        delegateType.SetCustomAttribute(callingConvention)
        constructorBuilder.SetImplementationFlags(MethodImplAttributes.Runtime ||| MethodImplAttributes.Managed)

        let parameters = func.GetParameters();
        let paramTypes = parameters |> Array.map (fun param -> param.ParameterType)

         // Define the Invoke method for the delegate

        let methodBuilder = delegateType.DefineMethod("Invoke", MethodAttributes.Public ||| MethodAttributes.HideBySig ||| MethodAttributes.NewSlot ||| MethodAttributes.Virtual, func.ReturnType, paramTypes);
        methodBuilder.SetImplementationFlags(MethodImplAttributes.Runtime ||| MethodImplAttributes.Managed);

        let result = delegateType.CreateType();
        cache.Add(typeName, result)
        result
