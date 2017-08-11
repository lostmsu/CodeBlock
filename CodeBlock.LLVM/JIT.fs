namespace CodeBlock

#nowarn "25"

open System.Linq
open System.Diagnostics
open System.Collections.Generic

open Mono.Cecil
open Mono.Cecil.Cil

open LLVM

type private NArrayType = LLVM.ArrayType
type private ArrayType = Mono.Cecil.ArrayType
type Action = System.Action

open Utils
open RuntimeUtils
open ExecutionStack
open ExecutionStack.StackItems
open Instructions
open CodeBlock.TypeSystem
open CodeBlock.TypeSystem.BuiltIns
open CodeBlock.TypeSystem.Cecil
open InstructionBuilderExtensions

type JIT() as this =
    let nativeBits = System.IntPtr.Size * 8
    let context = Context.Global
    let container = Module("", context)
    let engine = ExecutionEngine(container)
    let clrInterop = LLVM.Interop.ClrInterop(engine)
    let mutable translators = []
    
    let resolver = DefaultAssemblyResolver()

    let mutable gc: TypeDefinition = null
    let mutable objModel: TypeReference = null

    let mutable gc_new: Function = null

    let typeEqualityComparer = { new IEqualityComparer<TypeReference> with
        member this.Equals(a: TypeReference, b: TypeReference) =
            warning "simplified type equality comparison"
            a.FullName = b.FullName
        member this.GetHashCode(a: TypeReference) = a.FullName.GetHashCode() }

    let functionEqualityComparer = { new IEqualityComparer<MethodReference> with
        member i.Equals(a: MethodReference, b: MethodReference) =
            // warning "simplified function equality comparison"
            a = b
//            typeEqualityComparer.Equals(a.DeclaringType, b.DeclaringType) &&
//            a.Name = b.Name
        member i.GetHashCode(a: MethodReference) =
            0
        }

    let fieldEqualityComparer = { new IEqualityComparer<FieldReference> with
        member i.Equals(a: FieldReference, b: FieldReference) =
            warning "name-only comparison"
            typeEqualityComparer.Equals(a.DeclaringType, b.DeclaringType) && a.Name = b.Name
        member i.GetHashCode(a: FieldReference) =
            a.DeclaringType.FullName.GetHashCode() ^^^ a.Name.GetHashCode()
        }

    let rec blockIndex blocks offset =
            if Array.get blocks offset = null then
                blockIndex blocks (offset - 1)
            else
                offset
    
    let ntypes = Dictionary<TypeReference, Type>(typeEqualityComparer)
    let nfunctions = Dictionary<MethodReference, Function>()
    let nfields = Dictionary<FieldReference, int>(fieldEqualityComparer)
    let staticFields = Dictionary<string, GlobalValue>()
    let methodTables = Dictionary<TypeReference, GlobalValue>(typeEqualityComparer)
    let compiled = HashSet()

    let fid (fref: #FieldReference) =
        if fref.DeclaringType.IsPrimitive then
            0
        else
            nfields.[fref]

    let stackSizeOf = function
        | I32 -> 32
        | I64 -> 64
        | Native -> nativeBits
        | Reference(_) -> nativeBits
        | _ -> System.ArgumentException() |> raise

    let native = IntegerType.Get(context, nativeBits)
    let i32 = IntegerType.GetInt32(context)
    let i64 = IntegerType.Get(context, 64)
    let i16 = IntegerType.Get(context, 16)
    let i8 = IntegerType.Get(context, 8)
    let i1 = IntegerType.Get(context, 1)
    let voidType = Type.GetVoid(context)
    let virtVoid = StructType(context, [| |], false)
    let pvoid = PointerType.Get(virtVoid, 0)

    let f32 = FloatType.Get(context, 32)
    let f64 = FloatType.Get(context, 64)

    let mutable objectType: PointerType = null
    let mutable arrayLengthIndex: int = -1
    let firstElemIndex() = arrayLengthIndex + 1

    let boxType = typeof<ObjectModel.ObjectBox<_>>.Definition
    
    let dbgbreaktype = FunctionType(voidType, [||], false)
    let dbgbreak = container.CreateFunction("DebugBreak", dbgbreaktype, CallingConvention = CallingConvention.StdCallX86)
    let llvmbreak = container.CreateFunction("LLVMDebugBreak", dbgbreaktype, CallingConvention = CallingConvention.Cdecl)

    let compile: MethodReference -> Function = function
        | null -> null
        | :? GenericInstanceMethod as generic ->
            let noarg = generic.Resolve()
            let map = Dictionary<TypeReference, _>()
            noarg.GenericParameters
            |> Seq.iter2 (fun arg param -> map.Add(param, arg) |> ignore)
                            generic.GenericArguments
            let typeMap =
                typeMapRec (flip Dict.tryFind map)
                
            this.Compile(noarg, false, typeMap)

        | :? MethodDefinition as methoddef ->
            this.Compile(methoddef)
        | hlmethod ->
            let definition = hlmethod.Resolve()
            if definition = null then
                System.NotSupportedException(hlmethod.GetType().Name) |> raise
            else

            let map = Dictionary<TypeReference, _>()
            match hlmethod.DeclaringType with
            | :? GenericInstanceType as genType ->
                let baseType = genType.Resolve()
                genType.GenericArguments
                |> Seq.iteri (fun i genArg -> map.[baseType.GenericParameters.[i]] <- genArg)

                let typeMap =
                    typeMapRec (flip Dict.tryFind map)
//                    match typeRef with
//                    | :? GenericParameter as argType ->
//                        map.[argType]
//                    | _ -> typeRef
                this.Compile(definition, false, typeMap)

            | _ -> this.Compile(definition)

    let load name =
        let hlmethod =
            try
                nfunctions.Keys |> Seq.find (fun func -> func.ToString() = name)
            with
                :? KeyNotFoundException ->
                    null
        
        let nfunc = compile hlmethod
        if nfunc <> null then nfunc |> engine.GetPointer else System.IntPtr.Zero

    let loader = LazyFunctionLoader(load)

    let arrayType (hltype: TypeReference) =
        let objType = hltype.Module.TypeSystem.Object
        let corlib = objType.Module
        let arrayTypeRef = TypeReference("System", "Array", corlib, objType.Scope)
        arrayTypeRef.Resolve()

    do engine.add_LazyLoad loader
    // let functions = Dictionary<MethodReference, MethodDefinition>(functionEqualityComparer)

    // let gc_new: MethodReference = null

    member this.RegisterTranslator(translator: IInstructionTranslator) =
        translators <- translator :: translators

    member this.GetWrapper<'a when 'a:> System.Delegate> (func, ?loop): 'a =
        let loop = defaultArg loop false
        clrInterop.GetDelegate(func, typeof<'a>, container, loop) :?> 'a

    member this.GC with get(): TypeReference = upcast gc
                    and set (value: TypeReference) =
                        if gc = null then
                            gc <- value.Resolve()
                            gc_new <- this.Compile(gc.Method("Alloc"))
                        elif value.Resolve() =&= gc then ()
                        else
                            invalidOp "GC can be set only once"

    member this.ObjectModel with get() = objModel
                            and set value =
                                if objModel = null then
                                    objModel <- value
                                elif objModel =&= value then ()
                                else
                                    invalidOp "ObjectModel can be set only once"

    member this.TypeEqualityComparison = typeEqualityComparer

    member this.FunctionEqualityComparison = functionEqualityComparer

    member this.MatchParameters(stack, parameters) =
        let parameters = parameters |> List.ofSeq |> List.rev

        let rec matchParameter stack (parameters: ParameterDefinition list) verifiable =
            match parameters, stack with
            | [], _ -> stack, verifiable
            | _, [] ->
                System.InvalidProgramException("MatchParameters") |> raise
            | parameter :: rest, actual :: stack ->
                if not (parameter.ParameterType != GetTypeReference actual) then
                    System.InvalidProgramException("MatchParameters") |> raise
                
                matchParameter stack rest verifiable

        matchParameter stack parameters true

    member this.Morf(hltype: TypeReference): LLVM.Type =
        try
            this.MorfUnchecked(hltype)
        with
            | :? System.NotSupportedException as cause ->
                raise <| TypeTranslationException(hltype, cause)
            | :? TypeTranslationException as cause ->
                raise <| TypeTranslationException(hltype, cause)

    member private this.MorfUnchecked (hltype: TypeReference) =
        let morfStarted, value = ntypes.TryGetValue(hltype)
        if morfStarted then
            if hltype.IsValueType || isVoid hltype then value
            else upcast PointerType.Get(value, 0)
        else
            let cache ntype =
                ntypes.Add(hltype, ntype)

                let definition = hltype.Resolve()
                if definition <> null then
                    for staticField in definition.Fields
                                       |> Seq.filter (fun f -> f.IsStatic) do
                        if staticFields.ContainsKey(staticField.FullName) then ()
                        else
                        let ntype = this.Morf(hltype)
                        let zero = ntype.Zero
                        let nvalue = container.AddGlobal(ntype, staticField.FullName, zero)
                        staticFields.Add(staticField.FullName, nvalue)

                ntype

            match hltype with
            | null -> System.ArgumentNullException("hltype") |> raise

            | :? Mono.Cecil.RequiredModifierType as modType ->
                this.Morf modType.ElementType

            | :? ArrayType as array when
                    array.Dimensions.Count = 1 &&
                    (not array.Dimensions.[0].LowerBound.HasValue || 
                     array.Dimensions.[0].LowerBound.Value = 0) ->
                let elementType: Type =
                    match array.ElementType with
                    | ReferenceType(_) ->
                        upcast objectType
                    | valueType ->
                        this.Morf valueType
                // TODO: reference type array sharing
                // TODO: multidimensional non-zero bounded arrays
                let result = StructType(context, hltype.FullName)
//                    let opaque = OpaqueType.Create(context)
//                    let handle = TypeHandle.Create(opaque)
                ntypes.Add(hltype, result)

                let objType = array.Module.TypeSystem.Object
                let corlib = objType.Module
                let arrayTypeRef = TypeReference("System", "Array", corlib, objType.Scope)

                let parts = List<Type>()
                
                let arrayDef = this.Morf(arrayTypeRef.Resolve()) :?> PointerType
                // size field, always 0
                arrayLengthIndex <- parts.Count
                parts.Add(native)

                // The first element of actual data
                parts.Add(NArrayType(elementType))

                result.SetBody(parts.ToArray())
                //warning "array types not supported"
                // TODO: arrays
                upcast PointerType.Get(result)
        
            //#region Primitives
            | _ when isInt32 hltype || isUInt32 hltype ->
                cache <| upcast i32
        
            | _ when isInt16 hltype || isUInt16 hltype ->
                cache <| upcast i16
        
            | _ when isInt64 hltype || isUInt64 hltype ->
                cache <| upcast i64

            | _ when isIntPtr hltype || isUIntPtr hltype ->
                cache <| upcast native
            
            | _ when isVoid hltype -> cache <| voidType

            | _ when isBool hltype ->
                cache <| upcast i1

            | _ when isByte hltype || isSByte hltype ->
                cache <| upcast i8

            | _ when isChar hltype ->
                // let structure = StructType(context, [| autocast<Type> i16 |], false)
                cache <| upcast i16

            | _ when isSingle hltype ->
                cache <| upcast f32

            | _ when isDouble hltype ->
                cache <| upcast f64

            | _ when isObject hltype ->
                if isNull objModel then notSupported "Cannot translate System.Object: object model is not specified"
                if isNull gc then notSupported "Cannot translate System.Object: GC is not specified"

                let structure = StructType(context, [| this.Morf objModel; this.Morf gc |], false)
                objectType <- PointerType.Get(cache structure, 0)
                upcast objectType
            //#endregion

            | :? GenericInstanceType
            | :? TypeDefinition ->
                let typedef = hltype.Resolve()

                let instantiate = hltype.TypeMap

                let createMethodTable() =
                    let methodsType = NArrayType(pvoid, typedef.Methods.Count)
                    let methodTableType = StructType(context, [methodsType :>Type])
                    let zero = methodTableType.Zero
                    let methodTable =
                        container.AddGlobal(methodTableType,
                            "MT:" + hltype.ToString(), zero)
                    methodTables.[hltype] <- methodTable

                let callStaticConstructor() =
                    let constructors =
                        typedef.Methods
                        |> Seq.filter (fun m -> m.IsConstructor)
                        |> List.ofSeq
                    let staticConstructor =
                        constructors
                        |> List.tryFind(fun m -> m.IsStatic)
                    match staticConstructor with
                    | None -> ()
                    | Some(classConstructor) ->
                        let native = this.Compile(classConstructor, false, instantiate)
                        let wrapper: Action = this.GetWrapper(native)
                        wrapper.Invoke()

                if typedef.IsClass then
                    let result = StructType(context, hltype.FullName)
//                    let opaque = OpaqueType.Create(context)
//                    let handle = TypeHandle.Create(opaque)
                    ntypes.Add(hltype, result)

                    let parts = List<Type>()
                    if not <| typedef.IsValueType then
                        let baseClass = this.Morf(instantiate typedef.BaseType) :?> PointerType
                        parts.Add(baseClass.ElementType)
                    
                    let fields = typedef.Fields |> Seq.filter (fun f -> not f.IsStatic)
                    fields |> Seq.iteri (fun i field ->
                        if typedef.IsValueType && typeEquals field.FieldType hltype then
                            invalid_code(sprintf "recursive type: %s" typedef.FullName)
                            |> typeTranslationFailed typedef

                        let instantiated = instantiate field.FieldType
                        let fieldRef = FieldReference(field.Name, instantiated, hltype)
                        nfields.[fieldRef] <- parts.Count
                        let nativeType = this.Morf(instantiated)
                        assert(nativeType <> null)
                        parts.Add(nativeType)
                    )
                    typedef.Fields |> Seq.filter(fun f -> f.IsStatic)
                    |> Seq.iter (fun f ->
                        let fref =
                            FieldReference(f.Name, instantiate f.FieldType, hltype)
                        if staticFields.ContainsKey(fref.FullName) then ()
                        else
                        warning "volatile not supported"
                        // TODO: mark as GC root
                        // TODO: volatile here, and in non-static
                        let ntype = this.Morf(fref.FieldType)
                        let zero = ntype.Zero
                        let nvalue = container.AddGlobal(ntype, fref.FullName, zero)
                        staticFields.Add(fref.FullName, nvalue)
                    )
                    result.SetBody(parts.ToArray(), false)
//                    let result = StructType(context, parts.ToArray(), false)
//                    (handle.Resolve() :?> OpaqueType).Refine result

//                    let result = handle.Resolve()

                    createMethodTable()
                    callStaticConstructor()

                    if hltype.IsValueType then
                        upcast result
                    else upcast PointerType.Get(result, 0)
            
                elif typedef.IsInterface then
                    warning "interface type: function pointers"
                    callStaticConstructor()
                    warning "create method table?"
                    cache <| upcast PointerType.Get(PointerType.Get(i32, 0), 0)

                else
                    not_implemented "type morfing" |> raise
        
            | :? ByReferenceType as hltype ->
                let elem = this.Morf(hltype.ElementType)
                cache <| upcast PointerType.Get(elem, 0)

            | _ when is<AssemblyNameReference> hltype.Scope ->
                let assembly = resolver.Resolve(downcast hltype.Scope)
                let typedef = assembly.GetType(hltype)
                this.Morf(typedef)

            | :? Mono.Cecil.PointerType as ptr ->
                let elem: Type =
                    if isVoid ptr.ElementType then
                        upcast i8
                    else
                        this.Morf(ptr.ElementType)
                cache <| upcast PointerType.Get(elem, 0)

            | _ ->
                not_implemented (sprintf "type morfing for %A" (hltype.GetType()))
                |> raise

    static member private GetTypeName(t: #TypeReference) =
        t.FullName

    static member private GetFunctionName(func: #MethodReference) =
        // JIT.GetTypeName(func.DeclaringType) + "." + func.Name
        func.ToString()

    member private this.GetFunction(hlmethod: MethodReference, ?typeMap) =
        if nfunctions.ContainsKey hlmethod then
            Debug.Print("function cache hit: {0}", hlmethod)
            nfunctions.[hlmethod]
        else
            let typeMap =
                let innerTypeMap = Generics.getTypeMap(hlmethod)
                match typeMap with
                | None -> innerTypeMap
                | Some(map) -> map >> innerTypeMap
            let rettype = this.Morf <| typeMap hlmethod.ReturnType
            // TODO: name args
            let args =
                hlmethod.Parameters
                |> Seq.map (fun param ->
                            let arg = this.Morf <| typeMap param.ParameterType
                            arg, param.Name )
            let args =
                if hlmethod.HasThis then
                    let declaringType = typeMap hlmethod.DeclaringType
                    let thisType = this.Morf declaringType
                    let thisType = if thisType.Kind = TypeKind.Pointer then thisType else upcast PointerType.Get(thisType, 0)
                    args |> Seq.append (Seq.singleton (thisType, "this"))
                else
                    args
            let args = args |> Array.ofSeq
            let ftype = FunctionType(rettype, Array.map fst args, false)
            let name = JIT.GetFunctionName(hlmethod)
            let nfunc = container.CreateFunction(name, ftype)
            nfunc.CallingConvention <- CallingConvention.Fast
            for i = 0 to args.Length - 1 do
                nfunc.[i].Name <- snd args.[i]
            nfunctions.Add(hlmethod, nfunc)
            nfunc
    
    member private this.GenerateBlocks(hlmethod: MethodDefinition, nfunc: Function) =
        let blocks = Array.init<Block> hlmethod.Body.CodeSize (fun idx -> null)
        blocks.[0] <- Block("", context, nfunc)
        
        let blockIndex = blockIndex blocks
        
        let markTarget (target: Cil.Instruction) =
            let block = blockIndex target.Offset
            if block <> target.Offset then
                blocks.[target.Offset] <- Block("", context, nfunc)

        for instr in hlmethod.Body.Instructions do
            if notNull instr then
                match instr.Operand with
                | :? Cil.Instruction as target ->
                   markTarget target
                | :? (Cil.Instruction[]) as targets ->
                    Array.iter markTarget targets                   
                | _ -> ()

                if IsControlFlow instr.OpCode.Code && notNull instr.Next then
                    markTarget instr.Next
        
        blocks

    member this.ExecutionEngine = engine

    member this.GetWrapper(func, delegateType, ?loop) =
        let loop = defaultArg loop false
        clrInterop.GetDelegate(func, delegateType, container, loop)

    member this.Compile(hlmethod: MethodDefinition, ?loop, ?typeMap) =
        let typeMap = defaultArg typeMap id
        let loop = defaultArg loop false
        try
            this.CompileUnchecked(hlmethod, loop, typeMap)
        with
            | :? TranslationException as cause ->
                raise <| MethodTranslationException(hlmethod, cause)

    member private this.CompileUnchecked(hlmethod: MethodDefinition, ?loop, ?typeMap) =
        let typeMap = defaultArg typeMap id
        let nfunc = this.GetFunction(hlmethod, typeMap)
        if not <| compiled.Add(nfunc) then nfunc
        elif not hlmethod.HasBody then
            let nfunc = container.CreateFunction(hlmethod.Name, nfunc.Type)
            nfunc.CallingConvention <- CallingConvention.C
            eprintfn "No implementation for %s" hlmethod.FullName
            if not <| hlmethod.DeclaringType.FullName.StartsWith("CodeBlock") then
                eprintfn "replaced with stub"
                let start = Block("stub", context, nfunc)
                let gen = InstructionBuilder(context, start)
                gen.Call(dbgbreak, [| |]) |> ignore
                gen.Return() |> ignore
            nfunc
        else
        let loop = defaultArg loop false
        let startBlock = Block("start", context, nfunc)
        let entryLoop =
            if loop then
                Block("entryLoop", context, nfunc)
            else null
        
        let blocks = this.GenerateBlocks(hlmethod, nfunc)
        let blockIndex = blockIndex blocks
        let gen = InstructionBuilder(context, blocks.[0])
        let prologue = InstructionBuilder(context, startBlock)

        if loop then
            gen.PointToEnd(entryLoop)

            gen.Call(dbgbreak) |> ignore
            gen.GoTo(blocks.[0]) |> ignore

            gen.PointToEnd(blocks.[0])
        
        let nvars = [| for i in 0 .. hlmethod.Body.Variables.Count - 1 ->
                        let variable = hlmethod.Body.Variables.[i]
                        let varType = typeMap variable.VariableType
                        let ntype = this.Morf(varType)
                        let ref = prologue.StackAlloc(ntype)
                        ref.Name <- variable.Name
                        if not varType.IsValueType then
                                    prologue.GCRoot(ref, container) |> ignore
                        if hlmethod.Body.InitLocals then
                            gen.Store(ntype.Zero, ref) |> ignore
                        ref |]
        
        let hasStarg = Array.init (hlmethod.Parameters.Count + 1) (fun i -> false)
        for instr in hlmethod.Body.Instructions do
            match instr with
            | Starg(index) ->
                hasStarg.[index] <- true
            | Ldarga(index) ->
                hasStarg.[index] <- true
            | _ -> ()
        let nargs = [| for i in 0 .. hlmethod.Parameters.Count ->
                        if i = 0 then
                            if hlmethod.HasThis then
                                let paramType: TypeReference =
                                    if hlmethod.DeclaringType.IsValueType then
                                        upcast Mono.Cecil.ByReferenceType(hlmethod.DeclaringType)
                                    else
                                        upcast hlmethod.DeclaringType
                                let ntype = this.Morf <| typeMap paramType
                                let narg = nfunc.Arguments 0
                                narg :> Value
                                // not_implemented("noargs") |> raise
                            else
                                null
                        else
                            let paramType = typeMap hlmethod.Parameters.[i - 1].ParameterType
                            let ntype = this.Morf(paramType)
                            let ni = if hlmethod.HasThis then i else i - 1
                            let narg = nfunc.Arguments ni
                            if hasStarg.[i] then
                                let ref = prologue.StackAlloc(ntype)
                                if not paramType.IsValueType then
                                    prologue.GCRoot(ref, container) |> ignore
                                gen.Store(narg, ref) |> ignore
                                ref :> Value
                            else
                                narg :> Value |]
        
        let isGCRef = function
            | Object(null) -> true
            | Object(oldT) when oldT <> null ->
                let t = typeMap oldT
                not t.IsValueType && not t.IsByReference
            | _ -> false

        let rec translateInternal stack nstack (instr: Cil.Instruction) =
            if instr = null then System.ArgumentNullException("instr") |> raise

            if blocks.[instr.Offset] <> null then
                if instr.Previous <> null && not(IsControlFlow instr.Previous.OpCode.Code) then
                    gen.GoTo(blocks.[instr.Offset]) |> ignore
                gen.PointToEnd(blocks.[instr.Offset])

            translate stack nstack instr
            
        and translate stack nstack (instr: Cil.Instruction) =
            let translator() =
                {   new IInstructionTranslator with
                        member this.Translate context state instruction =
                            match translate state.Stack state.NativeStack instruction with
                            | stack, nstack, verifiable ->
                                { state with
                                    Stack = stack;
                                    NativeStack = nstack;
                                    Verifiable = verifiable;
                                }
                                |> Option.Some }

            //#region helpers
            let getValue stackItem (nvalue: Value) =
                if isGCRef stackItem then gen.Load(nvalue) :> Value else nvalue

            let newValue stackItem (nvalue: Value)  =
                if isGCRef stackItem then
                    if not (nvalue.Type.Kind = TypeKind.Pointer) then
                        let msg =
                            sprintf
                                "Value on managed stack is a reference (%A), but unmanaged value is not (%A)"
                                stackItem nvalue.Type.Kind
                        raise <| TranslatorAssertionFailed(msg)
                    let alloca = prologue.StackAlloc(nvalue.Type)
                    prologue.GCRoot(alloca, container) |> ignore
                    gen.Store(nvalue, alloca) |> ignore
                    alloca :> Value
                else
                    match stackItem with
                    | IsInteger(item) ->
                        let actualSize = (nvalue.Type :?> IntegerType).Width
                        let targetSize = stackSizeOf item
                        let targetType = IntegerType.Get(context, targetSize)
                        if actualSize < targetSize then
                            gen.SignExtend(nvalue, targetType)
                        elif actualSize = targetSize then
                            nvalue
                        else
                            invalidOp "Automatic int truncation is not allowed"
                    | _ -> nvalue

            let genUpcast stackItem (targetType: TypeReference) value =
                let getBitCast() =
                    let nativeTarget = this.Morf(targetType) :?> PointerType
                    let value = gen.PointerCast(value, nativeTarget)
                    let resultValue = newValue (Object targetType) value
                    let readValue = getValue stackItem resultValue
                    readValue

                match stackItem with
                | Object(o) when o = targetType ->
                    value
                | Object(o) when o = null && not targetType.IsValueType ->
                    getBitCast()
                | Object(o) when not targetType.IsValueType ->
                    // TODO: array variance
                    let targetTypeDef = targetType.ResolveFixed()
                    let oDef = o.ResolveFixed()
                    if oDef.Interfaces.Contains(targetTypeDef) 
                         || isBasedOn targetTypeDef oDef then
                        getBitCast()
                    else
                        let msg = sprintf "Can't cast %O (%O) to %O (%O)"
                                             o oDef targetType targetTypeDef
                        raise <| System.InvalidCastException(msg)

                | I32 when isEnum targetType ->
                    let ntype = this.Morf targetType
                    let enumValue = gen.StackAlloc(ntype)
                    let field = gen.StructureElement(enumValue, 0)
                    gen.Store(value, field) |> ignore
                    upcast gen.Load(enumValue)

                | I32 when isBool targetType ->
                    gen.Trunc(value, i1)

                | I32 when isUInt16 targetType ->
                    gen.Trunc(value, i16)

                | I32 when isChar targetType ->
                    gen.Trunc(value, i16)

                | Reference(o) when not o.IsValueType ->
                    let targetType = targetType.Resolve()
                    let o = o.Resolve()
                    if o = targetType then value
                    elif o.Interfaces.Contains(targetType) || isBasedOn targetType o then
                        let nativeTarget = this.Morf(targetType) :?> PointerType
                        let value = gen.PointerCast(value, PointerType.Get nativeTarget)
                        newValue (Object targetType) value
                    else
                        raise <| System.InvalidCastException()
                
                | _ -> value

            let call (target: Function, hlfunc: MethodReference) ret 
                     (nstack: Value list) stack
                     cc isTail =
                let argc = target.Type.ArgumentCount
                let parameters =
                    let nativeArgs =
                        Seq.take argc nstack
                        |> Seq.zip stack
                        |> Seq.map (fun (t, v) -> t, getValue t v)
                        |> Array.ofSeq
                    System.Array.Reverse(nativeArgs)
                    let actualParams = hlfunc.ActualParametersResolved
                    nativeArgs
                    |> Seq.zip actualParams
                    |> Seq.map(fun (target, (stackItem, value)) ->
                        genUpcast stackItem target value)
                    |> Array.ofSeq
                let call = gen.Call(target, parameters)
                call.CallingConvention <- cc
                if isTail then call.TailCall <- true
                if target.Type.ReturnType.Kind = TypeKind.Void then
                    List.skip argc nstack
                else
                    let call = newValue ret call
                    call :: List.skip argc nstack

            let extend value sourceType =
                if isBool sourceType || isByte sourceType || isUInt16 sourceType then
                    gen.ZeroExtend(value, i32, "")
                elif isSByte sourceType || isInt16 sourceType then
                    gen.SignExtend(value, i32, "")
                else
                    value

            let truncate value destType =
                if isBool destType then
                    gen.Trunc(value, i1, "")
                elif isByte destType || isSByte destType then
                    gen.Trunc(value, i8, "")
                elif isInt16 destType || isUInt16 destType then
                    gen.Trunc(value, i16, "")
                else
                    value

            let convNIU name (value: Value) (targetType: IntegerType) =
                let vtype = value.Type :?> IntegerType
                let cmp = vtype.Width.CompareTo(targetType.Width)
                if cmp > 0 then
                    gen.Trunc(value, targetType, name)
                elif cmp < 0 then
                    gen.ZeroExtend(value, targetType, name)
                else
                    value

            let convI name value nvalue targetType =
                if stackSizeOf value > stackSizeOf targetType then
                    gen.Trunc(nvalue, native, name)
                elif stackSizeOf value < stackSizeOf targetType then
                    gen.ZeroExtend(nvalue, native, name)
                else
                    nvalue

            let convNative name value nvalue =
                convI name value nvalue Native

            let ldarg num =
                let param =
                    if hlmethod.HasThis then
                        if num = 0 then
                            let declarer = typeMap hlmethod.DeclaringType
                            if declarer.IsValueType then
                                Mono.Cecil.ByReferenceType(declarer) :> TypeReference
                            else
                                declarer
                        else
                            typeMap hlmethod.Parameters.[num - 1].ParameterType
                    else
                        typeMap hlmethod.Parameters.[num].ParameterType
                
                let num = if hlmethod.HasThis then num else num + 1
                let arg =
                    if not hasStarg.[num] then
                        nargs.[num]
                    else
                        upcast gen.Load(nargs.[num])
                    |> newValue (GetStackItem param)

                let earg = extend arg param

                GetStackItem param :: stack, arg :: nstack, true
            
            let ldloc num =
                let loc = hlmethod.Body.Variables.[num]
                let varType = typeMap loc.VariableType
                let value =
                    gen.Load nvars.[num]
                    |> newValue (GetStackItem varType)
                let evalue = extend value varType
                GetStackItem varType :: stack, evalue :: nstack, true

            let stloc (item, nitem) num =
                let loc = hlmethod.Body.Variables.[num]
                let varType = typeMap loc.VariableType
                let verifiable = varType != GetTypeReference item
                let nitem = getValue item nitem
                let titem = truncate nitem varType
                let ptr = nvars.[num]
                if item = StackItem.Object(null) then
                    gen.Store(ptr.Type.ElementType.Zero, ptr) |> ignore
                else
                    gen.Store(titem, ptr) |> ignore
                stack.Tail, nstack.Tail, verifiable

            let ldc4 (value: int32) =
                let c4 = i32.Constant(uint32 value |> uint64, true) :> Value
                I32 :: stack, c4 :: nstack, true

            let verifyBranch target stack =
                warning "branch verification"
                true
            
            let icompare opcode nstack =
                let a :: b :: rest = nstack
                let at :: bt :: _ = stack
                let comparison =
                    match opcode with
                    | Code.Ceq -> IntegerComparison.Equal
                    | Code.Cgt -> IntegerComparison.SignedGreater
                    | Code.Cgt_Un -> IntegerComparison.UnsignedGreater
                    | Code.Clt -> IntegerComparison.SignedLess
                    | Code.Clt_Un -> IntegerComparison.UnsignedLess
                    | _ -> System.ArgumentOutOfRangeException() |> raise
                
                let a = getValue at a
                let b = getValue bt b
                let boolResult = gen.Compare(comparison, b, a, "")
                let intResult = gen.ZeroExtend(boolResult, i32, "compareRes")
                intResult :: rest

            let icbranch opcode nstack =
                let compare, swap =
                    match opcode with
                    | Code.Beq | Code.Beq_S -> icompare Code.Ceq, false
                    | Code.Bge | Code.Bge_S -> icompare Code.Clt, true
                    | Code.Bge_Un | Code.Bge_Un_S -> icompare Code.Clt_Un, true
                    | Code.Bgt | Code.Bgt_S -> icompare Code.Cgt, false
                    | Code.Bgt_Un | Code.Bgt_Un_S -> icompare Code.Cgt_Un, false
                    | Code.Ble | Code.Ble_S -> icompare Code.Cgt, true
                    | Code.Ble_Un | Code.Ble_Un_S -> icompare Code.Cgt_Un, true
                    | Code.Blt | Code.Blt_S -> icompare Code.Clt, false
                    | Code.Blt_Un | Code.Blt_Un_S -> icompare Code.Clt_Un, false
                    | Code.Bne_Un | Code.Bne_Un_S -> icompare Code.Ceq, true
                    | _ -> System.ArgumentOutOfRangeException() |> raise

                let ontrue = blocks.[(instr.Operand :?> Cil.Instruction).Offset]
                let onfalse = blocks.[instr.Next.Offset]
                let ontrue, onfalse =
                    if swap then onfalse, ontrue
                    else ontrue, onfalse
                
                let cond :: nstack = compare nstack
                let cond = gen.IsNull(cond)
                
                gen.``if`` onfalse ontrue (cond :: nstack)

            let iarithmetic = function
                | Code.Add -> gen.add
                | Code.Sub -> gen.sub
                | Code.Mul -> gen.mul
                | Code.Div -> gen.div true
                | Code.Div_Un -> gen.div false
                | Code.Rem -> gen.rem true
                | Code.Rem_Un -> gen.rem false
                | _ -> System.ArgumentOutOfRangeException() |> raise

            let ilogic = function
                | Code.And -> gen.``and``
                | Code.Or -> gen.``or``
                | Code.Xor -> gen.xor
                | _ -> System.ArgumentOutOfRangeException() |> raise

            let ishift = function
                | Code.Shl -> gen.shl
                | Code.Shr -> gen.shr true
                | Code.Shr_Un -> gen.shr false
                | _ -> System.ArgumentOutOfRangeException() |> raise

            let inative() =
                if nativeBits = 32 then nstack
                else
                    warning "i32 to native: sign extension only"
                    match nstack with
                    | a :: b :: rest ->
                        if stack.Head = I32 then
                            let value = gen.SignExtend(a, native, "")
                            value :: b :: rest
                        else
                            let value = gen.SignExtend(b, native, "")
                            a :: value :: rest
                    | _ -> System.InvalidOperationException() |> raise

            let actualField (field: FieldReference) =
                let declaringType = typeMap field.DeclaringType
                this.Morf declaringType |> ignore
                FieldReference(field.Name, typeMap field.FieldType, declaringType)

            let instructionField() =
                let field = instr.Operand :?> FieldReference
                actualField field

            let ldfldRef stackItem stack =
                let field = instructionField()
                let fieldType = typeMap(field.DeclaringType.TypeMap field.FieldType)
                let ref :: nstack = nstack
                if field.DeclaringType.IsPrimitive then
                    let targetType = this.Morf fieldType
                    let nvalue =
                        let nvalue = gen.Load(ref)
                        if targetType.Kind = TypeKind.Pointer then
                            gen.IntToPointer(nvalue, downcast targetType)
                        else
                            upcast nvalue
                    GetStackItem field.DeclaringType :: stack, nvalue :: nstack, false
                else
                let fid = fid field
                let targetType = this.Morf field.DeclaringType
                let ref = getValue stackItem ref
                let targetType = if field.DeclaringType.IsValueType then
                                    PointerType.Get targetType
                                 else downcast targetType
                let ref = gen.PointerCast(ref, targetType)
                let ptr = gen.StructureElement(ref, fid, "")
                let value = newValue (GetStackItem fieldType) <| gen.Load(ptr, "")
                let evalue = extend value fieldType
                GetStackItem fieldType :: stack, evalue :: nstack, true

            let elementRefNoGC narr index =
                // TODO: LLVM inbounds?
                // TODO: bounds check
                let arrayDataIndex = firstElemIndex()
                let zeroIndex = i32.Constant(0UL, false)
                let arrIndex = i32.Constant(uint64 arrayDataIndex, false)
                let indexes: Value array = [| zeroIndex; arrIndex; index  |]
                gen.Element(narr, indexes, "elemPtr")

            let elementRef arrStackType narrGC index =
                let narr = getValue arrStackType narrGC
                elementRefNoGC narr index
                
            //#endregion helpers

            let resolveMethodReference = Generics.resolveMethodReference typeMap

            let makeCall (stack: _ list) =
                let isTail = instr.Previous <> null && instr.OpCode.Code = Code.Tail
                // TODO: generic method calls
                let func = resolveMethodReference(downcast instr.Operand)

                let target = compile func
                warning "no parameter verifying"
                let retType = typeMap func.ActualReturnType
                if isVoid retType then
                    List.skip target.Type.ArgumentCount stack,
                        call (target, func) (Object null)
                             nstack stack
                             target.CallingConvention isTail, true
                else
                    GetStackItem retType :: List.skip target.Type.ArgumentCount stack,
                        call (target, func) (GetStackItem retType)
                             nstack stack
                             target.CallingConvention isTail, true

            let newObj init =
                let ctor = compile init
                let objType = typeMap init.DeclaringType
                if objType.IsValueType then
                    let ntype = this.Morf objType
                    let ptr = gen.StackAlloc(ntype, "") :> Value
                    gen.Store(ntype.Zero, ptr) |> ignore
                    let nstack = nstack |> List.insert init.Parameters.Count ptr
                    let refType = ByReferenceType(objType)
                    let stack = List.insert init.Parameters.Count (GetStackItem refType) stack
                    let nstack = call (ctor, init) (GetStackItem init.ReturnType)
                                        nstack stack ctor.CallingConvention false
                    warning "no verification"
                    let resultType = GetStackItem objType
                    let result: Value = upcast gen.Load(ptr, "")
                    resultType :: List.skip ctor.Type.ArgumentCount stack,
                    result :: nstack, true
                    // notImplemented "newobj struct"
                else
                    if gc = null then notSupported "Heap is not supported. Please, specify a GC."
                    let ntype = this.Morf objType :?> PointerType
                    let structType = ntype.ElementType
                    let alignType = gc_new.Arguments(1).Type :?> IntegerType
                    let sizeType = gc_new.Arguments(0).Type :?> IntegerType
                    let structAlign = convNIU "gc_align" structType.Align alignType
                    let structSize = convNIU "gc_size" structType.Size sizeType
                    let ptr = gen.Call(gc_new, [| structSize; structAlign |])
                    let ptr = gen.IntToPointer(ptr, ntype, "")
                    gen.Store(ntype.ElementType.Zero, ptr) |> ignore
                    let newObjPtr = newValue (GetStackItem objType) ptr

                    let preInitNstack = List.insert init.Parameters.Count newObjPtr nstack
                    let preInitStack = List.insert init.Parameters.Count (GetStackItem objType)  stack
                    let nstack = call (ctor, init) (GetStackItem init.ReturnType)
                                        preInitNstack preInitStack ctor.CallingConvention false
                    warning "no verification"
                    let resultType = GetStackItem objType
                    resultType :: List.skip ctor.Type.ArgumentCount preInitStack,
                    newObjPtr :: nstack, true

            match instr.OpCode.Code, stack with
            //#region arithmetics
            | Arithmetic(op), IsInteger(a) :: IsInteger(b) :: stack when a = b->
                let func = iarithmetic op
                a :: stack, func nstack, true

            | Arithmetic(op), IsInteger(a) :: IsInteger(b) :: stack when NativeMix a b->
                let func = iarithmetic op
                let nstack = inative()
                Native :: stack, func nstack, true

            | Arithmetic(op), _ ->
                warning "arithmetics: floats"
                warning "arithmetics: pointers"
                System.InvalidProgramException("Arithmetic") |> raise
            
            | DivUnArithmetic(op), IsInteger(a) :: IsInteger(b) :: stack when a = b->
                let func = iarithmetic op
                a :: stack, func nstack, true
            | DivUnArithmetic(op), _ ->
                warning "arithmetics: type mixing"
                System.InvalidProgramException("Arithmetic") |> raise


            | Code.Neg, IsInteger(v) :: stack ->
                v :: stack, gen.neg nstack, true
            | Code.Neg, _ ->
                warning "arithmetics: floats"
                System.InvalidProgramException("Neg") |> raise
            //#endregion arithmetics

            //#region logics
            | LogicBinary(op), IsInteger(a) :: IsInteger(b) :: stack when a = b->
                let func = ilogic op
                a :: stack, func nstack, true
            | LogicBinary(op), _ ->
                warning "logics: type mixing"
                System.InvalidProgramException("LogicBinary") |> raise

            | Code.Not, IsInteger(a) :: stack ->
                a :: stack, gen.not nstack, true
            | Code.Not, _ ->
                System.InvalidProgramException("Not") |> raise
            //#endregion logics

            //#region bits
            | Shift(op), a :: b :: stack when a = b && (a = I32 || a = Native) ->
                let func = ishift op
                a :: stack, func nstack, true
            | Shift(op), I32 :: I64 :: stack ->
                let shiftAmount :: nstack = nstack
                let shiftAmount = gen.ZeroExtend(shiftAmount, i64)
                let nstack = shiftAmount :: nstack
                let func = ishift op
                I64 :: stack, func nstack, true
            | Shift(op), _ ->
                warning "shifts: type mixing"
                System.InvalidProgramException("Shift") |> raise
            //#endregion bits

            //#region compare
            | Comparison(op), IsInteger(a) :: IsInteger(b) :: stack when a = b ->
                I32 :: stack, icompare op nstack, true
            | Comparison(op), Reference(_) :: Reference(_) :: stack ->
                let func = icompare op
                I32 :: stack, func nstack, true
            
            | Code.Ceq, Object(_) :: Object(_) :: stack ->
                warning "ceq: non-reference types"
                I32 :: stack, icompare Code.Ceq nstack, true

            | Code.Cgt_Un, Object(_) :: Object(_) :: stack ->
                warning "cgt_un: non-reference types"
                I32 :: stack, icompare Code.Cgt_Un nstack, true

            | Code.Ceq, Native :: Object(_) :: stack
            | Code.Ceq, Object(_) :: Native :: stack ->
                warning "ceq: non-reference types"
                warning "ceq: assuming sizeof(Native) == sizeof(void*)"
                I32 :: stack, icompare Code.Ceq nstack, true
            | Comparison(op), _ ->
                warning "compare: floats"
                warning "compare: objects"
                warning "compare: type mixing"
                System.InvalidProgramException("Comparison") |> raise
            //#endregion compare

            //#region branches
            | ComparisonBranch(op), IsInteger(a) :: IsInteger(b) :: stack when a = b ->
                stack, icbranch op nstack, true
            | ComparisonBranch(op), Reference(_) :: Reference(_) :: stack ->
                stack, icbranch op nstack, true
            | Code.Beq, Object(_) :: Object(_) :: stack
            | Code.Beq_S, Object(_) :: Object(_) :: stack ->
                stack, icbranch Code.Beq nstack, true
            | Code.Beq, Native :: Reference(_) :: stack
            | Code.Beq, Native :: Reference(_) :: stack
            | Code.Beq_S, Reference(_) :: Native :: stack
            | Code.Beq_S, Reference(_) :: Native :: stack ->
                stack, icbranch Code.Beq nstack, true
            | ComparisonBranch(op), _ ->
                warning "cbranches: floats"
                warning "cbranches: objects"
                warning "cbranches: type mixing"
                System.InvalidProgramException("CBranch") |> raise

            | Code.Br, stack | Code.Br_S, stack ->
                let target = blocks.[(instr.Operand :?> Cil.Instruction).Offset]
                [], gen.goto target nstack, true

            | Code.Switch, value :: stack ->
                let next = blocks.[instr.Next.Offset]
                assert(notNull next)
                let nvalue :: nstack = nstack
                let nvalue =
                    match value with
                    | I32 -> nvalue
                    | Object t when t.Resolve().IsEnum ->
                        let addr = gen.StackAlloc(nvalue.Type)
                        gen.Store(nvalue, addr) |> ignore
                        let valueAddr = gen.StructureElement(addr, 0)
                        upcast gen.Load(valueAddr)
                let pairs =
                    let targets = instr.Operand :?> Cil.Instruction[]
                    targets
                    |> Array.mapi (fun i v ->
                        autocast<Value>(i32.Constant(uint64 i, false)), 
                        blocks.[v.Offset])
                gen.Switch(nvalue, next, pairs) |> ignore
                stack, nstack, true

            | Code.Brfalse, CanBeZero(a) :: stack
            | Code.Brfalse_S, CanBeZero(a) :: stack ->
                let jump = blocks.[(instr.Operand :?> Cil.Instruction).Offset]
                let next = blocks.[instr.Next.Offset]
                let cond :: nstack = nstack
                let nstack = gen.IsNull(cond, "") :: nstack
                stack, gen.``if`` jump next nstack, true
            | Code.Brtrue, CanBeZero(a) :: stack
            | Code.Brtrue_S, CanBeZero(a) :: stack ->
                let jump = blocks.[(instr.Operand :?> Cil.Instruction).Offset]
                let next = blocks.[instr.Next.Offset]
                let cond :: nstack = nstack
                let nstack = gen.IsNull(cond, "") :: nstack
                stack, gen.``if`` next jump nstack, true
            
            | Code.Brfalse, _ | Code.Brfalse_S, _
            | Code.Brtrue, _ | Code.Brtrue_S, _ ->
                System.InvalidProgramException("Brtrue/false") |> raise

            | Code.Leave, stack | Code.Leave_S, stack ->
                warning "leave: exception handling is not implemented"
                let target = blocks.[(instr.Operand :?> Cil.Instruction).Offset]
                [], gen.goto target nstack, true
            //#endregion branches

            | Code.Break, stack ->
                not_implemented("noargs") |> raise

            | Code.Tail, stack ->
                // do nothing, we will handle it later
                stack, nstack, true

            | Code.Call, stack ->
                makeCall stack

            | Code.Calli, stack ->
                let isTail = instr.Previous <> null && instr.OpCode.Code = Code.Tail
                not_implemented("noargs") |> raise

            | Code.Ckfinite, Float :: stack ->
                not_implemented("noargs") |> raise
            | Code.Ckfinite, _ ->
                System.InvalidProgramException("Ckfininte") |> raise

            //#region conv
            | ConvI(size, signed), IsInteger(a) :: stack ->
                let value :: nstack = nstack
                let mvalue =
                    match size with
                    | 0 -> Native
                    | 8 | 16 | 32 -> I32
                    | 64 -> I64
                    | _ -> System.InvalidProgramException("ConvI") |> raise
                let size = if size = 0 then nativeBits else size
                let targetType = IntegerType.Get(context, size)
                let value =
                    if stackSizeOf a > size then
                        gen.Trunc(value, targetType, "")
                    elif stackSizeOf a < size then
                        if signed then
                            gen.SignExtend(value, targetType, "")
                        else
                            gen.ZeroExtend(value, targetType, "")
                    else
                        value
                let value =
                    if size > 0 && size < 32 then
                        if signed then
                            gen.SignExtend(value, i32, "")
                        else
                            gen.ZeroExtend(value, i32, "")
                    else
                        value
                mvalue :: stack, value :: nstack, true
            | ConvI(_, _), Float :: stack ->
                not_implemented("noargs") |> raise
            | ConvI(_, _), _ ->
                System.InvalidProgramException("ConvI") |> raise

            | ConvFloat(op), IsNumeric(_) :: stack ->
                not_implemented("noargs") |> raise
            //#endregion conv
            
            //#region Stack, native memory and jumps
            | Code.Localloc, Native :: stack
            | Code.Localloc, I32 :: stack ->
                let size :: nstack = nstack
                let value = gen.StackAlloc(i8, size, "")
                Native :: stack, upcast value :: nstack, false
            
            | Code.Cpblk, I32 :: IsPointer(s) :: IsPointer(d) :: stack ->
                let size :: src :: dst :: nstack = nstack
                let memmove = if nativeBits <= 32 then container.MemMove32 else container.MemMove64
                gen.Call(memmove, [| dst; src; size; upcast i32.Constant(1UL, false); upcast i1.Zero |]) |> ignore
                stack, nstack, false
            
            | Code.Initblk, I32 :: I32 :: IsPointer(d) :: stack ->
                not_implemented("noargs") |> raise
            
            | Code.Dup, some :: stack ->
                let nvalue :: nstack = nstack
                some :: some :: stack, nvalue :: nvalue :: nstack , true

            | Code.Dup, [] ->
                System.InvalidProgramException("Stack is empty on Dup") |> raise
            
            | Code.Pop, some :: stack ->
                stack, nstack.Tail, true
            | Code.Pop, [] ->
                System.InvalidProgramException("Stack is empty on Pop") |> raise
            
            | Code.Jmp, [] ->
                not_implemented("noargs") |> raise
            
            | Code.Jmp, _ ->
                System.InvalidProgramException("Stack is not empty on Jmp") |> raise
            
            | Code.Ldarg, _ | Code.Ldarg_S, _ ->
                let param = instr.Operand :?> ParameterDefinition
                ldarg param.Index

            | Code.Ldarg_0, _ | Code.Ldarg_1, _ | Code.Ldarg_2, _ | Code.Ldarg_3, _ ->
                int instr.OpCode.Code - int Code.Ldarg_0 |> ldarg
            
            | Code.Starg, value :: rest | Code.Starg_S, value :: rest ->
                let arg = instr.Operand :?> ParameterDefinition
                let argi = arg.Index
                warning "starg: no verification"
                let nvalue :: nstack = nstack
                let nvalue = getValue value nvalue
                let tvalue = truncate nvalue arg.ParameterType
                gen.Store(tvalue, nargs.[argi]) |> ignore
                rest, nstack, true
            
            | Code.Ldloc, _ | Code.Ldloc_S, _ ->
                let loc = (instr.Operand :?> VariableDefinition).Index
                ldloc loc
            
            | Code.Ldloc_0, _ | Code.Ldloc_1, _ | Code.Ldloc_2, _ | Code.Ldloc_3, _ ->
                instr.OpCode.Code - Code.Ldloc_0 |> int |> ldloc
            
            | Code.Stloc, item :: stack | Code.Stloc_S, item :: stack ->
                let loc = (instr.Operand :?> VariableDefinition).Index
                warning "stloc: no verification"
                stloc (item, nstack.Head) loc
            
            | Code.Stloc_0, item :: stack | Code.Stloc_1, item :: stack
            | Code.Stloc_2, item :: stack | Code.Stloc_3, item :: stack ->
                int instr.OpCode.Code - int Code.Stloc_0 |> stloc (item, nstack.Head)
            
            | Code.Ldarga, _ | Code.Ldarga_S, _ ->
                let param = instr.Operand :?> ParameterDefinition
                let value = nargs.[if hlmethod.HasThis then param.Index else param.Index + 1]
                let stackItem = Reference(param.ParameterType)
                let value = newValue stackItem value
                stackItem :: stack,
                value :: nstack, true
            
            | Code.Ldloca, _ | Code.Ldloca_S, _ ->
                let var = instr.Operand :?> VariableDefinition
                let stackItem = Reference(var.VariableType)
                let value =  newValue stackItem nvars.[var.Index]
                stackItem :: stack,
                value :: nstack, true
            //#endregion
            
            //#region loading constants
            | Code.Ldc_I4_0, _ | Code.Ldc_I4_1, _ | Code.Ldc_I4_2, _ | Code.Ldc_I4_3, _
            | Code.Ldc_I4_4, _ | Code.Ldc_I4_5, _ | Code.Ldc_I4_6, _ | Code.Ldc_I4_7, _
            | Code.Ldc_I4_8, _ ->
                int instr.OpCode.Code - int Code.Ldc_I4_0 |> ldc4
            
            | Code.Ldc_I4_M1, _ ->
                ldc4 -1
            
            | Code.Ldc_I4, _ | Code.Ldc_I4_S, _ ->
                System.Convert.ToInt32(instr.Operand) |> ldc4
            
            | Code.Ldc_R4, _ ->
                not_implemented("noargs") |> raise
            
            | Code.Ldc_R8, _ ->
                not_implemented("noargs") |> raise
            
            | Code.Ldc_I8, _ ->
                let c8 = i64.Constant(instr.Operand |> System.Convert.ToInt64 |> uint64, false)
                I64 :: stack, upcast c8 :: nstack, true
            
            | Code.Ldnull, _ ->
                let cnative = newValue (Object null) pvoid.Zero
                StackItem.Object(null) :: stack, cnative :: nstack, true
            //#endregion loading constants
            
            | Code.Ldftn, _ ->
                not_implemented("noargs") |> raise
            
            //#region Ldind Num
            | Code.Ldind_I, Reference(t) :: stack ->
                warning "ldind: are we need a pointer cast?"
                warning "ldind: verification"
                let addr :: nstack = nstack
                let value = gen.Load(addr, "")
                Native :: stack, upcast value :: nstack, true
            | Code.Ldind_I1, Reference(t) :: stack ->
                warning "ldind: are we need a pointer cast?"
                warning "ldind: verification"
                let addr :: nstack = nstack
                let value = gen.Load(addr, "")
                let value = gen.SignExtend(value, i32, "")
                Native :: stack, value :: nstack, true
            | Code.Ldind_U1, Reference(t) :: stack ->
                warning "ldind: are we need a pointer cast?"
                warning "ldind: verification"
                let addr :: nstack = nstack
                let value = gen.Load(addr, "")
                let value = gen.ZeroExtend(value, i32, "")
                Native :: stack, value :: nstack, true
            | Code.Ldind_I2, Reference(t) :: stack ->
                warning "ldind: are we need a pointer cast?"
                warning "ldind: verification"
                let addr :: nstack = nstack
                let value = gen.Load(addr, "")
                let value = gen.SignExtend(value, i32, "")
                Native :: stack, value :: nstack, true
            | Code.Ldind_U2, Reference(t) :: stack ->
                warning "ldind: are we need a pointer cast?"
                warning "ldind: verification"
                let addr :: nstack = nstack
                let value = gen.Load(addr, "")
                let value = gen.ZeroExtend(value, i32, "")
                Native :: stack, value :: nstack, true
            | Code.Ldind_I4, Reference(t) :: stack
            | Code.Ldind_U4, Reference(t) :: stack ->
                warning "ldind: are we need a pointer cast?"
                warning "ldind: verification"
                warning "ldind_*4: threated the same, but have different opcodes"
                let addr :: nstack = nstack
                let value = gen.Load(addr, "")
                Native :: stack, upcast value :: nstack, true
            | Code.Ldind_I8, Reference(t) :: stack ->
                warning "ldind: are we need a pointer cast?"
                warning "ldind: verification"
                let addr :: nstack = nstack
                let value = gen.Load(addr, "")
                Native :: stack, upcast value :: nstack, true
            | Code.Ldind_R4, Reference(t) :: stack ->
                not_implemented("noargs") |> raise
            | Code.Ldind_R8, Reference(t) :: stack ->
                not_implemented("noargs") |> raise
            | Code.Ldind_Ref, Reference(t) :: stack ->
                not_implemented("noargs") |> raise
            //#endregion Ldind Num

            //#region Stind Num
            | Code.Stind_I, Native :: Reference(t) :: stack ->
                warning "stind: are we need a pointer cast?"
                warning "stind: verification"
                let value :: addr :: nstack = nstack
                gen.Store(value, addr) |> ignore
                stack, nstack, true
            | Code.Stind_I1, I32 :: Reference(t) :: stack ->
                warning "stind: are we need a pointer cast?"
                warning "stind: verification"
                let value :: addr :: nstack = nstack
                let value = gen.Trunc(value, i8, "")
                gen.Store(value, addr) |> ignore
                stack, nstack, true
            | Code.Stind_I2, I32 :: Reference(t) :: stack ->
                warning "stind: are we need a pointer cast?"
                warning "stind: verification"
                let value :: addr :: nstack = nstack
                let value = gen.Trunc(value, i16, "")
                gen.Store(value, addr) |> ignore
                stack, nstack, true
            | Code.Stind_I4, I32 :: Reference(t) :: stack ->
                warning "stind: are we need a pointer cast?"
                warning "stind: verification"
                let value :: addr :: nstack = nstack
                gen.Store(value, addr) |> ignore
                stack, nstack, true
            | Code.Stind_I8, I64 :: Reference(t) :: stack ->
                warning "stind: are we need a pointer cast?"
                warning "stind: verification"
                let value :: addr :: nstack = nstack
                gen.Store(value, addr) |> ignore
                stack, nstack, true
            | Code.Stind_Ref, Object(a) :: Reference(t) :: stack ->
                warning "stind: are we need a pointer cast?"
                warning "stind: verification"
                let value :: addr :: nstack = nstack
                gen.Store(value, addr) |> ignore
                stack, nstack, true
            | Code.Stind_R4, Float :: Reference(t) :: stack
            | Code.Stind_R8, Float :: Reference(t) :: stack ->
                not_implemented("noargs") |> raise
            //#endregion Stind Num
            
            | Code.Nop, _ ->
                stack, nstack, true
            
            | Code.Ret, [] when isVoid hlmethod.ReturnType ->
                gen.Return() |> ignore
                [], [], true

            | Code.Ret, value :: [] when not <| isVoid hlmethod.ReturnType ->
                warning "ret: void is detected by type name only"
                let nvalue :: _ = nstack
                let nvalue = getValue value nvalue
                gen.Return nvalue |> ignore
                [], [], true

            | Code.Ret, _ ->
                System.InvalidProgramException("Invalid stack state on ret instruction") |> raise
            
            | Code.Switch, I32 :: stack ->
                not_implemented("noargs") |> raise
            
            
            //#region object instructions
            | Code.Box, Object(t) :: stack ->
                let target = instr.Operand :?> TypeReference
                let verifiable = target != t
                match target with
                | ReferenceType(_) ->
                    GetStackItem target :: stack, nstack, verifiable
                | NullableType(ofT) ->
                    not_implemented("noargs") |> raise
                    not_implemented("unbox.*") |> raise
                | _ ->
                    let boxtype = GenericInstanceType(boxType)
                    boxtype.GenericArguments.Add(target)
                    let initgen = boxType.Methods |> Seq.find (fun m -> m.IsConstructor)
                    let init = MethodReference(initgen.Name, initgen.ReturnType, boxtype,
                                HasThis = initgen.HasThis, ExplicitThis = initgen.ExplicitThis,
                                CallingConvention = initgen.CallingConvention)
                    init.Parameters.Add(ParameterDefinition(t))
                    warning "box: virtual method table"
                    newObj init

            | Code.Unbox, Object(t) :: stack ->
                not_implemented("noargs") |> raise

            | Code.Unbox_Any, Object(t) :: stack ->
                let target = instr.Operand :?> TypeReference
                Warnings.NoVerification "unbox.any"
                let nbox :: nstack = nstack
                let value =
                    match t with
                    | _ when t.Resolve() = typeof<ObjectModel.ObjectBox<_>>.Definition ->
                        if not target.IsValueType then
                            notImplemented "unbox.any(value type) -> interface"
                        let box = gen.Load(nbox, "")
                        gen.Extract(box, 1)
                    | _ ->
                        if target = t then nbox
                        else gen.PointerCast(nbox, this.Morf target :?> PointerType)
                GetStackItem target :: stack, value :: nstack, false

            | Code.Callvirt, stack ->
                warning "virt calls ignored"
                makeCall stack

            | Code.Castclass, Object(t) :: stack ->
                let target = instr.Operand :?> TypeReference
                match target with
                | NullableType(ofT) ->
                    not_implemented("noargs") |> raise
                | _ ->
                    not_implemented("noargs") |> raise

            | Code.Cpobj, IsPointer src :: IsPointer dst :: stack ->
                not_implemented("noargs") |> raise

            | Code.Initobj, IsPointer dest :: stack ->
                let objtype = instr.Operand :?> TypeReference
                let ntype = this.Morf(objtype)
                let zero = ntype.Zero
                let target :: nstack = nstack
                let target = getValue dest target
                let store = gen.Store(zero, target)
                warning "initobj: no verification"
                stack, nstack, true

            | Code.Isinst, Object(t) :: stack ->
                let target = instr.Operand :?> TypeReference
                match target with
                | NullableType(ofT) ->
                    not_implemented("noargs") |> raise
                | _ ->
                    not_implemented("noargs") |> raise

            //#region Ldelem
            | Code.Ldelem_I1, IsIndex(idx) :: Object(t) :: stack ->
                not_implemented("noargs") |> raise

            | Code.Ldelem_U1, IsIndex(idx) :: Object(t) :: stack ->
                not_implemented("noargs") |> raise

            | Code.Ldelem_I2, IsIndex(idx) :: Object(t) :: stack ->
                not_implemented("noargs") |> raise

            | Code.Ldelem_U2, IsIndex(idx) :: Object(t) :: stack ->
                not_implemented("noargs") |> raise

            | Code.Ldelem_I4, IsIndex(idx) :: Object(t) :: stack ->
                not_implemented("noargs") |> raise

            | Code.Ldelem_U4, IsIndex(idx) :: Object(t) :: stack ->
                not_implemented("noargs") |> raise

            | Code.Ldelem_I8, IsIndex(idx) :: Object(t) :: stack ->
                not_implemented("noargs") |> raise

            | Code.Ldelem_R4, IsIndex(idx) :: Object(t) :: stack ->
                not_implemented("noargs") |> raise

            | Code.Ldelem_R8, IsIndex(idx) :: Object(t) :: stack ->
                not_implemented("noargs") |> raise

            | Code.Ldelem_I, IsIndex(idx) :: Object(t) :: stack ->
                not_implemented("noargs") |> raise

            | Code.Ldelem_Ref, IsIndex(idx) :: Object(t) :: stack ->
                not_implemented("noargs") |> raise

            | Code.Ldelem_Any, IsIndex(idx) :: Object(t) :: stack ->
                let nidx :: narrGC :: nstack = nstack
                let elemPtr = elementRef (Object t) narrGC nidx
                let nvalue = gen.Load(elemPtr, "elem") :> Value
                let elemT =
                    match t with
                    | :? ArrayType as array -> array.ElementType
                    | _ -> invalid_code "Ldelem_Any is called on non-array" |> raise
                GetStackItem elemT :: stack,
                nvalue :: nstack, true
            //#endregion Ldelem

            //#region Stelem
            | Code.Stelem_Any, value :: IsIndex idx :: Object(t) :: stack ->
                let nvalue :: nidx :: narrGC :: nstack = nstack
                let elemPtr = elementRef (Object t) narrGC nidx
                // let elemPtr = gen.Add(nidx, firstElem, "elemPtr")
                gen.Store(nvalue, elemPtr) |> ignore
                stack, nstack, true

            | Code.Stelem_I1, value :: IsIndex idx :: Object(t) :: stack ->
                not_implemented("noargs") |> raise

            | Code.Stelem_I2, value :: IsIndex idx :: Object(t) :: stack ->
                not_implemented("noargs") |> raise

            | Code.Stelem_I4, value :: IsIndex idx :: Object(t) :: stack ->
                not_implemented("noargs") |> raise

            | Code.Stelem_I8, value :: IsIndex idx :: Object(t) :: stack ->
                not_implemented("noargs") |> raise

            | Code.Stelem_R4, value :: IsIndex idx :: Object(t) :: stack ->
                not_implemented("noargs") |> raise

            | Code.Stelem_R8, value :: IsIndex idx :: Object(t) :: stack ->
                not_implemented("noargs") |> raise

            | Code.Stelem_I, value :: IsIndex idx :: Object(t) :: stack ->
                not_implemented("noargs") |> raise

            | Code.Stelem_Ref, value :: IsIndex idx :: Object(t) :: stack ->
                not_implemented("noargs") |> raise
            //#endregion Stelem

            | Code.Ldelema, IsIndex(idx) :: Object(t) :: stack ->
                not_implemented("noargs") |> raise

            //#region Field Access
            | Code.Ldfld, Object(t) :: stack ->
                let field = instructionField()
                let fid = field |> fid
                let objValue :: nstack = nstack
                let objValue = getValue (Object t) objValue
                Warnings.NoVerification "ldfld"
                Warnings.NoTypeMixing "ldfld"
                if t.IsValueType then
                    if t.IsPrimitive then
                        let targetType =
                            field.FieldType
                            |> this.Morf
                        let nvalue =
                            if targetType.Kind = TypeKind.Pointer then
                                gen.IntToPointer(objValue, downcast targetType)
                            else
                                objValue
                        GetStackItem t :: stack, nvalue :: nstack, false
                    else
                    StackItem.Object(t) :: stack, gen.extract fid nstack, true
                else
                    ldfldRef (Object t) stack
                    // not_implemented("noargs") |> raise

            | Code.Ldfld, Reference(t) :: stack ->
                ldfldRef (Reference t) stack

            | Code.Ldfld, Native :: stack ->
                not_implemented("noargs") |> raise

//            | Code.Stfld, value :: Object(t) :: stack ->
//                warning "stfld: no verification"
//                warning "stfld: no type mixing"
//                let field = instr.Operand :?> FieldReference |> fid
//                let nvalue :: nobj :: nstack = nstack
//                let nvalue = getValue value nvalue
//                let nobj = getValue (Object t) nobj
//                gen.Insert(nobj, nvalue, field) |> ignore
//                stack, nstack, true

            | Code.Stfld, valueT :: (Object(_) as n) :: stack
            | Code.Stfld, valueT :: (Reference(_) as n) :: stack ->
                warning "stfld: no verification"
                warning "stfld: no type mixing"
                let field = instructionField()

                let value :: ref :: nstack = nstack

                let ptr =
                    let targetType =
                        let ntype = this.Morf <| typeMap field.DeclaringType
                        if field.DeclaringType.IsValueType then PointerType.Get ntype
                        else downcast ntype
                    if field.DeclaringType.IsPrimitive then
                        gen.PointerCast(ref, targetType)
                    else
                    let ref = getValue n ref
                    let ref = gen.PointerCast(ref, targetType)
                    gen.StructureElement(ref, fid field, "")

                let tvalue =
                    if valueT = Object null then
                        (ptr.Type :?> PointerType).ElementType.Zero :> Value
                    else
                        let value = getValue valueT value
                        truncate value field.FieldType

                gen.Store(tvalue, ptr) |> ignore
                stack, nstack, true

            | Code.Stfld, value :: Native :: stack ->
                not_implemented("noargs") |> raise

            | Code.Stsfld, value :: stack ->
                let field = instructionField()

                let nvalGC :: nstack = nstack
                let nval = getValue value nvalGC 
                let globalRef = staticFields.[field.FullName]
                let upcastVal = genUpcast value field.FieldType nval

                gen.Store(upcastVal, globalRef) |> ignore

                stack, nstack, true

            | Code.Ldflda, Object(t) :: stack ->
                not_implemented("noargs") |> raise

            | Code.Ldflda, Reference(t) :: stack ->
                let field = instructionField()
                let ref :: nstack = nstack
                let ref = getValue (Reference t) ref
                let ptr = gen.StructureElement(ref, fid field, "")
                let ptr = newValue (Reference field.FieldType) ptr
                Reference(field.FieldType) :: stack, ptr :: nstack, true

            | Code.Ldflda, Native :: stack ->
                not_implemented("noargs") |> raise

            | Code.Ldsfld, stack ->
                let field = instructionField()
                //this.Morf field.DeclaringType |> ignore
                let sfields = staticFields
                let exists, nfield = staticFields.TryGetValue(field.FullName)
                if not exists then
                    not_implemented("noargs") |> raise
                else
                let stackItem = GetStackItem field.FieldType
                let nvalue = newValue stackItem (gen.Load nfield)
                stackItem :: stack, nvalue :: nstack, true

            | Code.Ldsflda, stack ->
                not_implemented("noargs") |> raise
            //#endregion Field Access

            | Code.Ldlen, Object(arr) :: stack ->
                warning "no verification"
                let narr :: nstack = nstack
                let narr = getValue (Object arr) narr
                let ptr = gen.StructureElement(narr, arrayLengthIndex, "lenptr")
                let len: Value = upcast gen.Load(ptr, "len")
                Native :: stack, len :: nstack, true
                // not_implemented("noargs") |> raise

            | Code.Ldobj, IsPointer src :: stack ->
                not_implemented("noargs") |> raise

            | Code.Stobj, value :: IsPointer dest :: stack ->
                not_implemented("noargs") |> raise

            | Code.Ldstr, stack ->
                warning "Ldstr is always null"
                let cnative = newValue (Object null) pvoid.Zero
                StackItem.Object(null) :: stack, cnative :: nstack, true

            | Code.Ldtoken, stack ->
                not_implemented("noargs") |> raise

            | Code.Newarr, IsIndex num :: stack ->
                if isNull gc then notSupported "Heap is not supported. Please, specify a GC."

                let elemType = typeMap(downcast instr.Operand)
                let nelem = this.Morf elemType
                let arrType = ArrayType(elemType)
                let narr = this.Morf arrType :?> PointerType

                let nsize :: nstack = nstack

                // TODO: quotations to LLVM
                // TODO: align! (this can be done by creating a struct,
                // consisting of arrObj and one item of elemType
                // TODO: convert with overflow!
                let nsize = convNative "arrSize" num nsize
//                let elemSize = convNIU "elemSize" nelem.Size native
//                let dataSize = gen.Multiply(nsize, elemSize, "dataSize")
//                let objSize = gen.Add(dataSize, narr.ElementType.Size, "arrObjSize")

                let size = elementRefNoGC narr.Zero nsize
                let objSize = gen.PointerToInt(size, native, "toAlloc")

                // TODO: align!
                let alignType = gc_new.Arguments(1).Type :?> IntegerType
                let align = convNIU "gc_align" narr.ElementType.Align alignType 
                let ptr = gen.Call(gc_new, [| objSize; align |], "gcN")
                let ptr = gen.IntToPointer(ptr, narr, "gcPtr")

                gen.Store(narr.ElementType.Zero, ptr) |> ignore
                let lenPtr = gen.StructureElement(ptr, arrayLengthIndex, "lenptr")
                gen.Store(nsize, lenPtr) |> ignore

                // TODO: zero the array
                let stackType = GetStackItem arrType
                let ptr = newValue stackType ptr

                stackType :: stack, ptr :: nstack, true

            | Code.Newobj, stack ->
                let newObjInitUnresolved = instr.Operand :?> MethodReference
                let newObjInit = resolveMethodReference newObjInitUnresolved
                newObj newObjInit

            | Code.Mkrefany, IsPointer ptr :: stack ->
                not_implemented("noargs") |> raise

            | Code.Refanytype, Reference(t) :: stack ->
                not_implemented("noargs") |> raise

            | Code.Refanyval, Reference(t) :: stack ->
                not_implemented("noargs") |> raise

            | Code.Rethrow, stack ->
                not_implemented("noargs") |> raise

            | Code.Sizeof, stack ->
                not_implemented("noargs") |> raise

            | Code.Throw, Object(e) :: stack ->
                gen.Call(dbgbreak) |> ignore
                gen.GoTo(blocks.[0]) |> ignore
                stack, nstack.Tail, true
                //not_implemented("noargs") |> raise

            //#endregion object instructions
            
            | Code.Volatile, stack ->
                warning "not implemented: volatile instruction"
                stack, nstack, true

            | _ ->
                let failWithInvalidInstruction() =
                    System.InvalidProgramException(sprintf "Invalid instruction: %A" instr)
                    |> raise

                if translators = [] then
                    failWithInvalidInstruction()
                else
                let translationContext =
                    {   TypeMap = typeMap
                        NativeContext = context
                        ParentTranslator = translator()
                        Method = hlmethod   }
                let translationState =
                    {   Stack = stack
                        NativeStack = nstack
                        Errors = []
                        Verifiable = true  }
                match translators |> List.tryPick (fun t -> t.Translate translationContext translationState instr) with
                | Some(state) ->
                    state.Stack, state.NativeStack, state.Verifiable
                | _ -> failWithInvalidInstruction()
        
//        let nextInstruction (current: Instruction) =
//            if current = null then
//                result.Body.Instructions.[0]
//            else
//                current.Next

        let vtranslate (stack, nstack, verifiable) instr =
            let stack, nstack, verstatus = translateInternal stack nstack instr

            assert(nstack |> Seq.forall2(fun hl n ->
                not(isGCRef hl) || n.Type.Kind = TypeKind.Pointer)
                stack
            )
            assert(nstack.Length = stack.Length)
            assert(nstack |> Seq.forall(fun v ->
                match v.Type with
                | :? IntegerType as itype ->
                    itype.Width = 32 || itype.Width = 64
                    || itype.Width = nativeBits
                | _ -> true
            ))

            stack, nstack, verstatus && verifiable

        hlmethod.Body.Instructions
        |> Seq.fold vtranslate ([], [], true)
        |> ignore

        prologue.GoTo(if loop then entryLoop else blocks.[0]) |> ignore
        
        let dump = nfunc.PrintToString()
        Debug.Print(dump)

        nfunc
