namespace CodeBlock.GC

open Mono.Cecil

open LLVM.GarbageCollection

type TestGC() =
    inherit GarbageCollector()

    do
        printfn "TestGC()"

    override this.InitializeCustomLowering(unit) =
        printfn "InitializeCustomLowering for %A" unit
        false

    override this.PerformCustomLowering(func) =
        printfn "PerformCustomLowering on %A" func
        false

    override this.FindCustomSafePoints(funcInfo, machineFunc) =
        printfn "FindCustomSafePoints %A %A" funcInfo machineFunc
        false

    override this.Name = "TestGC"

//type SimpleGC =
//    struct
//        val  