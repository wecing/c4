/// Make IR compliant to X86-64 calling convention.
module X86Rewrite

open C4.Ir

type private Instr = Proto.BasicBlock.Types.Instruction
type private Terminator = Proto.BasicBlock.Types.Terminator
type private K = Instr.Types.Kind
type private TK = Proto.Type.Types.Kind
type private TermK = Terminator.Types.Kind
type private VC = Proto.Value.VOneofCase

type private EightByteClass = INTEGER | SSE | NO_CLASS | MEMORY

// using x86_64 ABI v0.21
let private classifyTp (ir: Proto.IrModule) (tp: Proto.Type)
                       : EightByteClass list =
    let nextTp : unit -> (EightByteClass * uint) option =
        let stack = ref [(tp, false)]
        let rec f = fun () ->
            match !stack with
            | [] -> None
            | (tp, paddingOnly) :: tps when tp.Kind = TK.Struct ->
                let structDef = ir.StructDefs.[tp.StructId]
                let fields =
                    Seq.zip structDef.Type structDef.PaddingOnly |> List.ofSeq
                let fields =
                    if paddingOnly then
                        fields |> List.map (fun x -> fst x, true)
                    else
                        fields
                stack := fields @ tps
                f()
            | (tp, paddingOnly) :: tps when tp.Kind = TK.Array ->
                if tp.ArraySize > 16u then
                    // early return: if array size exceeds two eightbytes, the
                    // whole struct must also be larger than two eightbytes.
                    stack := []
                    Some (MEMORY, 1u) // 1u is a dummy value
                else
                    let tps = List.replicate (min 16u tp.ArraySize |> int)
                                             (tp.ArrayElemType, paddingOnly)
                    stack := tps @ tps
                    f()
            | (tp, paddingOnly) :: tps ->
                stack := tps
                let (clazz, sz) =
                    match tp.Kind with
                    | TK.Int8 -> (INTEGER, 1u)
                    | TK.Int16 -> (INTEGER, 2u)
                    | TK.Int32 -> (INTEGER, 4u)
                    | TK.Int64 -> (INTEGER, 8u)
                    | TK.Float -> (SSE, 4u)
                    | TK.Double -> (SSE, 8u)
                    | TK.Pointer -> (INTEGER, 8u)
                    | _ -> failwith "illegal protobuf message"
                if paddingOnly then
                    Some (NO_CLASS, sz)
                else
                    Some (clazz, sz)
        f

    let sz = ref 0u
    let r = ref [NO_CLASS]
    let classified = ref false
    while not !classified do
        match nextTp() with
        | None -> classified := true
        | Some (clazz, s) ->
            sz := !sz + s
            if !sz > 16u then
                r := [MEMORY]
                classified := true
            elif !sz > 8u && !sz - s = 8u then
                r := !r @ [clazz]
            else
                let prevClazz = List.last !r
                let newClazz =
                    match prevClazz, clazz with
                    | (NO_CLASS, x) | (x, NO_CLASS) -> x
                    | (MEMORY, _) | (_, MEMORY) -> MEMORY
                    | (INTEGER, _) | (_, INTEGER) -> INTEGER
                    | (SSE, SSE) -> SSE
                r :=
                    match !r with
                    | _ when newClazz = MEMORY ->
                        classified := true
                        [MEMORY]
                    | [_] -> [newClazz]
                    | [x; _] -> [x; newClazz]
                    | _ -> failwith "unreachable"
    !r

let rec private sizeofTp (ir: Proto.IrModule) (tp: Proto.Type) : uint =
    match tp.Kind with
    | TK.Int8 -> 1u
    | TK.Int16 -> 2u
    | TK.Int32 -> 4u
    | TK.Int64 -> 8u
    | TK.Float -> 4u
    | TK.Double -> 8u
    | TK.Struct ->
        let sd = ir.StructDefs.[tp.StructId]
        sd.Type |> Seq.map (sizeofTp ir) |> Seq.sum
    | TK.Pointer -> 8u
    | TK.Array -> sizeofTp ir tp.ArrayElemType * tp.ArraySize
    | _ -> failwith "illegal protobuf message"

// rewrite fn arg passing & value returning & call instrs
let private rewriteFn (ir: Proto.IrModule) (fn: Proto.FunctionDef) =
    fn.ArgByval.Clear()
    fn.ArgTypes |> Seq.iter (fun _ -> fn.ArgByval.Add(false))

    let remIntRegs = ref 6 // rdi, rsi, rdx, rcx, r8, r9
    let remSseRegs = ref 8 // xmm0 - xmm7

    // this could be empty for fn decls
    let irInstrs : Map<uint, Instr> =
        fn.Bbs.Values
        |> Seq.collect (fun bb -> bb.Instructions)
        |> Seq.map (fun x -> (x.Id, x))
        |> Map.ofSeq

    // x86 rewrite must happen before MemToReg, so no phi here
    let nextIrId : unit -> uint =
        let irIds =
            irInstrs
            |> Map.toSeq
            |> Seq.map fst
            |> Seq.append fn.Args
            |> Set.ofSeq
            |> ref
        let lastIrId = Set.count !irIds |> uint |> ref
        fun () ->
            lastIrId := !lastIrId + 1u
            while Set.contains !lastIrId !irIds do
                lastIrId := !lastIrId + 1u
            irIds := Set.add !lastIrId !irIds
            !lastIrId

    let insertAfter (oldInstr: Instr) (newInstr: Instr) =
        let (bbId, instrIdx) =
            fn.Bbs
            |> Seq.map (fun (KeyValue (bbId, bb)) ->
                bb.Instructions
                |> Seq.tryFindIndex (fun x -> x.Id = oldInstr.Id)
                |> Option.map (fun idx -> bbId, idx))
            |> Seq.find Option.isSome
            |> Option.get
        fn.Bbs.[bbId].Instructions.Insert(instrIdx + 1, newInstr)

    let insertBefore (oldInstr: Instr) (newInstr: Instr) =
        let (bbId, instrIdx) =
            fn.Bbs
            |> Seq.map (fun (KeyValue (bbId, bb)) ->
                bb.Instructions
                |> Seq.tryFindIndex (fun x -> x.Id = oldInstr.Id)
                |> Option.map (fun idx -> bbId, idx))
            |> Seq.find Option.isSome
            |> Option.get
        fn.Bbs.[bbId].Instructions.Insert(instrIdx, newInstr)

    // rewrite value returning first
    if fn.ReturnType.Kind = TK.Struct &&
       classifyTp ir fn.ReturnType = [MEMORY] then
        // from:
        //   &T f():
        //     %v = load %p
        //     ret %v
        // to:
        //   &T* f(&T* %r):
        //     memcpy %r, %p
        //     ret %r
        remIntRegs := !remIntRegs - 1
        let rt = Proto.Type()
        rt.PointeeType <- fn.ReturnType
        rt.Kind <- TK.Pointer
        fn.ReturnType <- rt
        let hiddenArg = nextIrId()
        fn.Args.Insert(0, hiddenArg)
        fn.ArgTypes.Insert(0, rt.Clone())
        fn.ArgByval.Insert(0, false)

        for bb in fn.Bbs.Values do
            if bb.Terminator.Kind = TermK.Return then
                let loadInstr = irInstrs.[bb.Terminator.ReturnValue.IrId]
                bb.Terminator.ReturnValue.IrId <- hiddenArg
                bb.Terminator.ReturnValue.Type <- rt.Clone()
                // loadInstr -> memcpyInstr
                loadInstr.Kind <- K.Memcpy
                loadInstr.MemcpySrc <- loadInstr.LoadSrc
                loadInstr.LoadSrc <- null
                loadInstr.MemcpyDst <- Proto.Value()
                loadInstr.MemcpyDst.IrId <- hiddenArg
                loadInstr.MemcpyDst.Type <- rt.Clone()
                loadInstr.MemcpySize <- sizeofTp ir rt.PointeeType
    // keep using the load+ret pattern if &T could be passed via regs

    let stores =
        irInstrs
        |> Map.toSeq
        |> Seq.map snd
        |> Seq.filter
            (fun x ->
                x.Kind = K.Store
                && x.StoreSrc.VCase = VC.IrId
                && Seq.contains x.StoreSrc.IrId fn.Args
                && x.StoreSrc.Type.Kind = TK.Struct)
    let args =
        Seq.zip fn.Args fn.ArgTypes
        |> Seq.indexed
        |> Seq.filter (fun p -> (snd (snd p)).Kind = TK.Struct)
    for (argIdx, (argIrId, argTp)) in args do
        // from:
        //   f(&T %arg1):
        //     %p = alloca &T
        //     store &T %arg1, &T* %p
        //
        // to either:
        //   f(i64 %arg1_p1, i64 %arg_p2):
        //     %p = alloca &T
        //     store i64, %arg1_p1, i64* (%p as i64* + 0)
        //     store i64, %arg1_p2, i64* (%p as i64* + 8)
        //
        // or:
        //   f(byval &T* %arg1):
        //     %p = %arg1
        let cs = classifyTp ir argTp
        let splitStore =
            if cs = [MEMORY] then
                false
            else
                let intRegs =
                    cs |> Seq.filter (fun c -> c = INTEGER) |> Seq.length
                let sseRegs = cs |> Seq.filter (fun c -> c = SSE) |> Seq.length
                if !remIntRegs >= intRegs && !remSseRegs >= sseRegs then
                    remIntRegs := !remIntRegs - intRegs
                    remSseRegs := !remSseRegs - sseRegs
                    true
                else
                    false
        if splitStore then
            let insertOrUpdateArg (irIdOpt: uint option)
                          (clazz: EightByteClass) : uint =
                let tp = Proto.Type()
                tp.Kind <- if clazz = INTEGER then TK.Int64 else TK.Double
                match irIdOpt with
                | Some x ->
                    fn.ArgTypes.[argIdx] <- tp
                    x
                | None ->
                    let irId = nextIrId()
                    fn.Args.Insert(argIdx + 1, irId)
                    fn.ArgTypes.Insert(argIdx + 1, tp)
                    fn.ArgByval.Insert(argIdx + 1, false)
                    irId
            let args =
                Seq.zip cs [0; 8]
                |> Seq.filter (fun (c, _) -> c <> NO_CLASS)
                |> Seq.zip [Some argIrId; None]
                |> Seq.map (fun (irIdOpt, (clazz, offset)) -> 
                    (insertOrUpdateArg irIdOpt clazz, clazz, offset))

            if fn.EntryBb <> 0u then
                let store =
                    stores |> Seq.find (fun x -> x.StoreSrc.IrId = argIrId)
                let alloc = irInstrs.[store.StoreDst.IrId]

                let newStore(): Instr =
                    let s = Instr()
                    s.Id <- nextIrId()
                    s.Type <- Proto.Type()
                    s.Type.Kind <- TK.Void
                    s.Kind <- K.Store
                    // StoreDst/Src will be fully initialized later
                    s.StoreDst <- Proto.Value()
                    s.StoreSrc <- Proto.Value()
                    insertAfter store s
                    s

                let processStore (s: Instr)
                                 (irId: uint)
                                 (clazz: EightByteClass)
                                 (offset: int) =
                    let tp = Proto.Type()
                    tp.Kind <- if clazz = SSE then TK.Double else TK.Int64
                    let ptrTp = Proto.Type()
                    ptrTp.Kind <- TK.Pointer
                    ptrTp.PointeeType <- tp
                    
                    s.StoreSrc.IrId <- irId
                    s.StoreSrc.Type <- tp.Clone()

                    let dstValue =
                        let dstAddr = Instr()
                        dstAddr.Id <- nextIrId()
                        dstAddr.Type <- ptrTp.Clone()
                        dstAddr.Kind <- K.Add
                        dstAddr.BinOpLeft <- Proto.Value()
                        dstAddr.BinOpLeft.Type <- ptrTp.Clone()
                        dstAddr.BinOpLeft.CastFrom <- Proto.Value()
                        dstAddr.BinOpLeft.CastFrom.Type <- alloc.Type.Clone()
                        dstAddr.BinOpLeft.CastFrom.IrId <- alloc.Id
                        dstAddr.BinOpRight <- Proto.Value()
                        dstAddr.BinOpRight.Type <- Proto.Type()
                        dstAddr.BinOpRight.Type.Kind <- TK.Int64
                        dstAddr.BinOpRight.I64 <- int64 offset
                        insertBefore store dstAddr
                        let v = Proto.Value()
                        v.Type <- ptrTp.Clone()
                        v.IrId <- dstAddr.Id
                        v
                    s.StoreDst <- dstValue

                Seq.zip [Some store; None] args
                |> Seq.map (fun (s, x) -> (Option.defaultWith newStore s, x))
                |> Seq.iter (fun (a, (b, c, d)) -> processStore a b c d)
        else
            fn.ArgTypes.[argIdx] <- Proto.Type()
            fn.ArgTypes.[argIdx].Kind <- TK.Pointer
            fn.ArgTypes.[argIdx].PointeeType <- argTp
            fn.ArgByval.[argIdx] <- true
            let argTp = fn.ArgTypes.[argIdx]
            if fn.EntryBb <> 0u then
                let store =
                    stores |> Seq.find (fun x -> x.StoreSrc.IrId = argIrId)
                let alloc = irInstrs.[store.StoreDst.IrId]
                // alloc -> value alias
                alloc.Kind <- K.Value
                alloc.Value <- Proto.Value()
                alloc.Value.Type <- argTp.Clone()
                alloc.Value.IrId <- argIrId
                // store -> undef
                store.Kind <- K.Value
                store.Type <- store.StoreSrc.Type
                store.Value <- Proto.Value()
                store.Value.Type <- store.Type.Clone()
                store.Value.Undef <- true
                store.StoreSrc <- null
                store.StoreDst <- null

    let rewriteCallInstrs() =
        let callInstrs =
            irInstrs
            |> Map.toSeq
            |> Seq.map snd
            |> Seq.filter (fun x -> x.Kind = K.FnCall)
        for call in callInstrs do
            call.FnCallArgByval.Clear()
            call.FnCallArgs
            |> Seq.iter (fun _ -> call.FnCallArgByval.Add(false))

            let remIntRegs = ref 6 // rdi, rsi, rdx, rcx, r8, r9
            let remSseRegs = ref 8 // xmm0 - xmm7

            // from:
            //   %p = alloca &T
            //   %v = call @f(...)
            //   store %v, %p
            // to:
            //   %p = alloca
            //   %v = call @f(%p, ...)
            let rt = call.FnCallFn.Type.PointeeType.FnReturnType
            if rt.Kind = TK.Struct && classifyTp ir rt = [MEMORY] then
                remIntRegs := !remIntRegs - 1
                let store =
                    irInstrs
                    |> Map.toSeq
                    |> Seq.map snd
                    |> Seq.find (fun x ->
                        x.Kind = K.Store && x.StoreSrc.IrId = call.Id)
                call.FnCallArgs.Insert(0, store.StoreDst)
                call.FnCallArgByval.Insert(0, false)
                call.Type <- store.StoreDst.Type.Clone()
                store.Kind <- K.Value
                store.Type <- Proto.Type()
                store.Type.Kind <- TK.Int8
                store.StoreSrc <- null
                store.StoreDst <- null
                store.Value <- Proto.Value()
                store.Value.Type <- store.Type.Clone()
                store.Value.Undef <- true

            // from:
            //   %v = load %p
            //   call @f(%v, ...)
            // to:
            //   %v1 = load (%p as i64* + 0)
            //   %v2 = load (%p as i64* + 8)
            //   call @f(%v1, %v2, ...)
            // or:
            //   call @f(byval %p, ...)
            let args =
                call.FnCallArgs
                |> Seq.indexed
                |> Seq.filter (fun (_, x) -> x.Type.Kind = TK.Struct)
                |> Seq.map (fun (idx, arg) -> idx, arg.IrId, arg.Type)
            let argsInserted = ref 0
            for (argIdx, argIrId, argTp) in args do
                let cs = classifyTp ir argTp
                let splitStore =
                    if cs = [MEMORY] then
                        false
                    else
                        let intRegs =
                            cs
                            |> Seq.filter (fun c -> c = INTEGER)
                            |> Seq.length
                        let sseRegs =
                            cs |> Seq.filter (fun c -> c = SSE) |> Seq.length
                        if !remIntRegs >= intRegs && !remSseRegs >= sseRegs then
                            remIntRegs := !remIntRegs - intRegs
                            remSseRegs := !remSseRegs - sseRegs
                            true
                        else
                            false

                let load = irInstrs.[argIrId]
                if splitStore then
                    let origAddr = load.LoadSrc.Clone()
                    let dstAddr (clazz: EightByteClass) (offset: int)
                                : Proto.Value =
                        let tp = Proto.Type()
                        tp.Kind <- if clazz = SSE then TK.Double else TK.Int64
                        let ptrTp = Proto.Type()
                        ptrTp.Kind <- TK.Pointer
                        ptrTp.PointeeType <- tp

                        let dstAddr = Instr()
                        dstAddr.Id <- nextIrId()
                        dstAddr.Type <- ptrTp.Clone()
                        dstAddr.Kind <- K.Add
                        dstAddr.BinOpLeft <- Proto.Value()
                        dstAddr.BinOpLeft.Type <- ptrTp.Clone()
                        dstAddr.BinOpLeft.CastFrom <- origAddr.Clone()
                        dstAddr.BinOpRight <- Proto.Value()
                        dstAddr.BinOpRight.Type <- Proto.Type()
                        dstAddr.BinOpRight.Type.Kind <- TK.Int64
                        dstAddr.BinOpRight.I64 <- int64 offset
                        insertBefore load dstAddr
                        let v = Proto.Value()
                        v.Type <- ptrTp.Clone()
                        v.IrId <- dstAddr.Id
                        v

                    for (idx, clazz) in Seq.indexed cs do
                        let addr = dstAddr clazz (idx * 8)
                        let load =
                            if idx = 0 then
                                load
                            else
                                let load = Instr()
                                load.Id <- nextIrId()
                                load.Kind <- K.Load
                                insertBefore call load
                                load
                        load.Type <- addr.Type.PointeeType.Clone()
                        load.LoadSrc <- addr
                        let arg = Proto.Value()
                        arg.Type <- load.Type.Clone()
                        arg.IrId <- load.Id
                        if idx = 0 then
                            call.FnCallArgs.[argIdx + !argsInserted] <- arg
                        else
                            argsInserted := !argsInserted + 1
                            call.FnCallArgs.Insert(argIdx + !argsInserted, arg)
                            call.FnCallArgByval.Insert(
                                argIdx + !argsInserted, false)
                else
                    call.FnCallArgs.[argIdx + !argsInserted] <- load.LoadSrc
                    call.FnCallArgByval.[argIdx + !argsInserted] <- true
                    load.LoadSrc <- null
                    load.Kind <- K.Value
                    load.Type <- Proto.Type()
                    load.Type.Kind <- TK.Int8
                    load.Value <- Proto.Value()
                    load.Value.Type <- load.Type.Clone()
                    load.Value.Undef <- true

    rewriteCallInstrs()

let rec rewriteFnType (ir: Proto.IrModule) (tp: Proto.Type) =
    let visitTp = rewriteFnType ir
    match tp.Kind with
    | TK.Pointer -> visitTp tp.PointeeType
    | TK.Array -> visitTp tp.ArrayElemType
    | TK.Function ->
        visitTp tp.FnReturnType
        tp.FnArgTypes |> Seq.iter visitTp
        tp.FnArgByval.Clear()
        tp.FnArgTypes |> Seq.iter (fun _ -> tp.FnArgByval.Add(false))
    | _ -> ()

    if tp.Kind = TK.Function then
        let remIntRegs = ref 6 // rdi, rsi, rdx, rcx, r8, r9
        let remSseRegs = ref 8 // xmm0 - xmm7

        // from: &T(...)
        // to: &T*(&T*, ...)
        if tp.FnReturnType.Kind = TK.Struct &&
           classifyTp ir tp.FnReturnType = [MEMORY] then
            remIntRegs := !remIntRegs - 1
            tp.FnArgTypes.Insert(0, Proto.Type())
            tp.FnArgTypes.[0].Kind <- TK.Pointer
            tp.FnArgTypes.[0].PointeeType <- tp.FnReturnType
            tp.FnArgByval.Insert(0, false)
            tp.FnReturnType <- tp.FnArgTypes.[0].Clone()

        // from: void(&T, ...)
        // to: void(i64, i64, ...)
        // or: void(byval &T*, ...)
        let argsInserted = ref 0
        let args =
            tp.FnArgTypes
            |> Seq.indexed
            |> Seq.filter (fun (_, x) -> x.Kind = TK.Struct)
        for (argIdx, argTp) in args do
            let cs = classifyTp ir argTp
            let splitStore =
                if cs = [MEMORY] then
                    false
                else
                    let intRegs =
                        cs
                        |> Seq.filter (fun c -> c = INTEGER)
                        |> Seq.length
                    let sseRegs =
                        cs |> Seq.filter (fun c -> c = SSE) |> Seq.length
                    if !remIntRegs >= intRegs && !remSseRegs >= sseRegs then
                        remIntRegs := !remIntRegs - intRegs
                        remSseRegs := !remSseRegs - sseRegs
                        true
                    else
                        false
            if splitStore then
                for (clazzIdx, clazz) in Seq.indexed cs do
                    let newTp = Proto.Type()
                    newTp.Kind <- if clazz = SSE then TK.Double else TK.Int64
                    if clazzIdx = 0 then
                        tp.FnArgTypes.[argIdx + !argsInserted] <- newTp
                    else
                        argsInserted := !argsInserted + 1
                        tp.FnArgTypes.Insert(argIdx + !argsInserted, newTp)
                        tp.FnArgByval.Insert(argIdx + !argsInserted, false)
            else
                let ptrTp = Proto.Type()
                ptrTp.Kind <- TK.Pointer
                ptrTp.PointeeType <- argTp
                tp.FnArgTypes.[argIdx + !argsInserted] <- ptrTp
                tp.FnArgByval.[argIdx + !argsInserted] <- true

let run (ir: Proto.IrModule) =
    for fn in ir.FunctionDefs.Values do
        rewriteFn ir fn

    // also rewrite all Type.FUNCTION instances
    let visitTp = rewriteFnType ir
    let rec visitValue (v: Proto.Value) =
        visitTp v.Type
        match v.VCase with
        | VC.Aggregate -> v.Aggregate.Values |> Seq.iter visitValue
        | VC.CastFrom -> visitValue v.CastFrom
        | _ -> ()
    for sd in ir.StructDefs.Values do
        sd.Type |> Seq.iter visitTp
    for gd in ir.GlobalDefs.Values do
        visitTp gd.Type
        if gd.Value <> null then
            visitValue gd.Value
    for fn in ir.FunctionDefs.Values do
        visitTp fn.ReturnType
        fn.ArgTypes |> Seq.iter visitTp
        for bb in fn.Bbs.Values do
            for phi in bb.PhiNodes do
                visitTp phi.Type
            for instr in bb.Instructions do
                visitTp instr.Type
                match instr.Kind with
                | K.Alloca -> ()
                | K.Store ->
                    visitValue instr.StoreDst
                    visitValue instr.StoreSrc
                | K.Load -> visitValue instr.LoadSrc
                | K.Memcpy ->
                    visitValue instr.MemcpyDst
                    visitValue instr.MemcpySrc
                | K.Neg -> visitValue instr.NegSrc
                | K.Not -> visitValue instr.NotSrc
                | k when K.BitOr <= k && k <= K.Ugeq ->
                    visitValue instr.BinOpLeft
                    visitValue instr.BinOpRight
                | K.FnCall ->
                    visitValue instr.FnCallFn
                    instr.FnCallArgs |> Seq.iter visitValue
                | K.VaStart -> visitValue instr.VaStartVl
                | K.VaArg -> visitValue instr.VaArgVl
                | K.VaEnd -> visitValue instr.VaEndVl
                | K.VaCopy ->
                    visitValue instr.VaCopyDst
                    visitValue instr.VaCopySrc
                | K.Value -> visitValue instr.Value
                | _ -> failwith "illegal protobuf message"
            let term = bb.Terminator
            match term.Kind with
            | TermK.CondBr -> visitValue term.CondBrCond
            | TermK.Return -> visitValue term.ReturnValue
            | TermK.Switch ->
                visitValue term.SwitchCond
                term.SwitchCaseValue |> Seq.iter visitValue
            | _ -> ()
