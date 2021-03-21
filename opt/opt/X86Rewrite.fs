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
        //   f(&T* %arg1):
        //     %p = %arg1
        let cs = classifyTp ir argTp
        let splitStore =
            if cs = [MEMORY] then
                false
            else
                for c in cs do
                    let r = if c = INTEGER then remIntRegs else remSseRegs
                    r := !r - 1
                !remIntRegs >= 0 && !remSseRegs >= 0
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
                        insertAfter alloc dstAddr
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
                let store =
                    irInstrs
                    |> Map.toSeq
                    |> Seq.map snd
                    |> Seq.find (fun x ->
                        x.Kind = K.Store && x.StoreSrc.IrId = call.Id)
                call.FnCallArgs.Insert(0, store.StoreDst)
                call.Type <- store.StoreDst.Type.Clone()
                store.Kind <- K.Value
                store.StoreSrc <- null
                store.StoreDst <- null
                store.Value <- Proto.Value()
                store.Value.Type <- Proto.Type()
                store.Value.Type.Kind <- TK.Int8
                store.Value.Undef <- true

            () // TODO: args rewrite

    rewriteCallInstrs()

let run (ir: Proto.IrModule) =
    for KeyValue (_, fn) in ir.FunctionDefs do
        rewriteFn ir fn
        // TODO: also rewrite all Type.FUNCTION instances
