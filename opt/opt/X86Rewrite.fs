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

let sizeofTp (ir: Proto.IrModule) (tp: Proto.Type) : uint =
    0u // TODO

// rewrite arg store on fn entry, e.g.:
//
// %p = alloca &T
// store &T %arg1, &T* %p
//
// could be rewritten as:
//
// %p = &T* %arg1
//
// or:
//
// %p = alloca &T
// store i64, %arg1_p1, i64* (%p as i64*)
// store i64, %arg1_p2, i64* (%p as i64* + 8)
//
// fn signature will also be updated here.
let private rewriteFnEntry (ir: Proto.IrModule) (fn: Proto.FunctionDef) =
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

    let processArg (alloc: Instr) (store: Instr) =
        let argIdx = Seq.findIndex (fun x -> x = store.StoreSrc.IrId) fn.Args
        let cs = classifyTp ir store.StoreSrc.Type
        if cs = [MEMORY] then
            // change fn signature
            fn.ArgTypes.[argIdx] <- alloc.Type
            // alloc -> value alias
            alloc.Kind <- K.Value
            alloc.Value <- Proto.Value()
            alloc.Value.Type <- alloc.Type
            alloc.Value.IrId <- store.StoreSrc.IrId
            // store -> undef
            store.Kind <- K.Value
            store.Type <- store.StoreSrc.Type
            store.Value <- Proto.Value()
            store.Value.Type <- store.Type
            store.Value.Undef <- true
            store.StoreSrc <- null
            store.StoreDst <- null
        else
            let newStore() =
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

            let processStore (s: Instr) (clazz: EightByteClass) (offset: int) =
                let tp = Proto.Type()
                tp.Kind <- if clazz = SSE then TK.Double else TK.Int64
                let ptrTp = Proto.Type()
                ptrTp.Kind <- TK.Pointer
                ptrTp.PointeeType <- tp
                
                s.StoreSrc.Type <- tp

                let dstValue =
                    let dstAddr = Instr()
                    dstAddr.Id <- nextIrId()
                    dstAddr.Type <- ptrTp
                    dstAddr.Kind <- K.Add
                    dstAddr.BinOpLeft <- Proto.Value()
                    dstAddr.BinOpLeft.Type <- ptrTp
                    dstAddr.BinOpLeft.CastFrom <- Proto.Value()
                    dstAddr.BinOpLeft.CastFrom.Type <- alloc.Type
                    dstAddr.BinOpLeft.CastFrom.IrId <- alloc.Id
                    dstAddr.BinOpRight <- Proto.Value()
                    dstAddr.BinOpRight.Type <- Proto.Type()
                    dstAddr.BinOpRight.Type.Kind <- TK.Int64
                    dstAddr.BinOpRight.I64 <- int64 offset
                    insertAfter alloc dstAddr
                    let v = Proto.Value()
                    v.Type <- ptrTp
                    v.IrId <- dstAddr.Id
                    v
                s.StoreDst <- dstValue

                // change fn signature
                if s.StoreSrc.IrId = 0u then
                    s.StoreSrc.IrId <- nextIrId()
                    fn.Args.Insert(argIdx + 1, s.StoreSrc.IrId)
                    fn.ArgTypes.Insert(argIdx + 1, tp)
                else
                    fn.ArgTypes.[argIdx] <- tp

            Seq.zip cs [0; 8]
            |> Seq.filter (fun (c, _) -> c <> NO_CLASS)
            |> Seq.zip [Some store; None]
            |> Seq.map (fun (s, x) -> (Option.defaultWith newStore s, x))
            |> Seq.iter (fun (x, (y, z)) -> processStore x y z)

    irInstrs
    |> Map.toSeq
    |> Seq.map snd
    |> Seq.filter
        (fun x ->
            x.Kind = K.Store
            && x.StoreSrc.VCase = VC.IrId
            && Seq.contains x.StoreSrc.IrId fn.Args
            && x.StoreSrc.Type.Kind = Proto.Type.Types.Kind.Struct)
    |> Seq.iter (fun x -> processArg irInstrs.[x.StoreDst.IrId] x)

// rewrite fn arg passing & value returning
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

    () // TODO

let run (ir: Proto.IrModule) =
    for KeyValue (_, fn) in ir.FunctionDefs do
        rewriteFn ir fn

        // // TODO: fn signature needs to be updated even if entryBb = 0u
        // if fn.EntryBb <> 0u then
        //     rewriteFnEntry ir fn
        //     // TODO fn call, fn return
        //     // TODO: also rewrite all Type.FUNCTION instances
