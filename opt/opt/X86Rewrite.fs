/// Make IR compliant to X86-64 calling convention.
module X86Rewrite

open C4.Ir

type private Instr = Proto.BasicBlock.Types.Instruction
type private K = Instr.Types.Kind
type private TK = Proto.Type.Types.Kind
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
            | (tp, paddingOnly) :: _ when tp.Kind = TK.Struct ->
                let structDef = ir.StructDefs.[tp.StructId]
                let fields =
                    Seq.zip structDef.Type structDef.PaddingOnly |> List.ofSeq
                let fields =
                    if paddingOnly then
                        fields |> List.map (fun x -> fst x, true)
                    else
                        fields
                stack := fields @ !stack
                f()
            | (tp, paddingOnly) :: _ when tp.Kind = TK.Array ->
                if tp.ArraySize > 16u then
                    // early return: if array size exceeds two eightbytes, the
                    // whole struct must also be larger than two eightbytes.
                    stack := []
                    Some (MEMORY, 1u) // 1u is a dummy value
                else
                    let tps = List.replicate (min 16u tp.ArraySize |> int)
                                             (tp.ArrayElemType, paddingOnly)
                    stack := tps @ !stack
                    f()
            | (tp, paddingOnly) :: _ ->
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
                    | _ when newClazz = MEMORY -> [MEMORY]
                    | [_] -> [newClazz]
                    | [x; _] -> [x; newClazz]
                    | _ -> failwith "unreachable"
    !r

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
let private rewriteFnEntry (ir: Proto.IrModule) (fn: Proto.FunctionDef) =
    let irInstrs : Map<uint32, Instr> =
        fn.Bbs.Values
        |> Seq.collect (fun bb -> bb.Instructions)
        |> Seq.map (fun x -> (x.Id, x))
        |> Map.ofSeq
    
    let processArg (alloc: Instr) (store: Instr) =
        () // TODO

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

    () // TODO

let run (ir: Proto.IrModule) =
    for KeyValue (_, fn) in ir.FunctionDefs do
        if fn.EntryBb <> 0u then
            rewriteFnEntry ir fn
// TODO fn call, fn return
