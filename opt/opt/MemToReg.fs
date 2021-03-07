module MemToReg

open System.Collections.Generic

open C4.Ir

type private Instruction = Proto.BasicBlock.Types.Instruction
type private Terminator = Proto.BasicBlock.Types.Terminator
type private PhiNode = Proto.BasicBlock.Types.PhiNode

type private InstrRef = { Bb: uint; Idx: int }

type private K = Instruction.Types.Kind
type private TK = Terminator.Types.Kind
type private VC = Proto.Value.VOneofCase

type private Either<'T1, 'T2> =
    | Left of 'T1
    | Right of 'T2

let rec private findReg (v: Proto.Value) : uint list =
    match v.VCase with
    | VC.I8 | VC.I16 | VC.I32 | VC.I64 | VC.F32 | VC.F64 -> []
    | VC.Aggregate -> v.Aggregate.Values |> List.ofSeq |> List.collect findReg
    | VC.Address -> []
    | VC.CastFrom -> findReg v.CastFrom
    | VC.Undef -> []
    | VC.IrId -> [v.IrId]
    | _ -> failwith "illegal protobuf message"

let private findNotPromotable (ins: Instruction): uint list =
    let vs =
        match ins.Kind with
        | K.Alloca -> []
        | K.Store ->
            match ins.StoreDst.VCase with
            | VC.IrId -> [ins.StoreSrc]
            | _ -> [ins.StoreDst; ins.StoreSrc]
        | K.Load ->
            match ins.LoadSrc.VCase with
            | VC.IrId -> []
            | _ -> [ins.LoadSrc]
        | K.Memcpy -> [ins.MemcpyDst; ins.MemcpySrc]
        | K.Neg -> [ins.NegSrc]
        | K.Not -> [ins.NotSrc]
        | k when K.BitOr <= k && k <= K.Ugeq -> [ins.BinOpLeft; ins.BinOpRight]
        | K.FnCall -> ins.FnCallFn :: List.ofSeq ins.FnCallArgs
        | K.VaStart -> [ins.VaStartVl]
        | K.VaArg -> [ins.VaArgVl]
        | K.VaEnd -> [ins.VaEndVl]
        | K.VaCopy -> [ins.VaCopyDst; ins.VaCopySrc]
        | K.Value -> [ins.Value]
        | _ -> failwith "illegal protobuf message"
    vs |> List.collect findReg

let private findNotPromotable' (t: Terminator) : uint list =
    let vs =
        match t.Kind with
        | TK.Br -> []
        | TK.CondBr -> [t.CondBrCond]
        | TK.ReturnVoid -> []
        | TK.Return -> [t.ReturnValue]
        | TK.Switch -> t.SwitchCond :: List.ofSeq t.SwitchCaseValue
        | _ -> failwith "illegal protobuf message"
    vs |> List.collect findReg

// Ron Cytron et al: "Efficiently Computing Static Single Assigment Form and the
// Control Dependence Graph"
// https://www.cs.utexas.edu/~pingali/CS380C/2010/papers/ssaCytron.pdf
let run (fn: Proto.FunctionDef) (cfg: CFG.CFG) (dom: DOM.DOM) =
    // find promotable allocas and their usages; key = instr id
    let allocaRefs =
        let rs = new Dictionary<uint, InstrRef list>();
        for bbId in cfg |> CFG.postOrder |> Seq.rev do
            let instructions = fn.Bbs.[bbId].Instructions :> seq<Instruction>
            for (insIdx, ins) in Seq.indexed instructions do
                if ins.Kind = K.Alloca then
                    rs.[ins.Id] <- [{ Bb = bbId; Idx = insIdx }]
                elif ins.Kind = K.Load &&
                     rs.ContainsKey(ins.LoadSrc.IrId) then
                    let reg = ins.LoadSrc.IrId
                    let r = { Bb = bbId; Idx = insIdx }
                    rs.[reg] <- r :: rs.[reg]
                elif ins.Kind = K.Store &&
                     rs.ContainsKey(ins.StoreDst.IrId) then
                    let reg = ins.StoreDst.IrId
                    let r = { Bb = bbId; Idx = insIdx }
                    rs.[reg] <- r :: rs.[reg]

                ins
                |> findNotPromotable
                |> List.iter (rs.Remove >> ignore)

            fn.Bbs.[bbId].Terminator
            |> findNotPromotable'
            |> List.iter (rs.Remove >> ignore)
        rs

    let insertPhi() =
        let assignsTo (reg: uint) : InstrRef list =
            let isAssign (r: InstrRef) =
                match fn.Bbs.[r.Bb].Instructions.[r.Idx].Kind with
                | K.Alloca | K.Store -> true
                | _ -> false
            allocaRefs.[reg] |> List.filter isAssign

        let getType (reg: uint) : Proto.Type =
            let getT (r: InstrRef) : Proto.Type =
                let ins = fn.Bbs.[r.Bb].Instructions.[r.Idx]
                match ins.Kind with
                | K.Alloca -> ins.Type.PointeeType
                | K.Store -> ins.StoreDst.Type.PointeeType
                | _ -> ins.LoadSrc.Type.PointeeType
            allocaRefs.[reg] |> List.head |> getT

        let mutable iterCount = 0
        let mutable hasAlready =
            cfg |> CFG.postOrder |> Seq.map (fun x -> x, 0) |> Map.ofSeq
        let mutable work = hasAlready
        let mutable w: Set<uint> = Set.empty

        for v in allocaRefs.Keys do
            iterCount <- iterCount + 1
            for x in assignsTo v |> List.map (fun t -> t.Bb) do
                work <- Map.add x iterCount work
                w <- Set.add x w
            while w <> Set.empty do
                let x = w |> Seq.head
                w <- Set.remove x w
                for y in DOM.frontier x dom do
                    if hasAlready.[y] < iterCount then
                        let phi =
                            let p = PhiNode()
                            p.Id <- v
                            p.Type <- getType v
                            for bb in CFG.preds y cfg do
                                p.ValIds.Add(v)
                                p.BbIds.Add(bb)
                            p
                        fn.Bbs.[y].PhiNodes.Add(phi)
                        hasAlready <- Map.add y iterCount hasAlready
                        if work.[y] < iterCount then
                            work <- Map.add y iterCount work
                            w <- Set.add y w
    insertPhi()

    let nextReg: unit -> uint =
        let rs = seq {
            yield! fn.Args
            let xs = fn.Bbs.Values |> Seq.collect (fun v -> v.PhiNodes)
            yield! xs |> Seq.map (fun x -> x.Id)
            let xs = fn.Bbs.Values |> Seq.collect (fun v -> v.Instructions)
            yield! xs |> Seq.map (fun x -> x.Id)
        }
        let rs = rs |> Set.ofSeq |> ref
        let nextTry = !rs |> Set.count |> uint |> (+) 1u |> ref
        fun () ->
            let mutable r = !nextTry
            while Set.contains r !rs do
                r <- r + 1u
            rs := Set.add r !rs
            nextTry := r + 1u
            r

    let rename() =
        let c =
            let getInstrId r = fn.Bbs.[r.Bb].Instructions.[r.Idx].Id
            allocaRefs.Values
            |> Seq.collect (Seq.map (fun r -> getInstrId r, ref 0))
            |> Map.ofSeq
        let s = c |> Map.map (fun _ _ -> ref [])
        let vi = c |> Map.map (fun _ _ -> new List<uint>())
        let mutable oldLHS: Map<uint, uint> = Map.empty
        // from: i32 %x = load i32* %V
        // to: i32 %x = i32 %Vi
        let replaceRHS (x: Instruction) =
            if x.Kind = K.Load then
                let v = x.LoadSrc.IrId
                match Map.tryFind v s with
                | None -> ()
                | Some sv ->
                    let i = List.head !sv
                    x.Kind <- K.Value
                    x.Value <- Proto.Value()
                    x.Value.Type <- x.LoadSrc.Type.PointeeType
                    x.Value.IrId <- vi.[v].[i]
                    x.LoadSrc <- null
        let replaceLHS (x: Either<PhiNode, Instruction>) =
            let v =
                match x with
                | Left phi -> phi.Id
                | Right i when i.Kind = K.Alloca -> i.Id
                | Right i when i.Kind = K.Store -> i.StoreDst.IrId
                | Right _ -> 0u
            match Map.tryFind v c with
            | None -> ()
            | Some cv ->
                let i = !cv
                let newVi = nextReg()
                match x with
                | Left phi -> phi.Id <- newVi
                | Right instr when instr.Kind = K.Alloca ->
                    // from: i32* %V = alloca i32
                    // to: i32 %Vi = i32 undef
                    instr.Kind <- K.Value
                    instr.Id <- newVi
                    instr.Type <- instr.Type.PointeeType
                    instr.Value <- Proto.Value()
                    instr.Value.Undef <- true
                    instr.Value.Type <- instr.Type.Clone()
                | Right instr ->
                    // from: (void %x =) store i32 1, i32* %V
                    // to: i32 %Vi = i32 1
                    instr.Kind <- K.Value
                    instr.Id <- newVi
                    instr.Type <- instr.StoreDst.Type.PointeeType
                    instr.Value <- Proto.Value()
                    instr.Value <- instr.StoreSrc
                    instr.StoreSrc <- null
                    instr.StoreDst <- null
                s.[v] := i :: !s.[v]
                cv := i + 1
                oldLHS <- Map.add newVi v oldLHS
                vi.[v].Add(newVi)
        let rec search (x: uint) =
            fn.Bbs.[x].PhiNodes |> Seq.iter (Left >> replaceLHS)
            for a in fn.Bbs.[x].Instructions do
                replaceRHS a
                replaceLHS (Right a)
            for y in CFG.succs x cfg do
                for phi in fn.Bbs.[y].PhiNodes do
                    let j = phi.BbIds |> Seq.findIndex ((=) x)
                    let v = phi.ValIds.[j]
                    let i = List.head !s.[v]
                    phi.ValIds.[j] <- vi.[v].[i]
            dom |> DOM.children x |> Seq.iter search
            let phiRegs = fn.Bbs.[x].PhiNodes |> Seq.map (fun x -> x.Id)
            let instrRegs = fn.Bbs.[x].Instructions |> Seq.map (fun x -> x.Id)
            for a in Seq.append phiRegs instrRegs do
                if Map.containsKey a oldLHS then
                    let v = oldLHS.[a]
                    if Map.containsKey v s then
                        s.[v] := List.tail !s.[v]
        search(fn.EntryBb)
    rename()