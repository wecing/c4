module CFG

open System.Collections.Generic

open C4.Ir

type CFG =
    private
        { Entry: uint;
          Successors: Map<uint, seq<uint>>;
          Predecessors: Map<uint, seq<uint>>;
          mutable PostOrder: seq<uint> option }

// private; visible for testing
let ofPaths (entry: uint) (succs: seq<uint * seq<uint>>) : CFG =
    let pred = new Dictionary<uint, HashSet<uint>>()
    for (p, _) in succs do
        pred.Add(p, new HashSet<uint>())
    for (p, ss) in succs do
        for s in ss do
            pred.[s].Add(p) |> ignore

    let predecessors =
        pred |> Seq.map (fun e -> e.Key, e.Value |> Seq.sort) |> Map.ofSeq

    { Entry = entry
      Successors = Map.ofSeq succs
      Predecessors = predecessors
      PostOrder = None }

type private TermK = Proto.BasicBlock.Types.Terminator.Types.Kind
let compute (fn: Proto.FunctionDef) : CFG =
    let getSucc (KeyValue (k, v: Proto.BasicBlock)) =
        let ts =
            match v.Terminator.Kind with
            | TermK.Br -> seq { yield v.Terminator.BrTarget }
            | TermK.CondBr -> seq {
                yield v.Terminator.CondBrTrue
                yield v.Terminator.CondBrFalse }
            | TermK.ReturnVoid | TermK.Return -> Seq.empty
            | TermK.Switch -> upcast v.Terminator.SwitchCaseTarget
            | _ -> failwith "illegal protobuf message"
        (k, Seq.sort ts)
    fn.Bbs |> Seq.map getSucc |> ofPaths fn.EntryBb

let entry (cfg: CFG) = cfg.Entry

let preds (n: uint) (cfg: CFG) = Map.find n cfg.Predecessors

let succs (n: uint) (cfg: CFG) = Map.find n cfg.Successors

let postOrder (cfg: CFG) : seq<uint> =
    if cfg.PostOrder.IsSome then cfg.PostOrder.Value else
    let visited = new HashSet<uint>()
    let order = new List<uint>()
    let rec visit (n: uint) =
        if visited.Add(n) then
            cfg.Successors |> Map.find n |> Seq.iter visit
            order.Add(n)
    visit cfg.Entry
    cfg.PostOrder <- Some (upcast order)
    upcast order