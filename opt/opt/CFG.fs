module CFG

open System.Collections.Generic

open C4.Ir

type CFG =
    private
        { Entry: uint;
          Successors: Map<uint, seq<uint>>;
          Predecessors: Map<uint, seq<uint>>;
          mutable PostOrder: seq<uint> option }

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
    let succ = Seq.map getSucc fn.Bbs

    let pred = new Dictionary<uint, HashSet<uint>>()
    for (p, _) in succ do
        pred.Add(p, new HashSet<uint>())
    for (p, ss) in succ do
        for s in ss do
            pred.[s].Add(p) |> ignore

    let predecessors =
        pred |> Seq.map (fun e -> e.Key, e.Value :> seq<uint>) |> Map.ofSeq

    { Entry = fn.EntryBb
      Successors = Map.ofSeq succ
      Predecessors = predecessors
      PostOrder = None }

let entry (cfg: CFG) = cfg.Entry

let preds (n: uint) (cfg: CFG) = Map.find n cfg.Predecessors

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