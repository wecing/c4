module DOM

open System.Collections.Generic

type DOM =
    private
        { ImmediateDominator: Map<uint, uint>; // entry: points to itself
          DominanceChildren: Map<uint, seq<uint>>; // children in dom tree
          DominanceFrontier: Map<uint, seq<uint>> }

// Cooper, Harvey, Kennedy: "A Simple, Fast Dominance Algorithm"
// http://www.hipersoft.rice.edu/grads/publications/dom14.pdf
let compute (cfg: CFG.CFG) : DOM =
    let numToNodeMap: Map<int, uint> =
        cfg |> CFG.postOrder |> Seq.indexed |> Map.ofSeq
    let numToNode = fun x -> Map.find x numToNodeMap
    let nodeToNumMap: Map<uint, int> =
        numToNodeMap |> Seq.map (fun (KeyValue (a, b)) -> b, a) |> Map.ofSeq
    let nodeToNum = fun x -> Map.find x nodeToNumMap

    let startNode = cfg |> CFG.entry |> nodeToNum

    let doms: int option array =
        [| for _ in 1 .. Map.count numToNodeMap -> None |]
    doms.[startNode] <- Some startNode
    let mutable changed = true

    let intersect (b1: int) (b2: int) : int =
        let mutable finger1 = b1
        let mutable finger2 = b2
        while finger1 <> finger2 do
            while finger1 < finger2 do
                finger1 <- doms.[finger1].Value
            while finger2 < finger1 do
                finger2 <- doms.[finger2].Value
        finger1

    while changed do
        changed <- false
        for b in (Map.count numToNodeMap - 1) .. -1 .. 0 do
            if b = startNode then () else
            let mutable newIdom: int option = None
            let preds = CFG.preds (numToNode b) cfg |> Seq.map nodeToNum
            for p in preds do
                if doms.[p].IsNone then () else
                if newIdom.IsNone then newIdom <- Some p else
                newIdom <- Some (intersect p newIdom.Value)
            if doms.[b] <> newIdom then
                doms.[b] <- newIdom
                changed <- true

    // doms is finalized, we could compute dominance frontiers now
    let frontiers: HashSet<int> array =
        [| for _ in 1 .. Map.count numToNodeMap -> new HashSet<int>() |]
    for b in 0 .. (Map.count numToNodeMap - 1) do
        let preds = CFG.preds (numToNode b) cfg |> Seq.map nodeToNum
        if Seq.length preds >= 2 then
            for p in preds do
                let mutable runner = p
                while runner <> doms.[b].Value do
                    frontiers.[runner].Add(b) |> ignore
                    runner <- doms.[runner].Value

    // now convert them back to nodes
    let immediateDominator =
        doms
        |> Seq.ofArray
        |> Seq.indexed
        |> Seq.map (fun (k, v) -> numToNode k, numToNode v.Value)
        |> Map.ofSeq
    let dominanceFrontier =
        frontiers
        |> Seq.ofArray
        |> Seq.indexed
        |> Seq.map (fun (k, v) -> numToNode k, Seq.map numToNode v |> Seq.sort)
        |> Map.ofSeq

    let children: Map<uint, seq<uint>> =
        let cs =
            immediateDominator |> Map.map (fun _ _ -> new SortedSet<uint>())
        for (c, p) in immediateDominator |> Map.toSeq do
            if c <> CFG.entry cfg || p <> CFG.entry cfg then
                cs.[p].Add(c) |> ignore
        cs |> Map.map (fun _ s -> upcast s)

    { ImmediateDominator = immediateDominator
      DominanceChildren = children
      DominanceFrontier = dominanceFrontier }

let parent (n: uint) (dom: DOM) = Map.find n dom.ImmediateDominator

let children (n: uint) (dom: DOM) = Map.find n dom.DominanceChildren

let frontier (n: uint) (dom: DOM) = Map.find n dom.DominanceFrontier