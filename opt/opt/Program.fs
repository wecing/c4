open System
open C4.Ir
open Google.Protobuf

[<EntryPoint>]
let main argv =
    let ir = Proto.IrModule.Parser.ParseFrom(Console.OpenStandardInput())

    // TODO: insert .Clone() on protobuf objs where needed
    // TODO: rewrite aggregate value init, e.g. `store {i64 1, i64 2}, %T* %p`
    // as scalar store or memcpy
    X86Rewrite.run ir
    for KeyValue (_, fn) in ir.FunctionDefs do
        if fn.EntryBb <> 0u then
            let cfg = CFG.compute fn
            let dom = DOM.compute cfg
            MemToReg.run fn cfg dom

    if argv |> Seq.contains "--debug" then
        IrPrinter.printIr ir
    else
        ir.WriteTo(Console.OpenStandardOutput())
    0