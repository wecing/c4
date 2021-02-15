open System
open C4.Ir
open Google.Protobuf

[<EntryPoint>]
let main argv =
    let ir = Proto.IrModule.Parser.ParseFrom(Console.OpenStandardInput())
    for KeyValue (fnName, fn) in ir.FunctionDefs do
        let cfg = CFG.compute fn
        let dom = DOM.compute cfg
        MemToReg.run fn cfg dom
    if argv |> Seq.contains "--debug" then
        IrPrinter.printIr ir
    else
        ir.WriteTo(Console.OpenStandardOutput())
    0