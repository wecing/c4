open System
open C4.Ir

[<EntryPoint>]
let main argv =
    let ir = Proto.IrModule.Parser.ParseFrom(Console.OpenStandardInput())
    for KeyValue (fnName, fn) in ir.FunctionDefs do
        let cfg = CFG.compute fn
        let dom = DOM.compute cfg
        MemToReg.run fn cfg dom
    IrPrinter.printIr ir
    0