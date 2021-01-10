open System
open C4.Ir

[<EntryPoint>]
let main argv =
    let ir = Proto.IrModule.Parser.ParseFrom(Console.OpenStandardInput())
    IrPrinter.printIr ir
    0