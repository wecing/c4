open System

[<EntryPoint>]
let main argv =
    let irModule = C4.Ir.Proto.IrModule.Parser.ParseFrom(Console.OpenStandardInput())
    printfn "%A" irModule
    0 // return an integer exit code