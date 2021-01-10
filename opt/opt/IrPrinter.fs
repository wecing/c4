module IrPrinter

open C4.Ir

type private K = Proto.Type.Types.Kind
let rec private formatType (tp: Proto.Type) =
    match tp.Kind with
    | K.Void -> "void"
    | K.Int8 -> "i8"
    | K.Int16 -> "i16"
    | K.Int32 -> "i32"
    | K.Int64 -> "i64"
    | K.Float -> "float"
    | K.Double -> "double"
    | K.Struct -> $"%%{tp.StructId}"
    | K.Pointer -> $"{formatType tp.PointeeType}*"
    | K.Array -> $"[{tp.ArraySize} x {formatType tp.ArrayElemType}]"
    | K.Function ->
        let ret = formatType tp.FnReturnType
        let args = List.ofSeq tp.FnArgTypes |> List.map formatType
        let varargs = if tp.FnIsVararg then ["..."] else []
        let fullArgs = String.concat "," (args @ varargs)
        $"{ret}({fullArgs})"
    | K.Boolean -> "bool"
    | _ -> failwith "illegal protobuf message"

type private VC = Proto.Value.VOneofCase
let rec private formatValue (value: Proto.Value) =
    let tp = formatType value.Type
    match value.VCase with
    | VC.I8 -> $"{tp} {sbyte value.I8}"
    | VC.I16 -> $"{tp} {int16 value.I16}"
    | VC.I32 -> $"{tp} {value.I32}"
    | VC.I64 -> $"{tp} {value.I64}"
    | VC.F32 -> $"{tp} {value.F32}"
    | VC.F64 -> $"{tp} {value.F64}"
    | VC.Aggregate ->
        let vs = List.ofSeq value.Aggregate.Values |> List.map formatValue
        let padding =
            if value.Aggregate.ZeroPaddingBytes = 0u then
                []
            else
                [$"{value.Aggregate.ZeroPaddingBytes} x i8 0"]
        let body = vs @ padding |> String.concat ", "
        $"{{ {body} }}"
    | VC.Address ->
        $"({tp} @{value.Address.Symbol} + i64 {value.Address.Offset})"
    | VC.CastFrom ->
        $"{formatValue value.CastFrom} as {tp}"
    | VC.IrId -> $"{tp} %%{value.IrId}"
    | _ -> failwith "illegal protobuf message"

let private printBb (id: uint32) (bb: Proto.BasicBlock) =
    printfn $"%%{id}:"
    printfn "  BODY TODO" // TODO
    printfn ""

let printIr (ir: Proto.IrModule) =
    for entry in ir.StructDefs |> List.ofSeq |> List.sortBy (fun x -> x.Key) do
        let formatField (tp, paddingOnly) =
            let suffix = if paddingOnly then " paddingonly" else ""
            formatType tp + suffix
        let body =
            List.zip (List.ofSeq entry.Value.Type)
                     (List.ofSeq entry.Value.PaddingOnly)
            |> List.map formatField
            |> String.concat ", "
            |> fun x -> if x = "" then "opaque" else $"{{ {x} }}"
        printfn $"%%{entry.Key} = type {body}"
    printfn ""

    for entry in ir.GlobalDefs do
        let linkage =
            match entry.Value.Linkage with
            | Proto.Linkage.None -> ""
            | Proto.Linkage.External -> "external "
            | Proto.Linkage.Internal -> "internal "
            | _ -> failwith "illegal protobuf message"
        let value =
            let v = entry.Value.Value
            if isNull v || v = Proto.Value() then
                formatType entry.Value.Type
            else
                formatValue v
        printfn $"@{entry.Key} = {linkage}global {value}"
    printfn ""

    let funcDefsByEntryBb =
        ir.FunctionDefs |> List.ofSeq |> List.sortBy (fun x -> x.Value.EntryBb)
    for entry in funcDefsByEntryBb do
        let fnDef = entry.Value
        let signature =
            let def = if fnDef.EntryBb = 0u then "declare" else "define"
            let linkage =
                match fnDef.Linkage with
                | Proto.Linkage.None -> ""
                | Proto.Linkage.External -> "external "
                | Proto.Linkage.Internal -> "internal "
                | _ -> failwith "illegal protobuf message"
            let retTp = formatType fnDef.ReturnType
            let formatArg = fun (tp, id) -> $"{formatType tp} %%{id}"
            let args =
                if fnDef.Args.Count = 0 then
                    List.ofSeq fnDef.ArgTypes
                    |> List.map formatType
                else
                    List.zip (List.ofSeq fnDef.ArgTypes)
                             (List.ofSeq fnDef.Args)
                    |> List.map formatArg
            let argsRepr =
                args @ if fnDef.IsVararg then ["..."] else []
                |> String.concat ", "
            $"{def} {linkage}{retTp} @{entry.Key}({argsRepr})"
        let suffix = if fnDef.EntryBb = 0u then "" else " {"
        if fnDef.EntryBb <> 0u then printfn ""
        printfn $"{signature}{suffix}"
        if fnDef.EntryBb <> 0u then
            fnDef.Bbs
            |> List.ofSeq
            |> List.sortBy (fun x -> x.Key)
            |> List.iter (fun x -> printBb x.Key x.Value)
            printfn "}"
