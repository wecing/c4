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

type private IK = Proto.BasicBlock.Types.Instruction.Types.Kind
type private TermK = Proto.BasicBlock.Types.Terminator.Types.Kind
let private printBb (id: uint32) (bb: Proto.BasicBlock) =
    let ft = formatType
    let fv = formatValue
    printfn ""
    printfn $"_{id}:"
    for i in bb.Instructions do
        let assign = $"%%{i.Id} ="
        let s =
            match i.Kind with
            | IK.Alloca -> $"{assign} alloca {ft i.Type.PointeeType}"
            | IK.Store -> $"store {fv i.StoreSrc}, {fv i.StoreDst}"
            | IK.Load ->
                $"{assign} load {ft i.LoadSrc.Type.PointeeType}, {fv i.LoadSrc}"
            | IK.Memcpy -> $"memcpy {fv i.MemcpySrc}, {fv i.MemcpyDst}"
            | IK.Neg -> $"{assign} neg {fv i.NegSrc}"
            | IK.Not -> $"{assign} not {fv i.NotSrc}"
            | _ when IK.BitOr <= i.Kind && i.Kind <= IK.Ugeq ->
                let l = fv i.BinOpLeft
                let r = fv i.BinOpRight
                let op =
                    match i.Kind with
                    | IK.BitOr -> "or"
                    | IK.BitAnd -> "and"
                    | k -> k.ToString().ToLower()
                $"{assign} {op} {ft i.Type}, {l}, {r}"
            | IK.FnCall ->
                let fn = fv i.FnCallFn
                let args = i.FnCallArgs |> Seq.map fv |> String.concat ", "
                if i.Type.Kind = K.Void then
                    $"call void, {fv i.FnCallFn} ({args})"
                else
                    $"{assign} call {ft i.Type}, {fv i.FnCallFn} ({args})"
            | IK.VaStart -> $"va_start {fv i.VaStartVl}"
            | IK.VaArg -> $"va_arg {ft i.Type}, {fv i.VaArgVl}"
            | IK.VaEnd -> $"va_end {fv i.VaEndVl}"
            | IK.VaCopy -> $"va_copy {fv i.VaCopySrc} {fv i.VaCopyDst}"
            | _ -> failwith "illegal protobuf message"
        printfn $"  {s}"
    let t =
        let term = bb.Terminator
        match term.Kind with
        | TermK.Br -> $"br _{term.BrTarget}"
        | TermK.CondBr ->
            let c = fv term.CondBrCond
            $"br {c}, _{term.CondBrTrue}, _{term.CondBrFalse}"
        | TermK.ReturnVoid -> "return void"
        | TermK.Return -> $"return {fv term.ReturnValue}"
        | TermK.Switch ->
            let c = fv term.SwitchCond
            let ts =
                Seq.zip term.SwitchCaseValue term.SwitchCaseTarget
                |> Seq.map (fun (v, t) -> $"{fv v}, _{t}")
                |> String.concat ", "
            $"switch {c}, _{term.SwitchDefaultTarget}, [{ts}]"
        | _ -> failwith "illegal protobuf message"
    printfn $"  {t}"

let printIr (ir: Proto.IrModule) =
    for entry in ir.StructDefs |> Seq.sortBy (fun x -> x.Key) do
        let formatField (tp, paddingOnly) =
            let suffix = if paddingOnly then " paddingonly" else ""
            formatType tp + suffix
        let body =
            Seq.zip entry.Value.Type entry.Value.PaddingOnly
            |> Seq.map formatField
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

    for entry in ir.FunctionDefs |> Seq.sortBy (fun x -> x.Value.EntryBb) do
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
                    Seq.map formatType fnDef.ArgTypes
                else
                    Seq.zip fnDef.ArgTypes fnDef.Args
                    |> Seq.map formatArg
            let argsRepr =
                List.ofSeq args @ if fnDef.IsVararg then ["..."] else []
                |> String.concat ", "
            $"{def} {linkage}{retTp} @{entry.Key}({argsRepr})"
        let suffix = if fnDef.EntryBb = 0u then "" else " {"
        if fnDef.EntryBb <> 0u then printfn ""
        printfn $"{signature}{suffix}"
        if fnDef.EntryBb <> 0u then
            printfn $"  br _{fnDef.EntryBb}"
            fnDef.Bbs
            |> Seq.sortBy (fun x -> x.Key)
            |> Seq.iter (fun x -> printBb x.Key x.Value)
            printfn "}"
