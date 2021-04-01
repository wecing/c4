{-# LANGUAGE TemplateHaskell #-}

module InstrSelect (run) where

import Control.Lens (Field2 (_2), at, to, use, uses, view, (^.), (.=), (%=), (<+=))
import Control.Lens.TH (makeLenses)
import Control.Monad.RWS.Lazy (RWS, evalRWS, tell)
import Data.Int (Int64)
import qualified Data.Map as Map
import qualified Data.ProtoLens.Runtime.Data.Text as Text
import qualified Data.Set as Set
import Data.Word (Word32)
import qualified Proto.Ir as IR
import qualified Proto.Ir_Fields as IR
import Text.Format (format)

-- data IntSize = Byte | Word | Long | Quad deriving (Eq)
data RegSize
  = Byte | Word | Long | Quad
  | F64 | F32
  | SizeAndAlign Int Int -- implies stack alloc; reg value is the ptr
    deriving (Eq, Show)

newtype RegIdx = RegIdx Int deriving (Eq, Show, Ord)

data MachineReg
  = RAX
  | RSP
  | RBP
  | RDI
  | RSI
  | RDX
  | RCX
  | R8
  | R9
  | XMM0
  | XMM1
  | XMM2
  | XMM3
  | XMM4
  | XMM5
  | XMM6
  | XMM7
    deriving (Show)

data Operand
  = Imm Int64
  | ImmF (Either Float Double)
  | Reg RegIdx -- mem, general reg, or sse reg
  | MReg RegSize MachineReg -- a specific machine reg
  | Addr String Int64 -- link time constant
  | StackParam Int -- byte offset relative to head of stack params region
    deriving (Show)

data Instr = Instr String (Maybe RegSize) [Operand] deriving (Show)

type FuncName = Text.Text

type BasicBlockId = Word32

data RunFnCallState = RunFnCallState
  { _remIntRegs :: Int
  , _remSseRegs :: Int
  , _curStackSize :: Int
  }
data InstrSelectState = InstrSelectState
  { _visitedBasicBlocks :: Set.Set BasicBlockId
  , _currentFuncName :: FuncName
  , _lastRegIdx :: Int
  , _regSize :: Map.Map RegIdx RegSize
  , _regIdxByIrId :: Map.Map Int RegIdx
  , _stackParamsRegionSize :: Int
  , _runFnCallState :: RunFnCallState -- init'ed for every runFnCall call
  }

makeLenses ''RunFnCallState
makeLenses ''InstrSelectState

initRunFnCallState :: RunFnCallState
initRunFnCallState = RunFnCallState
  { _remIntRegs = 6
  , _remSseRegs = 8
  , _curStackSize = 0
  }

type FuncBody = [(BasicBlockId, [Instr])]

run :: IR.IrModule -> Map.Map FuncName FuncBody
run irModule = Map.mapWithKey runFunc' $ irModule ^. IR.functionDefs
  where
    runFunc' :: FuncName -> IR.FunctionDef -> FuncBody
    runFunc' funcName funcDef = snd $ evalRWS m r s
      where
        m = runArgs' funcDef >> runBasicBlock (funcDef ^. IR.entryBb)
        r = irModule
        s = InstrSelectState
              { _visitedBasicBlocks = Set.empty
              , _currentFuncName = funcName
              , _lastRegIdx = 0
              , _regSize = Map.empty
              , _regIdxByIrId = Map.empty
              , _stackParamsRegionSize = 0
              , _runFnCallState = initRunFnCallState
              }
    runArgs' :: Monoid a
             => IR.FunctionDef
             -> RWS IR.IrModule a InstrSelectState ()
    runArgs' funcDef =
      mapM_ defineRegForIrId $ zip (funcDef ^. IR.argTypes) (funcDef ^. IR.args)

runBasicBlock :: BasicBlockId -> RWS IR.IrModule FuncBody InstrSelectState ()
runBasicBlock basicBlockId = do
  visited <- uses visitedBasicBlocks (Set.member basicBlockId)
  if visited
    then return ()
    else do
      visitedBasicBlocks %= Set.insert basicBlockId
      basicBlock <- getBasicBlock basicBlockId
      xs1 <- concat <$> mapM runPhi (basicBlock ^. IR.phiNodes)
      xs2 <- concat <$> mapM runInstr (basicBlock ^. IR.instructions)
      xs3 <- runTerminator $ basicBlock ^. IR.terminator
      tell [(basicBlockId, xs1 ++ xs2 ++ xs3)]
      mapM_ runBasicBlock $ getSuccessors basicBlock

runPhi :: Monoid a
       => IR.BasicBlock'PhiNode
       -> RWS IR.IrModule a InstrSelectState [Instr]
runPhi phi = do
  defineRegForIrId (phi ^. IR.type', phi ^. IR.id)
  return [] -- phi will be revisited and eliminated in reg allocator

runInstr :: Monoid a
         => IR.BasicBlock'Instruction
         -> RWS IR.IrModule a InstrSelectState [Instr]
runInstr irInstr =
  case irInstr ^. IR.kind of
    IR.BasicBlock'Instruction'STORE -> do
      (instrsDst, regIdxDst) <- runValue (irInstr ^. IR.storeDst)
      (instrsSrc, regIdxSrc) <- runValue (irInstr ^. IR.storeSrc)
      regSize <- getRegSize regIdxSrc
      -- "store a, b" means "mov a, (b)"
      let mov = Instr "store" (Just regSize) [Reg regIdxSrc, Reg regIdxDst]
      return (instrsDst ++ instrsSrc ++ [mov])
    IR.BasicBlock'Instruction'LOAD -> do
      (instrsSrc, regIdxSrc) <- runValue (irInstr ^. IR.loadSrc)
      regIdxDst <- defineRegForIrId (irInstr ^. IR.type', irInstr ^. IR.id)
      regSize <- getRegSize regIdxDst
      -- "load a, b" means "mov (a), b"
      let mov = Instr "load" (Just regSize) [Reg regIdxSrc, Reg regIdxDst]
      return (instrsSrc ++ [mov])
    IR.BasicBlock'Instruction'NOT -> do
      (instrsSrc, regIdxSrc) <- runValue (irInstr ^. IR.loadSrc)
      regIdxDst <- defineRegForIrId (irInstr ^. IR.type', irInstr ^. IR.id)
      regSize <- getRegSize regIdxDst
      -- NOT could only be applied to integral types
      let ins = Instr "not" (Just regSize) [Reg regIdxSrc]
      let mov = Instr "mov" (Just regSize) [Reg regIdxSrc, Reg regIdxDst]
      return (instrsSrc ++ [ins, mov])
    k | isBinArithOp k -> runBinArithOp irInstr k
    k | isCmpOp k -> runCmpOp irInstr k
    IR.BasicBlock'Instruction'FN_CALL -> runFnCall irInstr
    IR.BasicBlock'Instruction'VALUE -> do
      case irInstr ^. IR.value . IR.maybe'v of
        Just (IR.Value'Undef _) -> do
          defineRegForIrId (irInstr ^. IR.type', irInstr ^. IR.id)
          return []
        _ -> do
          (instrs, regIdx) <- runValue (irInstr ^. IR.value)
          regSize <- getRegSize regIdx
          r <- defineRegForIrId (irInstr ^. IR.type', irInstr ^. IR.id)
          let mov = Instr "mov" (Just regSize) [Reg regIdx, Reg r]
          return (instrs ++ [mov])
    x -> error ("TODO: unimplemented instr " ++ show x)

runTerminator :: Monoid a
              => IR.BasicBlock'Terminator
              -> RWS IR.IrModule a InstrSelectState [Instr]
runTerminator _ = return [] -- TODO

-- always return a writable RegIdx
runValue :: Monoid a
         => IR.Value
         -> RWS IR.IrModule a InstrSelectState ([Instr], RegIdx)
runValue irValue = do
  let Just v = irValue ^. IR.maybe'v
  case v of
    IR.Value'I8 x -> do
      let imm = Imm (fromIntegral x)
      regIdx <- defineReg Byte
      return ([Instr "mov" (Just Byte) [imm, Reg regIdx]], regIdx)
    IR.Value'I16 x -> do
      let imm = Imm (fromIntegral x)
      regIdx <- defineReg Word
      return ([Instr "mov" (Just Word) [imm, Reg regIdx]], regIdx)
    IR.Value'I32 x -> do
      let imm = Imm (fromIntegral x)
      regIdx <- defineReg Long
      return ([Instr "mov" (Just Long) [imm, Reg regIdx]], regIdx)
    IR.Value'I64 x -> do
      let imm = Imm (fromIntegral x)
      regIdx <- defineReg Quad
      return ([Instr "mov" (Just Quad) [imm, Reg regIdx]], regIdx)
    IR.Value'F32 x -> do
      let imm = ImmF (Left x)
      regIdx <- defineReg F32
      return ([Instr "mov" (Just F32) [imm, Reg regIdx]], regIdx)
    IR.Value'F64 x -> do
      let imm = ImmF (Right x)
      regIdx <- defineReg F64
      return ([Instr "mov" (Just F64) [imm, Reg regIdx]], regIdx)
    -- TODO: aggregate
    IR.Value'Address' addr -> do
      let imm = Addr (Text.unpack (addr ^. IR.symbol)) (addr ^. IR.offset)
      regIdx <- defineReg Quad
      return ([Instr "lea" (Just Quad) [imm, Reg regIdx]], regIdx)
    IR.Value'CastFrom fromIrValue -> do
      (baseInstrs, fromRegIdx) <- runValue fromIrValue
      fromSz <- getRegSize fromRegIdx
      let toSz = case irValue ^. IR.type' . IR.kind of
            IR.Type'INT8 -> Byte
            IR.Type'INT16 -> Word
            IR.Type'INT32 -> Long
            IR.Type'INT64 -> Quad
            IR.Type'FLOAT -> F32
            IR.Type'DOUBLE -> F64
            IR.Type'POINTER -> Quad
            IR.Type'BOOLEAN -> Byte
      toRegIdx <- defineReg toSz
      let fromReg = Reg fromRegIdx
      let toReg = Reg toRegIdx
      let regs = [fromReg, toReg]
      castInstrs <- case (fromSz, toSz) of
            (SizeAndAlign _ _, _) -> error "unreachable"
            (_, SizeAndAlign _ _) -> error "unreachable"
            (x, y) | x == y -> return [Instr "mov" (Just x) regs]
            (F32, F64) -> return [Instr "cvtss2sd" Nothing regs]
            (F64, F32) -> return [Instr "cvtsd2ss" Nothing regs]
            -- ? -> bool
            _ | irValue ^. IR.type' . IR.kind == IR.Type'BOOLEAN ->
              case fromSz of
                -- F32/F64 -> bool
                _ | fromSz == F32 || fromSz == F64 -> do
                  r <- defineReg fromSz
                  return
                    [ Instr "xorps" Nothing [Reg r, Reg r]
                    , Instr "cmpneq" (Just fromSz) [fromReg, Reg r]
                    , Instr "mov" (Just Byte) [Reg r, toReg]
                    , Instr "and" (Just Byte) [Imm 1, toReg]
                    ]
                -- intN -> bool
                _ -> return
                  [ Instr "test" (Just fromSz) [fromReg, fromReg]
                  , Instr "setnz" Nothing [toReg]
                  ]
            -- F32/F64 <-> intN
            -- TODO: unsigned?
            -- TODO: cvtXX int arg must be r32/r64
            (F32, _) -> return [Instr "cvtss2si" Nothing regs]
            (F64, _) -> return [Instr "cvtsd2si" Nothing regs]
            (_, F32) -> return [Instr "cvtsi2ss" Nothing regs]
            (_, F64) -> return [Instr "cvtsi2sd" Nothing regs]
            -- intN -> intN
            -- TODO: unsigned?
            (Byte, _) -> return [Instr "movsb" (Just toSz) regs]
            (_, Byte) -> return [Instr "mov" (Just toSz) regs]
            (Word, _) -> return [Instr "movsw" (Just toSz) regs]
            (_, Word) -> return [Instr "mov" (Just toSz) regs]
            (Long, _) -> return [Instr "movsl" (Just toSz) regs]
            (_, Long) -> return [Instr "mov" (Just toSz) regs]
            (Quad, Quad) -> return [Instr "mov" (Just toSz) regs]
      return (baseInstrs ++ castInstrs, toRegIdx)
    -- TODO: undef
    IR.Value'IrId irIdRaw -> do
      let irId = fromIntegral irIdRaw
      fromIdx <- use (regIdxByIrId . at irId . to unwrapMaybe)
      regSize <- getRegSize fromIdx
      toIdx <- defineReg regSize
      return ([Instr "mov" (Just regSize) [Reg fromIdx, Reg toIdx]], toIdx)

runBinArithOp :: Monoid a
              => IR.BasicBlock'Instruction
              -> IR.BasicBlock'Instruction'Kind
              -> RWS IR.IrModule a InstrSelectState [Instr]
runBinArithOp irInstr k = do
  (instrsL, regIdxL) <- runValue (irInstr ^. IR.binOpLeft)
  (instrsR, regIdxR) <- runValue (irInstr ^. IR.binOpRight)
  regSize <- getRegSize regIdxR
  r <- defineRegForIrId (irInstr ^. IR.type', irInstr ^. IR.id)
  -- TODO: assuming szL == szR == szDst
  let tpKind = irInstr ^. IR.type' . IR.kind
  let isFp = tpKind `elem` [IR.Type'FLOAT, IR.Type'DOUBLE]
  let op = case k of
        IR.BasicBlock'Instruction'BIT_OR -> "or"
        IR.BasicBlock'Instruction'XOR -> "xor"
        IR.BasicBlock'Instruction'BIT_AND -> "and"
        IR.BasicBlock'Instruction'SHL -> "shl"
        IR.BasicBlock'Instruction'ASHR -> "sar"
        IR.BasicBlock'Instruction'LSHR -> "shr"
        IR.BasicBlock'Instruction'ADD -> "add"
        IR.BasicBlock'Instruction'SUB -> "sub"
        IR.BasicBlock'Instruction'MUL | isFp -> "mul"
        IR.BasicBlock'Instruction'MUL -> "imul"
        IR.BasicBlock'Instruction'DIV | isFp -> "div"
        IR.BasicBlock'Instruction'DIV -> "idiv"
        IR.BasicBlock'Instruction'UDIV -> "div"
        IR.BasicBlock'Instruction'MOD -> "idiv"
        IR.BasicBlock'Instruction'UMOD -> "div"
  let isIntDiv =
        not isFp &&
        k `elem` [ IR.BasicBlock'Instruction'DIV
                 , IR.BasicBlock'Instruction'UDIV
                 ]
  let isIntMod =
        k `elem` [ IR.BasicBlock'Instruction'MOD
                 , IR.BasicBlock'Instruction'UMOD
                 ]
  let arith
        | isIntDiv =
            [ Instr "mov" (Just regSize) [Imm 0, MReg regSize RDX]
            , Instr "mov" (Just regSize) [Reg regIdxR, MReg regSize RAX]
            , Instr op (Just regSize) [Reg regIdxL]
            , Instr "mov" (Just regSize) [MReg regSize RAX, Reg r]
            ]
        | isIntMod =
            [ Instr "mov" (Just regSize) [Imm 0, MReg regSize RDX]
            , Instr "mov" (Just regSize) [Reg regIdxR, MReg regSize RAX]
            , Instr op (Just regSize) [Reg regIdxL]
            , Instr "mov" (Just regSize) [MReg regSize RDX, Reg r]
            ]
        | otherwise =
            [ Instr op (Just regSize) [Reg regIdxR, Reg regIdxL]
            , Instr "mov" (Just regSize) [Reg regIdxL, Reg r]
            ]
  return (instrsL ++ instrsR ++ arith)

runCmpOp :: Monoid a
         => IR.BasicBlock'Instruction
         -> IR.BasicBlock'Instruction'Kind
         -> RWS IR.IrModule a InstrSelectState [Instr]
runCmpOp irInstr k = do
  (instrsL, regIdxL) <- runValue (irInstr ^. IR.binOpLeft)
  (instrsR, regIdxR) <- runValue (irInstr ^. IR.binOpRight)
  regSize <- getRegSize regIdxR
  r <- defineRegForIrId (irInstr ^. IR.type', irInstr ^. IR.id)
  let tpKind = irInstr ^. IR.type' . IR.kind
  let isFp = tpKind `elem` [IR.Type'FLOAT, IR.Type'DOUBLE]
  let isEq = k == IR.BasicBlock'Instruction'EQ
  let isNeq = k == IR.BasicBlock'Instruction'NEQ
  cmp <- if isFp && (isEq || isNeq) then do
          -- eq/ne for fp needs special treatment here, since
          -- (ucomi NaN, NaN) sets ZF=1 but (NaN cmp NaN) is false.
          let (op1, op2, op3) = if isEq
              then ("sete", "setnp", "and")
              else ("setne", "setp", "or")
          npRegIdx <- defineReg Byte
          return
            [ Instr "ucomi" (Just regSize) [Reg regIdxL, Reg regIdxR]
            , Instr op1 Nothing [Reg r]
            , Instr op2 Nothing [Reg npRegIdx]
            , Instr op3 (Just Byte) [Reg npRegIdx, Reg r]
            ]
        else if isFp then do
          let (op, revCmpArgs) = case k of
                IR.BasicBlock'Instruction'LT -> ("seta", False)
                IR.BasicBlock'Instruction'GT -> ("seta", True)
                IR.BasicBlock'Instruction'LEQ -> ("setae", False)
                IR.BasicBlock'Instruction'GEQ -> ("setae", True)
          let cmpArgs = if revCmpArgs
              then [Reg regIdxL, Reg regIdxR]
              else [Reg regIdxR, Reg regIdxL]
          return
            [ Instr "ucomi" (Just regSize) cmpArgs
            , Instr op Nothing [Reg r]
            ]
        else do
          let op = case k of
                IR.BasicBlock'Instruction'EQ -> "sete"
                IR.BasicBlock'Instruction'NEQ -> "setne"
                IR.BasicBlock'Instruction'LT -> "setl"
                IR.BasicBlock'Instruction'GT -> "setg"
                IR.BasicBlock'Instruction'LEQ -> "setle"
                IR.BasicBlock'Instruction'GEQ -> "setge"
                IR.BasicBlock'Instruction'ULT -> "setb"
                IR.BasicBlock'Instruction'UGT -> "seta"
                IR.BasicBlock'Instruction'ULEQ -> "setbe"
                IR.BasicBlock'Instruction'UGEQ -> "setae"
          return
            [ Instr "cmp" (Just regSize) [Reg regIdxR, Reg regIdxL]
            , Instr op Nothing [Reg r]
            ]
  -- I actually do not understand why clang emits this
  let and1 = Instr "and" (Just Byte) [Imm 1, Reg r]
  return (instrsL ++ instrsR ++ cmp ++ [and1])

runFnCall :: Monoid a
          => IR.BasicBlock'Instruction
          -> RWS IR.IrModule a InstrSelectState [Instr]
runFnCall callInstr = do
  runFnCallState .= initRunFnCallState

  (instrsFn, regIdxFn) <- runValue (callInstr ^. IR.fnCallFn)
  let tupleConcat xs = (map fst xs, map snd xs)
  (instrsArgs, regIdxArgs) <-
    tupleConcat <$> mapM runValue (callInstr ^. IR.fnCallArgs)

  let args = zip (callInstr ^. IR.fnCallArgs) (callInstr ^. IR.fnCallArgByval)
  firstPassInstrs <- mapM (runFnCallArg True) args
  secondPassInstrs <- mapM (runFnCallArg False) args
  -- TODO: return values / varargs %al

  stackSize <- use (runFnCallState . curStackSize)
  stackParamsRegionSize %= max stackSize
  let instrs = instrsFn ++
               concat (instrsArgs ++ firstPassInstrs ++ secondPassInstrs)
  return instrs

runFnCallArg :: Monoid a
             => Bool
             -> (IR.Value, Bool) -- arg, isByVal
             -> RWS IR.IrModule a InstrSelectState [Instr]
runFnCallArg isFirstPass (arg, True) = do
  (sz, align) <- getTypeSizeAndAlign $ arg ^. IR.type' . IR.pointeeType
  stackSize <- use $ runFnCallState . curStackSize
  let stackSize =
        if stackSize `mod` align == 0
          then stackSize
          else stackSize - (stackSize `mod` align) + align

  -- TODO: copy / memcpy arg to (StackParam stackSize)

  let stackSize = stackSize + sz

  runFnCallState . curStackSize .= stackSize
  return [] -- TODO
runFnCallArg isFirstPass (arg, False) = do
  return [] -- TODO

getFuncDef :: Monoid a => RWS IR.IrModule a InstrSelectState IR.FunctionDef
getFuncDef = do
  funcName <- use currentFuncName
  view (IR.functionDefs . at funcName . to unwrapMaybe)

getBasicBlock :: Monoid a
              => BasicBlockId
              -> RWS IR.IrModule a InstrSelectState IR.BasicBlock
getBasicBlock basicBlockId =
  (^. IR.bbs . at basicBlockId . to unwrapMaybe) <$> getFuncDef

getSuccessors :: IR.BasicBlock -> [BasicBlockId]
getSuccessors bb =
  case t ^. IR.kind of
    IR.BasicBlock'Terminator'BR -> [t ^. IR.brTarget]
    IR.BasicBlock'Terminator'COND_BR ->
      [t ^. IR.condBrTrue, t ^. IR.condBrFalse]
    IR.BasicBlock'Terminator'RETURN_VOID -> []
    IR.BasicBlock'Terminator'RETURN -> []
    IR.BasicBlock'Terminator'SWITCH ->
      t ^. IR.switchDefaultTarget : t ^. IR.switchCaseTarget
  where
    t = bb ^. IR.terminator

getTypeSizeAndAlign :: Monoid a
                    => IR.Type
                    -> RWS IR.IrModule a s (Int, Int)
getTypeSizeAndAlign tp = do
  case tp ^. IR.kind of
    IR.Type'INT8 -> return (1, 1)
    IR.Type'INT16 -> return (2, 2)
    IR.Type'INT32 -> return (4, 4)
    IR.Type'INT64 -> return (8, 8)
    IR.Type'FLOAT -> return (4, 4)
    IR.Type'DOUBLE -> return (8, 8)
    IR.Type'STRUCT -> do
      sd <- view (IR.structDefs . at (tp ^. IR.structId) . to unwrapMaybe)
      sizeAndAlignList <- mapM getTypeSizeAndAlign (sd ^. IR.type')
      let sz = sum $ map fst sizeAndAlignList
      let align = foldr (max . snd . snd) 1 $
                        filter fst $ zip (sd ^. IR.paddingOnly) sizeAndAlignList
      return (sz, align)
    IR.Type'POINTER -> return (8, 8)
    IR.Type'ARRAY -> do
      (sz, align) <- getTypeSizeAndAlign (tp ^. IR.arrayElemType)
      return (sz * (tp ^. IR.arraySize . to fromIntegral), align)

getRegSize :: Monoid a
           => RegIdx
           -> RWS r a InstrSelectState RegSize
getRegSize regIdx = use (regSize . at regIdx . to unwrapMaybe)

defineRegForIrId :: Monoid a
                 => (IR.Type, Word32)
                 -> RWS r a InstrSelectState RegIdx
defineRegForIrId (tp, irId) = do
  let sz = case tp ^. IR.kind of
        IR.Type'INT8 -> Byte
        IR.Type'INT16 -> Word
        IR.Type'INT32 -> Long
        IR.Type'INT64 -> Quad
        IR.Type'FLOAT -> F32
        IR.Type'DOUBLE -> F64
        IR.Type'POINTER -> Quad
        -- TODO: struct/array
  regIdx <- defineReg sz
  regIdxByIrId %= Map.insert (fromIntegral irId) regIdx
  return regIdx

defineReg :: Monoid a
          => RegSize
          -> RWS r a InstrSelectState RegIdx
defineReg sz = do
  newIdx <- lastRegIdx <+= 1
  regSize %= Map.insert (RegIdx newIdx) sz
  return $ RegIdx newIdx

isBinArithOp :: IR.BasicBlock'Instruction'Kind -> Bool
isBinArithOp k = b <= v && v <= e
  where
    b = fromEnum IR.BasicBlock'Instruction'BIT_OR
    v = fromEnum k
    e = fromEnum IR.BasicBlock'Instruction'UMOD

isCmpOp :: IR.BasicBlock'Instruction'Kind -> Bool
isCmpOp k = b <= v && v <= e
  where
    b = fromEnum IR.BasicBlock'Instruction'EQ
    v = fromEnum k
    e = fromEnum IR.BasicBlock'Instruction'UGEQ

unwrapMaybe :: Maybe a -> a
unwrapMaybe (Just x) = x