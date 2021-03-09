{-# LANGUAGE TemplateHaskell #-}

module InstrSelect (run) where

import Control.Lens (Field2 (_2), at, to, use, uses, view, (^.), (%=), (<+=))
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
  | SizeAndAlign Int Int deriving (Eq, Show)

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
    deriving (Show)

data Instr = Instr String (Maybe RegSize) [Operand] deriving (Show)

type FuncName = Text.Text

type BasicBlockId = Word32

data InstrSelectState = InstrSelectState
  { _visitedBasicBlocks :: Set.Set BasicBlockId
  , _currentFuncName :: FuncName
  , _lastRegIdx :: Int
  , _regSize :: Map.Map RegIdx RegSize
  , _regIdxByIrId :: Map.Map Int RegIdx
  }

makeLenses ''InstrSelectState

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
    IR.BasicBlock'Instruction'VALUE -> do
      case irInstr ^. IR.value . IR.maybe'v of
        Just (IR.Value'Undef _) -> do
          defineRegForIrId (irInstr ^. IR.type', irInstr ^. IR.id)
          return []
        _ -> do
          (instrs, regIdx) <- runValue (irInstr ^. IR.value)
          regSize <- use (regSize . at regIdx . to unwrapMaybe)
          r <- defineRegForIrId (irInstr ^. IR.type', irInstr ^. IR.id)
          let mov = Instr "mov" (Just regSize) [Reg regIdx, Reg r]
          return (instrs ++ [mov])
    -- TODO: generic bin op processing?
    IR.BasicBlock'Instruction'ADD -> do
      (instrsL, regIdxL) <- runValue (irInstr ^. IR.binOpLeft)
      (instrsR, regIdxR) <- runValue (irInstr ^. IR.binOpRight)
      regSize <- use (regSize . at regIdxR . to unwrapMaybe)
      -- TODO: assuming szL == szR == szDst
      let add = Instr "add" (Just regSize) [Reg regIdxL, Reg regIdxR]
      r <- defineRegForIrId (irInstr ^. IR.type', irInstr ^. IR.id)
      let mov = Instr "mov" (Just regSize) [Reg regIdxR, Reg r]
      return (instrsL ++ instrsR ++ [add, mov])
    _ -> do
      return [] -- TODO

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
      fromSz <- use (regSize . at fromRegIdx . to unwrapMaybe)
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
      regSize <- use (regSize . at fromIdx . to unwrapMaybe)
      toIdx <- defineReg regSize
      return ([Instr "mov" (Just regSize) [Reg fromIdx, Reg toIdx]], toIdx)

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

defineRegForIrId :: Monoid a
                 => (IR.Type, Word32)
                 -> RWS IR.IrModule a InstrSelectState RegIdx
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

unwrapMaybe :: Maybe a -> a
unwrapMaybe (Just x) = x