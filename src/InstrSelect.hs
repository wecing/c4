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

-- -- TODO: MEMany, GR, MREG
-- data RegMatcher
--   = IMM IntSize
--   | MEM IntSize
--   | REG IntSize

-- -- TODO: terminator, phi, value
-- data InstrMatcher
--   = InstrMatcher IR.BasicBlock'Instruction'Kind [RegMatcher]

data Instr = Instr String (Maybe RegSize) [Operand] deriving (Show)

-- out / uses / sets could probably just be RegIdx
--
-- data MatchResult = MatchResult
--   { outReg :: Maybe Operand,
--     usesRegs :: [Operand],
--     setsRegs :: [Operand],
--     fmtInstr :: [Operand] -> [Instr]
--   }

-- fmtOperand :: Operand -> String
-- fmtOperand _ = "TODO" -- TODO

-- withSizeSuffix :: String -> IntSize -> String
-- withSizeSuffix s Byte = s ++ "b"
-- withSizeSuffix s Word = s ++ "w"
-- withSizeSuffix s Long = s ++ "l"
-- withSizeSuffix s Quad = s ++ "q"

-- simpleBinOpFmt :: String -> IntSize -> [Operand] -> [String]
-- simpleBinOpFmt op sz rs =
--   [format "{0} {1}, {2}" $ withSizeSuffix op sz : map fmtOperand rs]

-- TODO: imm, ptradd, etc
-- matchAdd :: [Operand] -> MatchResult
-- matchAdd [src, dst] =
--   case (src, dst) of
--     (Reg s1 _, Reg s2 _) | s1 == s2 -> simpleResult s1
--     (Reg s1 _, Mem s2 _) | s1 == s2 -> simpleResult s1
--     (Mem s1 _, Reg s2 _) | s1 == s2 -> simpleResult s1
--   where
--     simpleResult sz =
--       MatchResult
--         { outReg = Just dst,
--           usesRegs = [src, dst],
--           setsRegs = [dst],
--           fmtInstr = simpleBinOpFmt "add" sz
--         }

-------------------------------------

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
        m = runBasicBlock $ funcDef ^. IR.entryBb
        r = irModule
        s = InstrSelectState
              { _visitedBasicBlocks = Set.empty
              , _currentFuncName = funcName
              , _lastRegIdx = 0
              , _regSize = Map.empty
              , _regIdxByIrId = Map.empty -- TODO: func args
              }

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
runPhi _ = return [] -- TODO

runInstr :: Monoid a
         => IR.BasicBlock'Instruction
         -> RWS IR.IrModule a InstrSelectState [Instr]
runInstr _ = return [] -- TODO

runTerminator :: Monoid a
              => IR.BasicBlock'Terminator
              -> RWS IR.IrModule a InstrSelectState [Instr]
runTerminator _ = return [] -- TODO

-- for cast_from, always return Reg
runValue :: Monoid a
         => IR.Value
         -> RWS IR.IrModule a InstrSelectState ([Instr], Operand)
runValue irValue = do
  let Just v = irValue ^. IR.maybe'v
  case v of
    IR.Value'I8 x -> return ([], Imm (fromIntegral x))
    IR.Value'I16 x -> return ([], Imm (fromIntegral x))
    IR.Value'I32 x -> return ([], Imm (fromIntegral x))
    IR.Value'I64 x -> return ([], Imm (fromIntegral x))
    IR.Value'F32 x -> return ([], ImmF (Left x))
    IR.Value'F64 x -> return ([], ImmF (Right x))
    -- TODO: aggregate
    IR.Value'Address' addr ->
      return ([], Addr (Text.unpack (addr ^. IR.symbol)) (addr ^. IR.offset))
    IR.Value'CastFrom fromIrValue -> do
      (baseInstrs, fromValue) <- runValue fromIrValue
      (copyInstrs, fromReg, fromSz) <- case fromValue of
        Reg regIdx -> do
          fromSz <- use (regSize . at regIdx . to unwrapMaybe)
          return ([], fromValue, fromSz)
        MReg _ _ -> error "unreachable"
        _ -> do
          let sz = case fromValue of
                Imm _ -> Quad
                ImmF (Left _) -> F32
                ImmF (Right _) -> F64
                Addr _ _ -> Quad
          regIdx <- defineReg sz
          let mov = Instr "mov" (Just sz) [fromValue, Reg regIdx]
          return ([mov], Reg regIdx, sz)
      let toSz = case irValue ^. IR.type' . IR.kind of
            IR.Type'INT8 -> Byte
            IR.Type'INT16 -> Word
            IR.Type'INT32 -> Long
            IR.Type'INT64 -> Quad
            IR.Type'FLOAT -> F32
            IR.Type'DOUBLE -> F64
            IR.Type'BOOLEAN -> Byte
      toRegIdx <- defineReg toSz
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
                  , Instr "setnz" Nothing [toReg]]
            -- F32/F64 <-> intN
            -- TODO: unsigned?
            -- TODO: cvtXX int arg must be r32/r64
            (F32, _) -> return [Instr "cvtss2si" Nothing regs]
            (F64, _) -> return [Instr "cvtsd2si" Nothing regs]
            (_, F32) -> return [Instr "cvtsi2ss" Nothing regs]
            (_, F64) -> return [Instr "cvtsi2sd" Nothing regs]
            -- intN -> intN
            (Byte, _) -> return [Instr "mov" (Just Byte) regs]
            (_, Byte) -> return [Instr "mov" (Just Byte) regs]
            (Word, _) -> return [Instr "mov" (Just Word) regs]
            (_, Word) -> return [Instr "mov" (Just Word) regs]
            (Long, _) -> return [Instr "mov" (Just Long) regs]
            (_, Long) -> return [Instr "mov" (Just Long) regs]
            (Quad, Quad) -> return [Instr "mov" (Just Quad) regs]
      return (baseInstrs ++ copyInstrs ++ castInstrs, toReg)
    -- TODO: undef
    IR.Value'IrId irIdRaw -> do
      let irId = fromIntegral irIdRaw
      regIdx <- use (regIdxByIrId . at irId . to unwrapMaybe)
      return ([], Reg regIdx)

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

defineReg :: Monoid w
          => RegSize
          -> RWS r w InstrSelectState RegIdx
defineReg sz = do
  newIdx <- lastRegIdx <+= 1
  regSize %= Map.insert (RegIdx newIdx) sz
  return $ RegIdx newIdx

unwrapMaybe :: Maybe a -> a
unwrapMaybe (Just x) = x