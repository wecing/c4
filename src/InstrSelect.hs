{-# LANGUAGE TemplateHaskell #-}

module InstrSelect (run) where

import Control.Lens (Field2 (_2), uses, views, (^.), (%=))
import Control.Lens.TH
import Control.Monad.RWS.Lazy (RWS, asks, evalRWS, gets, modify)
import Data.Int (Int64)
import Data.Map ((!))
import qualified Data.Map as Map
import qualified Data.ProtoLens.Runtime.Data.Text as Text
import qualified Data.Set as Set
import Data.Word (Word32)
import qualified Proto.Ir as IR
import qualified Proto.Ir_Fields as IR
import Text.Format (format)

data IntSize = Byte | Word | Long | Quad deriving (Eq)

newtype RegIdx = RegIdx Int

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

data Operand
  = Imm IntSize Int64
  | Mem IntSize RegIdx
  | Reg IntSize RegIdx -- some machine reg for integers
  | FReg IntSize RegIdx -- some machine reg for floating point numbers
  | MReg IntSize MachineReg -- a specific machine reg

-- TODO: MEMany, GR, MREG
data RegMatcher
  = IMM IntSize
  | MEM IntSize
  | REG IntSize

-- TODO: terminator, phi, value
data InstrMatcher
  = InstrMatcher IR.BasicBlock'Instruction'Kind [RegMatcher]

data MatchResult = MatchResult
  { outReg :: Maybe Operand,
    usesRegs :: [Operand],
    setsRegs :: [Operand],
    fmtInstr :: [Operand] -> [String]
  }

fmtOperand :: Operand -> String
fmtOperand _ = "TODO" -- TODO

withSizeSuffix :: String -> IntSize -> String
withSizeSuffix s Byte = s ++ "b"
withSizeSuffix s Word = s ++ "w"
withSizeSuffix s Long = s ++ "l"
withSizeSuffix s Quad = s ++ "q"

simpleBinOpFmt :: String -> IntSize -> [Operand] -> [String]
simpleBinOpFmt op sz rs =
  [format "{0} {1}, {2}" $ withSizeSuffix op sz : map fmtOperand rs]

-- TODO: imm, ptradd, etc
matchAdd :: [Operand] -> MatchResult
matchAdd [src, dst] =
  case (src, dst) of
    (Reg s1 _, Reg s2 _) | s1 == s2 -> simpleResult s1
    (Reg s1 _, Mem s2 _) | s1 == s2 -> simpleResult s1
    (Mem s1 _, Reg s2 _) | s1 == s2 -> simpleResult s1
  where
    simpleResult sz =
      MatchResult
        { outReg = Just dst,
          usesRegs = [src, dst],
          setsRegs = [dst],
          fmtInstr = simpleBinOpFmt "add" sz
        }

-------------------------------------

type FuncName = Text.Text

type BasicBlockId = Word32

type Instr = String

newtype InstrSelectState = InstrSelectState
  { _visitedBasicBlocks :: Set.Set BasicBlockId }

makeLenses ''InstrSelectState

type FuncBody = [(BasicBlockId, [Instr])]

run :: IR.IrModule -> Map.Map FuncName FuncBody
run irModule = Map.map runFunc' $ irModule ^. IR.functionDefs
  where
    runFunc' :: IR.FunctionDef -> FuncBody
    runFunc' funcDef = snd $ evalRWS m r s
      where
        m = runBasicBlock $ funcDef ^. IR.entryBb
        r = (irModule, funcDef)
        s = InstrSelectState {_visitedBasicBlocks = Set.empty}

runBasicBlock ::
  BasicBlockId ->
  RWS (IR.IrModule, IR.FunctionDef) FuncBody InstrSelectState ()
runBasicBlock basicBlockId = do
  visited <- uses visitedBasicBlocks (Set.member basicBlockId)
  if visited
    then return ()
    else do
      visitedBasicBlocks %= Set.insert basicBlockId
      basicBlock <- views (_2 . IR.bbs) (! basicBlockId)
      return () -- TODO: recurse