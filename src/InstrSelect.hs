{-# LANGUAGE TemplateHaskell #-}

module InstrSelect (run) where

import Control.Lens (Field2 (_2), at, use, uses, view, (^.), (%=))
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

data InstrSelectState = InstrSelectState
  { _visitedBasicBlocks :: Set.Set BasicBlockId
  , _currentFuncName :: FuncName }

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
              { _visitedBasicBlocks = Set.empty,
                _currentFuncName = funcName
              }

runBasicBlock :: BasicBlockId -> RWS IR.IrModule FuncBody InstrSelectState ()
runBasicBlock basicBlockId = do
  visited <- uses visitedBasicBlocks (Set.member basicBlockId)
  if visited
    then return ()
    else do
      visitedBasicBlocks %= Set.insert basicBlockId
      basicBlock <- getBasicBlock basicBlockId
      xs1 <- mconcat' $ map runPhi $ basicBlock ^. IR.phiNodes
      xs2 <- mconcat' $ map runInstr $ basicBlock ^. IR.instructions
      xs3 <- runTerminator $ basicBlock ^. IR.terminator
      tell [(basicBlockId, xs1 ++ xs2 ++ xs3)]
      foldl (>>) (return ()) $ map runBasicBlock $ getSuccessors basicBlock
  where
    mconcat' :: Monad m => [m [a]] -> m [a]
    mconcat' ms = concat <$> foldr h (return []) ms
    h :: Monad m => m [a] -> m [[a]] -> m [[a]]
    h m1 m2 = do x1 <- m1; x2 <- m2; return (x1 : x2)

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

runValue :: Monoid a
         => IR.Value
         -> RWS IR.IrModule a InstrSelectState Operand
runValue = undefined -- TODO

getFuncDef :: Monoid a => RWS IR.IrModule a InstrSelectState IR.FunctionDef
getFuncDef = do
  funcName <- use currentFuncName
  maybeFuncDef <- view (IR.functionDefs . at funcName)
  let Just funcDef = maybeFuncDef
  return funcDef

getBasicBlock :: Monoid a
              => BasicBlockId
              -> RWS IR.IrModule a InstrSelectState IR.BasicBlock
getBasicBlock basicBlockId = do
  funcDef <- getFuncDef
  let Just basicBlock = funcDef ^. IR.bbs . at basicBlockId
  return basicBlock

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