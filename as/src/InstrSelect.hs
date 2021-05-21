{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module InstrSelect where

import Control.Lens (at, to, use, uses, view, (^.), (.=), (%=), (<+=), (+=))
import Control.Lens.TH (makeLenses)
import Control.Monad (forM_, liftM2, when)
import Control.Monad.RWS.Lazy (RWS, evalRWS, execRWS, state, tell)
import Data.Int (Int64)
import Data.List (tails)
import Data.Map ((!))
import Data.Maybe (isJust, isNothing, fromJust)
import Data.Word (Word32)

import qualified Control.Monad.ST.Lazy as ST
import qualified Data.Map as Map
import qualified Data.ProtoLens.Runtime.Data.Text as Text
import qualified Data.Set as Set
import qualified Data.STRef.Lazy as ST

import qualified Proto.Ir as IR
import qualified Proto.Ir_Fields as IR

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
    deriving (Eq, Show, Ord)

data Operand
  = Imm Int64
  | ImmF (Either Float Double)
  | Reg RegIdx -- mem, general reg, or sse reg
  | MReg RegSize MachineReg -- a specific machine reg
  | Addr String Int64 -- link time constant
  -- to get addr, use lea
  --
  -- byte offset relative to head of stack params region (callers view)
  | StackParam Int
  -- like StackParam, but for callees; equals to n+16(%rbp)
  | CalleeStackParam Int
  -- byte offset into reg save area
  | SavedReg Int
  -- only used in reg alloc, replaces StackParam/CalleeStackParam/SavedReg.
  -- e.g. RbpOffset -4 == -4(%rbp)
  | RbpOffset Int
    deriving (Eq, Show)

data Instr = Instr String (Maybe RegSize) [Operand] deriving (Show)

type FuncName = Text.Text

type BasicBlockId = Word32

data RunFnCallState = RunFnCallState
  { _remIntRegs :: [MachineReg]
  , _remSseRegs :: [MachineReg]
  , _curStackSize :: Int
  } deriving (Show)
data InstrSelectState = InstrSelectState
  { _visitedBasicBlocks :: Set.Set BasicBlockId
  , _currentFuncName :: FuncName
  , _lastRegIdx :: Int
  , _regSize :: Map.Map RegIdx RegSize
  , _regIdxByIrId :: Map.Map Int RegIdx
  , _stackParamsRegionSize :: Int
  , _runFnCallState :: RunFnCallState -- init'ed for every runFnCall call
  , _regSaveRegionSize :: Int
  } deriving (Show)

makeLenses ''RunFnCallState
makeLenses ''InstrSelectState

initRunFnCallState :: RunFnCallState
initRunFnCallState = RunFnCallState
  { _remIntRegs = [RDI, RSI, RDX, RCX, R8, R9]
  , _remSseRegs = [XMM0, XMM1, XMM2, XMM3, XMM4, XMM5, XMM6, XMM7]
  , _curStackSize = 0
  }

type FuncBody = Map.Map BasicBlockId [Instr]

run :: IR.IrModule -> Map.Map FuncName (InstrSelectState, FuncBody)
run irModule = Map.mapWithKey runFunc' $ irModule ^. IR.functionDefs
  where
    runFunc' :: FuncName -> IR.FunctionDef -> (InstrSelectState, FuncBody)
    runFunc' funcName funcDef = (s'', dePhi phiNodes regIdxSz funcBody')
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
              , _regSaveRegionSize = 0
              }
        (s', funcBody) = execRWS m r s
        phiNodes = Map.map (^. IR.phiNodes) $ funcDef ^. IR.bbs
        regIdxSz = Map.map (\idx -> (idx, (s' ^. regSize) ! idx))
                           $ s' ^. regIdxByIrId

        -- prologue:
        --   int %1 / &T* %1 =>
        --     mov %rdi, SavedReg 0
        --     mov SavedReg 0, %1
        --   float %1 =>
        --     mov %xmm0, SavedReg 48
        --     mov SavedReg 48, %1
        --   overfloating %n =>
        --     mov CalleeStackParam X, %n
        --   byval &T* %n =>
        --     lea CalleeStackParam X, %n
        prologue :: (Int, [Instr])
        prologue = ST.runST $ do
          gps <- ST.newSTRef $ zip [RDI, RSI, RDX, RCX, R8, R9] [0 :: Int, 8..]
          fps <- ST.newSTRef $
                   zip [XMM0, XMM1, XMM2, XMM3, XMM4, XMM5, XMM6, XMM7]
                       [48 :: Int, 64..]
          paramsOffset <- ST.newSTRef (0 :: Int)
          saveInstrs <- ST.newSTRef ([] :: [Instr])
          initInstrs <- ST.newSTRef ([] :: [Instr])
          regSaveRegionSz <- ST.newSTRef (0 :: Int)
          let args = zip3 (funcDef ^. IR.argByval)
                          (funcDef ^. IR.argTypes)
                          (map fromIntegral (funcDef ^. IR.args) :: [Int])
          forM_ args $ \(isByVal, tp, irId) -> do
            let tp' = if isByVal then tp ^. IR.pointeeType else tp
            let m' = getTypeSizeAndAlign tp'
                     :: RWS IR.IrModule [()] () (Int, Int)
            let ((sz, _), _) = evalRWS m' irModule ()
            let reg = s' ^. regIdxByIrId . at irId . to fromJust
            let regSz = s' ^. regSize . at reg . to fromJust
            offset <- ST.readSTRef paramsOffset
            if isByVal then do
              let instr =
                    Instr "lea" (Just Quad) [CalleeStackParam offset, Reg reg]
              ST.modifySTRef paramsOffset (+ roundUp sz 8)
              ST.modifySTRef initInstrs (++ [instr])
            else if (tp ^. IR.kind)
                    `elem` [IR.Type'FLOAT, IR.Type'DOUBLE] then do
              fps' <- ST.readSTRef fps
              if null fps' then do
                let instr = Instr "mov" (Just regSz)
                                        [CalleeStackParam offset, Reg reg]
                ST.modifySTRef paramsOffset (+ roundUp sz 8)
                ST.modifySTRef initInstrs (++ [instr])
              else do
                let (mreg, off) = head fps'
                let saveI = Instr "mov" (Just regSz)
                                        [MReg regSz mreg, SavedReg off]
                ST.modifySTRef saveInstrs (++ [saveI])
                let initI = Instr "mov" (Just regSz)
                                        [SavedReg off, Reg reg]
                ST.modifySTRef initInstrs (++ [initI])
                ST.modifySTRef fps tail
                ST.modifySTRef regSaveRegionSz (max $ off + 8)
            else if (tp ^. IR.kind)
                    `elem` [ IR.Type'INT8, IR.Type'INT16, IR.Type'INT32
                           , IR.Type'INT64, IR.Type'POINTER ] then do
              gps' <- ST.readSTRef gps
              if null gps' then do
                let instr = Instr "mov" (Just regSz)
                                        [CalleeStackParam offset, Reg reg]
                ST.modifySTRef paramsOffset (+ roundUp sz 8)
                ST.modifySTRef initInstrs (++ [instr])
              else do
                let (mreg, off) = head gps'
                let saveI = Instr "mov" (Just regSz)
                                        [MReg regSz mreg, SavedReg off]
                ST.modifySTRef saveInstrs (++ [saveI])
                let initI = Instr "mov" (Just regSz)
                                        [SavedReg off, Reg reg]
                ST.modifySTRef initInstrs (++ [initI])
                ST.modifySTRef gps tail
                ST.modifySTRef regSaveRegionSz (max $ off + 8)
            else
              error $ "illegal fn call arg type: " ++ show (tp ^. IR.kind)

            when (funcDef ^. IR.isVararg) $ do
              fps' <- ST.readSTRef fps
              gps' <- ST.readSTRef gps
              let f (mreg, off) = Instr "mov" (Just F64)
                                              [MReg F64 mreg, SavedReg off]
              let g (mreg, off) = Instr "mov" (Just Quad)
                                              [MReg Quad mreg, SavedReg off]
              ST.modifySTRef saveInstrs (++ map g gps' ++ map f fps')
              ST.writeSTRef regSaveRegionSz (8 * (6 + 8))

          liftM2 (,) (ST.readSTRef regSaveRegionSz)
                     $ liftM2 (++) (ST.readSTRef saveInstrs)
                                   (ST.readSTRef initInstrs)

        funcBody' =
          Map.insertWith (++) (funcDef ^. IR.entryBb) (snd prologue) funcBody
        s'' = s' { _regSaveRegionSize = fst prologue }

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
      tell $ Map.singleton basicBlockId $ xs1 ++ xs2 ++ xs3
      mapM_ runBasicBlock $ getSuccessors basicBlock

runPhi :: Monoid a
       => IR.BasicBlock'PhiNode
       -> RWS IR.IrModule a InstrSelectState [Instr]
runPhi phi = do
  _ <- defineRegForIrId (phi ^. IR.type', phi ^. IR.id)
  return [] -- phi will be revisited and eliminated in reg allocator

runInstr :: Monoid a
         => IR.BasicBlock'Instruction
         -> RWS IR.IrModule a InstrSelectState [Instr]
runInstr irInstr =
  case irInstr ^. IR.kind of
    IR.BasicBlock'Instruction'ALLOCA -> undefined -- TODO
    IR.BasicBlock'Instruction'STORE -> do
      (instrsDst, regIdxDst) <- runValue (irInstr ^. IR.storeDst)
      (instrsSrc, regIdxSrc) <- runValue (irInstr ^. IR.storeSrc)
      regSz <- getRegSize regIdxSrc
      -- "store a, b" means "mov a, (b)"
      let mov = Instr "store" (Just regSz) [Reg regIdxSrc, Reg regIdxDst]
      return (instrsDst ++ instrsSrc ++ [mov])
    IR.BasicBlock'Instruction'LOAD -> do
      (instrsSrc, regIdxSrc) <- runValue (irInstr ^. IR.loadSrc)
      regIdxDst <- defineRegForIrId (irInstr ^. IR.type', irInstr ^. IR.id)
      regSz <- getRegSize regIdxDst
      -- "load a, b" means "mov (a), b"
      let mov = Instr "load" (Just regSz) [Reg regIdxSrc, Reg regIdxDst]
      return (instrsSrc ++ [mov])
    IR.BasicBlock'Instruction'MEMCPY -> undefined -- TODO
    IR.BasicBlock'Instruction'NEG -> undefined -- TODO
    IR.BasicBlock'Instruction'NOT -> do
      (instrsSrc, regIdxSrc) <- runValue (irInstr ^. IR.loadSrc)
      regIdxDst <- defineRegForIrId (irInstr ^. IR.type', irInstr ^. IR.id)
      regSz <- getRegSize regIdxDst
      -- NOT could only be applied to integral types
      let ins = Instr "not" (Just regSz) [Reg regIdxSrc]
      let mov = Instr "mov" (Just regSz) [Reg regIdxSrc, Reg regIdxDst]
      return (instrsSrc ++ [ins, mov])
    k | isBinArithOp k -> runBinArithOp irInstr k
    k | isCmpOp k -> runCmpOp irInstr k
    IR.BasicBlock'Instruction'FN_CALL -> runFnCall irInstr
    IR.BasicBlock'Instruction'VA_START -> undefined -- TODO
    IR.BasicBlock'Instruction'VA_ARG -> undefined -- TODO
    IR.BasicBlock'Instruction'VA_END -> undefined -- TODO
    IR.BasicBlock'Instruction'VA_COPY -> undefined -- TODO
    IR.BasicBlock'Instruction'VALUE -> do
      case irInstr ^. IR.value . IR.maybe'v of
        Just (IR.Value'Undef _) -> do
          _ <- defineRegForIrId (irInstr ^. IR.type', irInstr ^. IR.id)
          return []
        _ -> do
          (instrs, regIdx) <- runValue (irInstr ^. IR.value)
          regSz <- getRegSize regIdx
          r <- defineRegForIrId (irInstr ^. IR.type', irInstr ^. IR.id)
          let mov = Instr "mov" (Just regSz) [Reg regIdx, Reg r]
          return (instrs ++ [mov])
    k -> error $ "illegal IR: instr kind " ++ show k

runTerminator :: Monoid a
              => IR.BasicBlock'Terminator
              -> RWS IR.IrModule a InstrSelectState [Instr]
runTerminator term = do
  case term ^. IR.kind of
    IR.BasicBlock'Terminator'BR -> do
      label <- getLabel $ term ^. IR.brTarget
      return [Instr "jmp" (Just Quad) [Addr label 0]]
    IR.BasicBlock'Terminator'COND_BR -> do
      labelTrue <- getLabel $ term ^. IR.condBrTrue
      labelFalse <- getLabel $ term ^. IR.condBrFalse
      (instrsCond, regIdxCond) <- runValue $ term ^. IR.condBrCond
      let instrsBr =
            [ Instr "cmp" (Just Byte) [Imm 0, Reg regIdxCond]
            , Instr "je" (Just Quad) [Addr labelFalse 0]
            , Instr "jmp" (Just Quad) [Addr labelTrue 0]
            ]
      return $ instrsCond ++ instrsBr
    IR.BasicBlock'Terminator'RETURN_VOID ->
      return [ Instr "ret" Nothing [] ]
    IR.BasicBlock'Terminator'RETURN -> do
      (instrsVal, regIdxVal) <- runValue $ term ^. IR.returnValue
      regSzVal <- getRegSize regIdxVal
      let instrsRet = case term ^. IR.returnValue . IR.type' . IR.kind of
            IR.Type'STRUCT -> undefined -- TODO: copy regIdxVal to RAX/RDX/XMM
            k ->
              let isSse = k `elem` [IR.Type'FLOAT, IR.Type'DOUBLE]
                  mr = if isSse then XMM0 else RAX
              in [ Instr "mov" (Just regSzVal) [Reg regIdxVal, MReg regSzVal mr]
                 , Instr "ret" Nothing []]
      return $ instrsVal ++ instrsRet
    IR.BasicBlock'Terminator'SWITCH -> do
      undefined -- TODO
    k -> error $ "illegal IR: terminator kind " ++ show k

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
    IR.Value'Aggregate' _ -> undefined -- TODO
    IR.Value'Address' addr -> do
      let imm = Addr (Text.unpack (addr ^. IR.symbol)) (addr ^. IR.offset)
      regIdx <- defineReg Quad
      return ([Instr "lea" (Just Quad) [imm, Reg regIdx]], regIdx)
    IR.Value'CastFrom fromIrValue -> do
      (baseInstrs, fromRegIdx) <- runValue fromIrValue
      fromSz <- getRegSize fromRegIdx
      let getSzAndIsUnsigned v' = case (v' :: IR.Value) ^. IR.type' . IR.kind of
            IR.Type'INT8 -> (Byte, v' ^. IR.type' . IR.intIsUnsigned)
            IR.Type'INT16 -> (Word, v' ^. IR.type' . IR.intIsUnsigned)
            IR.Type'INT32 -> (Long, v' ^. IR.type' . IR.intIsUnsigned)
            IR.Type'INT64 -> (Quad, v' ^. IR.type' . IR.intIsUnsigned)
            IR.Type'FLOAT -> (F32, False)
            IR.Type'DOUBLE -> (F64, False)
            IR.Type'POINTER -> (Quad, True)
            IR.Type'BOOLEAN -> (Byte, False)
            k -> error $ "illegal IR: cannot cast to " ++ show k
      let (toSz, toUnsigned) = getSzAndIsUnsigned irValue
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
            (Byte, _) -> let op = if toUnsigned then "movzb" else "movsb" in
                         return [Instr op (Just toSz) regs]
            (_, Byte) -> return [Instr "mov" (Just toSz) regs]
            (Word, _) -> let op = if toUnsigned then "movzw" else "movsw" in
                         return [Instr op (Just toSz) regs]
            (_, Word) -> return [Instr "mov" (Just toSz) regs]
            (Long, _) -> let op = if toUnsigned then "movzl" else "movsl" in
                         return [Instr op (Just toSz) regs]
            (_, Long) -> return [Instr "mov" (Just toSz) regs]
            (Quad, Quad) -> return [Instr "mov" (Just toSz) regs]
      return (baseInstrs ++ castInstrs, toRegIdx)
    IR.Value'Undef _ -> undefined -- TODO
    IR.Value'IrId irIdRaw -> do
      let irId = fromIntegral irIdRaw
      fromIdx <- use (regIdxByIrId . at irId . to fromJust)
      regSz <- getRegSize fromIdx
      toIdx <- defineReg regSz
      return ([Instr "mov" (Just regSz) [Reg fromIdx, Reg toIdx]], toIdx)

runBinArithOp :: Monoid a
              => IR.BasicBlock'Instruction
              -> IR.BasicBlock'Instruction'Kind
              -> RWS IR.IrModule a InstrSelectState [Instr]
runBinArithOp irInstr k = do
  (instrsL, regIdxL) <- runValue (irInstr ^. IR.binOpLeft)
  (instrsR, regIdxR) <- runValue (irInstr ^. IR.binOpRight)
  regSz <- getRegSize regIdxR
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
        _ -> error $ "not a bin op: " ++ show k
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
            [ Instr "mov" (Just regSz) [Imm 0, MReg regSz RDX]
            , Instr "mov" (Just regSz) [Reg regIdxR, MReg regSz RAX]
            , Instr op (Just regSz) [Reg regIdxL]
            , Instr "mov" (Just regSz) [MReg regSz RAX, Reg r]
            ]
        | isIntMod =
            [ Instr "mov" (Just regSz) [Imm 0, MReg regSz RDX]
            , Instr "mov" (Just regSz) [Reg regIdxR, MReg regSz RAX]
            , Instr op (Just regSz) [Reg regIdxL]
            , Instr "mov" (Just regSz) [MReg regSz RDX, Reg r]
            ]
        | otherwise =
            [ Instr op (Just regSz) [Reg regIdxR, Reg regIdxL]
            , Instr "mov" (Just regSz) [Reg regIdxL, Reg r]
            ]
  return (instrsL ++ instrsR ++ arith)

runCmpOp :: Monoid a
         => IR.BasicBlock'Instruction
         -> IR.BasicBlock'Instruction'Kind
         -> RWS IR.IrModule a InstrSelectState [Instr]
runCmpOp irInstr k = do
  (instrsL, regIdxL) <- runValue (irInstr ^. IR.binOpLeft)
  (instrsR, regIdxR) <- runValue (irInstr ^. IR.binOpRight)
  regSz <- getRegSize regIdxR
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
            [ Instr "ucomi" (Just regSz) [Reg regIdxL, Reg regIdxR]
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
                _ -> error "illegal IR"
          let cmpArgs = if revCmpArgs
              then [Reg regIdxL, Reg regIdxR]
              else [Reg regIdxR, Reg regIdxL]
          return
            [ Instr "ucomi" (Just regSz) cmpArgs
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
                _ -> error "illegal IR"
          return
            [ Instr "cmp" (Just regSz) [Reg regIdxR, Reg regIdxL]
            , Instr op Nothing [Reg r]
            ]
  -- I actually do not understand why clang emits this
  let and1 = Instr "and" (Just Byte) [Imm 1, Reg r]
  return (instrsL ++ instrsR ++ cmp ++ [and1])

runFnCall :: Monoid a
          => IR.BasicBlock'Instruction
          -> RWS IR.IrModule a InstrSelectState [Instr]
runFnCall callInstr = do
  (instrsFn, regIdxFn) <- runValue (callInstr ^. IR.fnCallFn)
  let tupleConcat xs = (map fst xs, map snd xs)
  (instrsArgs, regIdxArgs) <-
    tupleConcat <$> mapM runValue (callInstr ^. IR.fnCallArgs)

  let args = zip3 (map (^. IR.type') $ callInstr ^. IR.fnCallArgs)
                  (callInstr ^. IR.fnCallArgByval)
                  regIdxArgs
  runFnCallState .= initRunFnCallState
  firstPassInstrs <- mapM (runFnCallArg True) args
  runFnCallState .= initRunFnCallState
  secondPassInstrs <- mapM (runFnCallArg False) args

  let isVarargs =
        callInstr ^. IR.fnCallFn . IR.type' . IR.pointeeType . IR.fnIsVararg
  alInstr <- if not isVarargs then return [] else do
        -- X86-64 ABI v0.21 3.5.6:
        -- When a function taking variable-arguments is called, %rax must be set
        -- to eight times the number of floating point parameters passed to the
        -- function in SSE registers.
        n <- use $ runFnCallState . remSseRegs . to length . to (8 -) . to (8 *)
        return [Instr "mov" (Just Byte) [Imm $ fromIntegral n, MReg Byte RAX]]

  -- something like callq *(%rax) or callq *-48(%rbp)
  let actualCallInstr = [Instr "call" (Just Quad) [Reg regIdxFn]]

  regIdxDst <- defineRegForIrId (callInstr ^. IR.type', callInstr ^. IR.id)
  regSzDst <- getRegSize regIdxDst
  let mvRet = case callInstr ^. IR.type' . IR.kind of
        IR.Type'STRUCT ->
          undefined -- TODO: copy RAX/RDX/XMM to regIdsDst (should be a ptr?)
        k ->
          let isSse = k `elem` [IR.Type'FLOAT, IR.Type'DOUBLE]
              mr = if isSse then XMM0 else RAX
          in [Instr "mov" (Just regSzDst) [MReg regSzDst mr, Reg regIdxDst]]

  stackSize <- use (runFnCallState . curStackSize)
  stackParamsRegionSize %= max stackSize
  let instrs = instrsFn ++
               concat (instrsArgs ++ firstPassInstrs ++ secondPassInstrs) ++
               alInstr ++
               actualCallInstr ++
               mvRet
  return instrs

-- first pass: copy on-stack params
-- second pass: pass rest params
runFnCallArg :: Monoid a
             => Bool
             -> (IR.Type, Bool, RegIdx)
             -> RWS IR.IrModule a InstrSelectState [Instr]
runFnCallArg isFirstPass (irTp, isByVal, regIdx) = do
  let tp = irTp ^. (if isByVal then IR.pointeeType else to id)
  (sz, _) <- getTypeSizeAndAlign tp
  reg <- if isByVal then return Nothing else do
    let isIntType = case tp ^. IR.kind of
          IR.Type'INT8 -> True
          IR.Type'INT16 -> True
          IR.Type'INT32 -> True
          IR.Type'INT64 -> True
          IR.Type'FLOAT -> False
          IR.Type'DOUBLE -> False
          IR.Type'POINTER -> True
          k -> error $ "illegal fn call arg type: " ++ show k
    regs <- use $ runFnCallState
                  . (if isIntType then remIntRegs else remSseRegs)
    case regs of
      r : rs -> do
        runFnCallState . (if isIntType then remIntRegs else remSseRegs) .= rs
        return $ Just r
      _ -> return Nothing

  if isFirstPass && (isByVal || isNothing reg) then do
    stackSize <- use $ runFnCallState . curStackSize

    instrs <-
      if isByVal then do
        -- memcpy arg to (StackParam stackSize)
        let repCount = Imm $ fromIntegral sz `div` 8
        let rep1 = if repCount == Imm 0 then [] else
              [ Instr "mov" (Just Quad) [repCount, MReg Quad RCX]
              , Instr "lea" (Just Quad) [StackParam stackSize, MReg Quad RDI]
              , Instr "mov" (Just Quad) [Reg regIdx, MReg Quad RSI]
              -- Move RCX quadwords from (RSI) to (RDI)
              , Instr "repmovs" (Just Quad) []
              ]
        let stackSize' = stackSize + (sz - sz `mod` 8)
        let repCount' = Imm $ fromIntegral sz `mod` 8
        let rep2 = if repCount' == Imm 0 then [] else
              [ Instr "mov" (Just Quad) [repCount', MReg Quad RCX]
              , Instr "lea" (Just Quad) [StackParam stackSize', MReg Quad RDI]
              , Instr "mov" (Just Quad) [Reg regIdx, MReg Quad RSI]
              -- Move RCX bytes from (RSI) to (RDI)
              , Instr "repmovs" (Just Byte) []
              ]
        return (rep1 ++ rep2)
      else do
        -- copy arg to (StackParam stackSize)
        regSz <- getRegSize regIdx
        let mov = Instr "mov" (Just regSz) [Reg regIdx, StackParam stackSize]
        return [mov]

    runFnCallState . curStackSize += roundUp sz 8
    return instrs
  else if not isFirstPass && isJust reg then do
    regSz <- getRegSize regIdx
    let r = fromJust reg
    let mov = Instr "mov" (Just regSz) [Reg regIdx, MReg regSz r]
    return [mov]
  else return []

dePhi :: Map.Map BasicBlockId [IR.BasicBlock'PhiNode]
      -> Map.Map Int (RegIdx, RegSize)
      -> FuncBody
      -> FuncBody
dePhi phiNodes regIdxSz funcBody = r
  where
    -- for: %dst = phi [(bbId, %src), ...]
    -- produce: mov %src, %dst
    getMov :: BasicBlockId -> IR.BasicBlock'PhiNode -> Instr
    getMov bbId phi =
      let srcIrId = phi ^. IR.id
          dstIrId = snd $ head $ filter (\p -> fst p == bbId)
                                        $ zip (phi ^. IR.bbIds)
                                              (phi ^. IR.valIds)
          (src, sz) = regIdxSz ! (fromIntegral srcIrId :: Int)
          (dst, _) = regIdxSz ! (fromIntegral dstIrId :: Int)
      in Instr "mov" (Just sz) [Reg dst, Reg src]
    s :: [BasicBlockId]
    s = filter (`Map.notMember` funcBody) [1..]
    -- w: new bbs inserted for phi
    -- return: funcBody with updated jmp targets
    m :: RWS () FuncBody [BasicBlockId] FuncBody
    m = Map.fromList <$> mapM f1 (Map.toList funcBody)
    f1 :: (BasicBlockId, [Instr])
       -> RWS () FuncBody [BasicBlockId] (BasicBlockId, [Instr])
    f1 (bbId, instrs) = (bbId,) <$> mapM f2 (zip (repeat bbId) instrs)
    f2 :: (BasicBlockId, Instr) -> RWS () FuncBody [BasicBlockId] Instr
    f2 (bbId, Instr op sz args)
      | op `elem` ["jmp", "je"] =
        do let label = case args of
                [Addr x 0] -> x
                _ -> error "Cannot process jmp target"
           let targetBbId =
                let t = tail $ last $ filter (\x -> x /= [] && head x == '.')
                                             $ tails label
                in fromIntegral (read t :: Int) :: BasicBlockId
           let phiNodes' = filter (\n -> bbId `elem` n ^. IR.bbIds)
                                  (phiNodes ! targetBbId)
           if null phiNodes' then return $ Instr op sz args else do
             newBbId <- state (\s' -> (head s', tail s'))
             let newBbLabel = reverse (dropWhile ('.' /=) $ reverse label)
                              ++ show newBbId
             let movs = map (getMov bbId) phiNodes' ++ [Instr "jmp" sz args]
             tell $ Map.singleton newBbId movs
             return $ Instr op sz [Addr newBbLabel 0]
      | otherwise = return $ Instr op sz args
    (funcBody', newBbs) = evalRWS m () s
    r = newBbs `Map.union` funcBody' -- union is left-biased


getFuncDef :: Monoid a => RWS IR.IrModule a InstrSelectState IR.FunctionDef
getFuncDef = do
  funcName <- use currentFuncName
  view (IR.functionDefs . at funcName . to fromJust)

getBasicBlock :: Monoid a
              => BasicBlockId
              -> RWS IR.IrModule a InstrSelectState IR.BasicBlock
getBasicBlock basicBlockId =
  (^. IR.bbs . at basicBlockId . to fromJust) <$> getFuncDef

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
    _ -> error "illegal IR"
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
      sd <- view (IR.structDefs . at (tp ^. IR.structId) . to fromJust)
      sizeAndAlignList <- mapM getTypeSizeAndAlign (sd ^. IR.type')
      let sz = sum $ map fst sizeAndAlignList
      let align = foldr (max . snd . snd) 1 $
                        filter fst $ zip (sd ^. IR.paddingOnly) sizeAndAlignList
      return (sz, align)
    IR.Type'POINTER -> return (8, 8)
    IR.Type'ARRAY -> do
      (sz, align) <- getTypeSizeAndAlign (tp ^. IR.arrayElemType)
      return (sz * (tp ^. IR.arraySize . to fromIntegral), align)
    k -> error $ "unexpected type " ++ show k

getRegSize :: Monoid a
           => RegIdx
           -> RWS r a InstrSelectState RegSize
getRegSize regIdx = use (regSize . at regIdx . to fromJust)

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
        IR.Type'BOOLEAN -> Byte
        IR.Type'STRUCT -> undefined -- TODO
        IR.Type'ARRAY -> undefined -- TODO
        k -> error $ "unexpected type " ++ show k
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

getLabel :: Monoid a
         => BasicBlockId
         -> RWS r a InstrSelectState String
getLabel bb = do
  fnName <- uses currentFuncName Text.unpack
  return $ "." ++ fnName ++ "." ++ show bb

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

roundUp :: Int -> Int -> Int
roundUp x n = if x `mod` n == 0 then x else x + n - (x `mod` n)
