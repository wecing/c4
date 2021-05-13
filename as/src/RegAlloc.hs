{-# LANGUAGE TemplateHaskell #-}

module RegAlloc where

import Control.Lens (at, use, uses, (^.), (%=))
import Control.Lens.TH (makeLenses)
import Control.Monad.State.Lazy (State, execState)
import Data.Either (isLeft, lefts, rights)
import Data.List (sortBy)
import Data.Map ((!))
import Data.Maybe (fromJust, fromMaybe, mapMaybe)

import InstrSelect
  ( BasicBlockId
  , FuncBody
  , FuncName
  , Instr(..)
  , InstrSelectState
  , MachineReg(..)
  , Operand(..)
  , RegIdx
  , RegSize(..)
  , regSize
  )

import qualified Data.Map as Map
import qualified Data.ProtoLens.Runtime.Data.Text as Text
import qualified Data.Set as Set

import qualified Proto.Ir as IR

type InstrLabel = (String, Int)  -- (asm label, instr idx)

type Var = Either RegIdx MachineReg

data RegAllocState = RegAllocState
  { _iSelState :: InstrSelectState
  , _useMap :: Map.Map InstrLabel [Var]
  , _defMap :: Map.Map InstrLabel [Var]
  , _succMap :: Map.Map InstrLabel [InstrLabel]
  , _predMap :: Map.Map InstrLabel [InstrLabel]
  , _liveMap :: Map.Map InstrLabel [Var]
  , _interfMap :: Map.Map Var [Var]
  , _instrMap :: Map.Map InstrLabel Instr
  } deriving (Show)

makeLenses ''RegAllocState

-- https://www.cs.cmu.edu/~fp/courses/15411-f14/lectures/04-liveness.pdf
run :: IR.IrModule
    -> Map.Map FuncName (InstrSelectState, FuncBody)
    -> Map.Map FuncName FuncBody
run _ = Map.mapWithKey run'
  where
    run' :: FuncName -> (InstrSelectState, FuncBody) -> FuncBody
    run' funcName (instrSelectState, funcBody) = funcBody''
      where
        labelInstrs :: BasicBlockId -> [Instr] -> [(InstrLabel, Instr)]
        labelInstrs bbId instrs =
          let asmLabel = "." ++ Text.unpack funcName ++ "." ++ show bbId
          in zip (zip (repeat asmLabel) [0..]) instrs
        funcBody' :: Map.Map InstrLabel Instr
        funcBody' = Map.fromList $ concat $ Map.mapWithKey labelInstrs funcBody
        -- uds: use, def, succ
        getUds :: InstrLabel -> Instr -> ([Var], [Var], [InstrLabel])
        getUds (asmLabel, instrIdx) instr = (u, d, s)
          where
            next = (asmLabel, instrIdx + 1)
            u = fst $ getUD instr
            d = snd $ getUD instr
            s = case instr of
              Instr "jmp" _ [Addr target 0] -> [(target, 0)]
              Instr "je" _ [Addr target 0] -> [next, (target, 0)]
              Instr "ret" _ _ -> []
              _ -> [next]
        udsMap :: Map.Map InstrLabel ([Var], [Var], [InstrLabel])
        udsMap = Map.mapWithKey getUds funcBody'
        allVars :: [Var] -- contains dups
        allVars = concat $ concatMap (\(u, d, _) -> [u, d]) (Map.elems udsMap)
        raState = RegAllocState
          { _iSelState = instrSelectState
          , _useMap = Map.map (\(x, _, _) -> x) udsMap
          , _defMap = Map.map (\(_, x, _) -> x) udsMap
          , _succMap = Map.map (\(_, _, x) -> x) udsMap
          , _predMap = getPredMap $ Map.map (\(_, _, x) -> x) udsMap
          , _liveMap = Map.map (const []) udsMap
          , _interfMap = Map.fromList $ zip allVars (repeat [])
          , _instrMap = funcBody'
          }
        m = do
          mapM_ visitInstr $ Map.keys udsMap
          mapM_ updateInterfMap $ Map.keys udsMap
        raState' = execState m raState

        -- TODO: value should be (Either MachineReg Int)
        colors :: Map.Map RegIdx (Maybe MachineReg)
        colors = colorNodes (raState' ^. iSelState . regSize)
                            (raState' ^. interfMap)

        rewriteInstr :: Instr -> Maybe Instr
        rewriteInstr (Instr op sz rs) =
          case rs' of
            [a, b] | a == b && op == "mov" -> Nothing
            _ -> Just $ Instr op sz rs'
          where
            rewriteOperand :: Operand -> Operand
            rewriteOperand (Reg r) =
              maybe (Reg r) (MReg (fromMaybe Byte sz)) (colors ! r)
            rewriteOperand x = x
            rs' = map rewriteOperand rs

        funcBody'' = Map.map (mapMaybe rewriteInstr) funcBody

        -- TODO: spill
        -- TODO: ret/call

getUD :: Instr -> ([Var], [Var])
getUD (Instr "and" _ [Imm _, Reg y]) = ([Left y], [Left y])
getUD (Instr "cmp" _ [Imm _, Reg y]) = ([Left y], [])
getUD (Instr "cmp" _ [Reg x, Reg y]) = ([Left x, Left y], [])
getUD (Instr "cmpneq" _ [Reg x, Reg y]) = ([Left x, Left y], [])
getUD (Instr "div" _ [Reg x]) =
  ([Left x, Right RDX, Right RAX], [Right RDX, Right RAX])
getUD (Instr "idiv" _ [Reg x]) =
  ([Left x, Right RDX, Right RAX], [Right RDX, Right RAX])
getUD (Instr "je" _ [Addr _ _]) = ([], [])
getUD (Instr "jmp" _ [Addr _ _]) = ([], [])
getUD (Instr "lea" _ [Addr _ _, Reg y]) = ([], [Left y])
getUD (Instr "lea" _ [CalleeStackParam _, Reg y]) = ([], [Left y])
getUD (Instr "lea" _ [StackParam _, MReg _ y]) = ([], [Right y])
getUD (Instr "mov" _ [CalleeStackParam _, Reg y]) = ([], [Left y])
getUD (Instr "mov" _ [Imm _, MReg _ y]) = ([], [Right y])
getUD (Instr "mov" _ [Imm _, Reg y]) = ([], [Left y])
getUD (Instr "mov" _ [Locals _, Reg y]) = ([], [Left y])
getUD (Instr "mov" _ [MReg _ x, Locals _]) = ([Right x], [])
getUD (Instr "mov" _ [MReg _ x, Reg y]) = ([Right x], [Left y])
getUD (Instr "mov" _ [Reg x, MReg _ y]) = ([Left x], [Right y])
getUD (Instr "mov" _ [Reg x, StackParam _]) = ([Left x], [])
getUD (Instr "not" _ [Reg x]) = ([Left x], [Left x])
getUD (Instr "repmovs" _ []) = ([Right RCX, Right RSI, Right RDI], [])
getUD (Instr "ret" _ []) = (map Right [RAX, RDX, XMM0, XMM1], []) -- TODO
getUD (Instr "store" _ [Reg x, Reg y]) = ([Left x, Left y], [])
getUD (Instr "test" _ [Reg x, Reg y]) = ([Left x, Left y], [])
getUD (Instr "ucomi" _ [Reg x, Reg y]) = ([Left x, Left y], [])
getUD (Instr op _ [Reg x, Reg y])
  | op `Map.member` dict = dict ! op
  where
    -- here div could only be divss/divsd
    movLikes = [ "load", "mov", "movsb", "movsw", "movsl", "cvtss2sd"
               , "cvtsd2ss", "cvtss2si", "cvtsd2si", "cvtsi2ss", "cvtsi2sd" ]
    addLikes = [ "xorps", "or", "xor", "and", "shl", "sar", "shr", "add", "sub"
               , "mul", "imul", "div" ]
    dict = Map.fromList
              $ zip movLikes (repeat movLike) ++
                zip addLikes (repeat addLike)
    movLike = ([Left x], [Left y]) -- use x, def y
    addLike = ([Left x, Left y], [Left y]) -- use x y, def y
getUD (Instr op _ [Reg x])
  | op `elem` setLikes = ([], [Left x])
  where
    setLikes = [ "seta", "setae", "setb", "setbe", "sete", "setne", "setg"
               , "setge", "setl", "setle", "setp", "setnp", "setnz" ]
getUD (Instr "call" _ [Reg x]) = (Left x : us, ds)
  where
    -- TODO: obtain call reg usage info from InstrSelect
    us = map Right [ RAX, RDI, RSI, RDX, RCX, R8, R9
                   , XMM0, XMM1, XMM2, XMM3, XMM4, XMM5, XMM6, XMM7 ]
    ds = map Right [ RAX, RDX, XMM0, XMM1 ]
getUD instr = error $ "unexpected instr for getUD: " ++ show instr

getPredMap :: Map.Map InstrLabel [InstrLabel] -> Map.Map InstrLabel [InstrLabel]
getPredMap succMap' =
  foldr f emptyPredMap pairs
  where
    emptyPredMap :: Map.Map InstrLabel [InstrLabel]
    emptyPredMap = Map.map (const []) succMap'
    pairs :: [(InstrLabel, InstrLabel)]
    pairs = concatMap (\p -> zip (repeat $ fst p) (snd p)) $ Map.toList succMap'
    f :: (InstrLabel, InstrLabel)
      -> Map.Map InstrLabel [InstrLabel]
      -> Map.Map InstrLabel [InstrLabel]
    f (p, s) = Map.update (\ps -> Just (p : ps)) s

-- visit*: liveness analysis methods
visitInstr :: InstrLabel -> State RegAllocState ()
visitInstr label = uses (useMap . at label) fromJust >>= mapM_ (visitUse label)

visitUse :: InstrLabel -> Var -> State RegAllocState ()
visitUse label v = do
  isLive <- uses (liveMap . at label) ((v `elem`) . fromJust)
  if isLive then return () else do
    liveMap . at label %= fmap (v :)
    uses (predMap . at label) fromJust >>= mapM_ (`visitPred` v)

visitPred :: InstrLabel -> Var -> State RegAllocState ()
visitPred label v = do
  isLive <- uses (liveMap . at label) ((v `elem`) . fromJust)
  isDef <- uses (defMap . at label) ((v `elem`) . fromJust)
  if isLive || isDef then return () else do
    liveMap . at label %= fmap (v :)
    uses (predMap . at label) fromJust >>= mapM_ (`visitPred` v)

-- interference graph building
updateInterfMap :: InstrLabel -> State RegAllocState ()
updateInterfMap label = do
  instr <- uses (instrMap . at label) fromJust
  liveAfter <- do
      ss <- uses (succMap . at label) fromJust
      m <- use liveMap
      return $ concatMap (m !) ss
  ds <- uses (defMap . at label) fromJust
  us <- uses (useMap . at label) fromJust
  let isMov = case instr of
        Instr "mov" _ _ -> True
        _ -> False
  -- getInterfs :: Var -> [Var]
  let getInterfs d =
        let f = if isMov then (\x -> x /= d && (x `notElem` us)) else (/= d) in
          filter f liveAfter
  -- interfPairs :: [(Var, Var)] (bi-directional)
  let interfPairs = concatMap (\(x, y) -> [(x, y), (y, x)]) $
                           concatMap (\(x, y) -> zip (repeat x) y) $
                                     zip ds $ map getInterfs ds
  interfMap %= (\m -> Map.unionsWith
                      (\x y -> filter (`notElem` x) y ++ x)
                      (m : map (\(x, y) -> Map.singleton x [y]) interfPairs))

colorNodes :: Map.Map RegIdx RegSize
           -> Map.Map Var [Var]
           -> Map.Map RegIdx (Maybe MachineReg)
colorNodes regSizeMap interfMap' = ret'
  where
    ssaMRegs = [XMM0, XMM1, XMM2, XMM3, XMM4, XMM5, XMM6, XMM7]
    intMRegs = [RAX, RDI, RSI, RDX, RCX, R8, R9]

    isSsaVar :: Var -> Bool
    isSsaVar (Right mr) = mr `elem` ssaMRegs
    isSsaVar (Left r) = (regSizeMap ! r) `elem` [F64, F32]
    isIntVar :: Var -> Bool
    isIntVar (Right mr) = mr `elem` intMRegs
    isIntVar (Left r) = (regSizeMap ! r) `elem` [Byte, Word, Long, Quad]

    sanitizeInterfPair :: (Var, [Var]) -> Maybe (RegIdx, [Var])
    sanitizeInterfPair (Right _, _) = Nothing
    sanitizeInterfPair (Left r, vs) =
      case regSizeMap ! r of
        SizeAndAlign _ _ -> Nothing
        sz | sz `elem` [F64, F32] -> Just (r, filter isSsaVar vs)
        _ -> Just (r, filter isIntVar vs)

    -- sanitized: values are regs/mregs of the same type (int or ssa)
    interfMap'' :: Map.Map RegIdx [Var]
    interfMap'' =
      Map.fromList $ mapMaybe sanitizeInterfPair $ Map.toList interfMap'

    weights :: [(RegIdx, Int)]
    weights = Map.toList $ Map.map (length . rights) interfMap''
    interfMap''' :: Map.Map RegIdx (Set.Set RegIdx)
    interfMap''' = Map.map (Set.fromList . lefts) interfMap''

    findNextNode (revAcc, wt, m) = (r : revAcc, wt'', m')
      where
        (r, _) : wt' = sortBy (\x y -> compare (snd y) (snd x)) wt
        m' = Map.map (Set.delete r) $ Map.delete r m
        wt'' = map (\(reg, w) ->
                       (reg, w + if reg `Set.member` (m ! r) then 1 else 0))
                   wt'

    coloringOrder :: [RegIdx]
    coloringOrder =
      head $ mapMaybe (\(rs, x, _) -> if null x then Just $ reverse rs
                                                else Nothing)
           $ iterate findNextNode ([], weights, interfMap''')

    colorNextNode :: (Map.Map Var Int, [RegIdx]) -> (Map.Map Var Int, [RegIdx])
    colorNextNode (acc, r:rs) = (Map.insert (Left r) n acc, rs)
      where
        assignedNums = mapMaybe (`Map.lookup` acc) $ interfMap'' ! r
        n = head $ filter (`notElem` assignedNums) [0..]
    colorNextNode (acc, []) = (acc, [])

    initAssignedNums =
      Map.fromList $
        zip (map Right intMRegs) [0..] ++ zip (map Right ssaMRegs) [0..]

    nums :: Map.Map Var Int
    nums = fst $ head
               $ filter (null . snd)
               $ iterate colorNextNode (initAssignedNums, coloringOrder)

    ret = Map.mapWithKey
            (\k v -> if isIntVar k && v < 7 then Just $ intMRegs !! v
                    else if isSsaVar k && v < 8 then Just $ ssaMRegs !! v
                    else Nothing)
          $ Map.filterWithKey (\k _ -> isLeft k) nums

    getLeft :: Either a b -> a
    getLeft (Left x) = x
    getLeft _ = undefined

    ret' = Map.mapKeys getLeft ret
