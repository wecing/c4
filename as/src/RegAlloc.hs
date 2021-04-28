{-# LANGUAGE TemplateHaskell #-}

module RegAlloc where

import Control.Lens ((^.))
import Control.Lens.TH (makeLenses)
import Data.Map ((!))
import Debug.Pretty.Simple (pTrace)

import InstrSelect (BasicBlockId, FuncBody, FuncName, Instr(..), InstrSelectState, MachineReg(..), Operand(..), RegIdx)

import qualified Data.Map as Map
import qualified Data.ProtoLens.Runtime.Data.Text as Text

import qualified Proto.Ir as IR

type InstrLabel = (String, Int)  -- (asm label, instr idx)

type Var = Either RegIdx MachineReg

data RegAllocState = RegAllocState
  { _iSelState :: InstrSelectState
  , _useMap :: Map.Map InstrLabel [Var]
  , _defMap :: Map.Map InstrLabel [Var]
  , _succMap :: Map.Map InstrLabel [InstrLabel]
  } deriving (Show)

makeLenses ''RegAllocState

-- https://www.cs.cmu.edu/~fp/courses/15411-f14/lectures/04-liveness.pdf
run :: IR.IrModule
    -> Map.Map FuncName (InstrSelectState, FuncBody)
    -> Map.Map FuncName FuncBody
run _ = Map.mapWithKey run'
  where
    run' :: FuncName -> (InstrSelectState, FuncBody) -> FuncBody
    run' funcName (instrSelectState, funcBody) = r -- TODO
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
        raState = RegAllocState
          { _iSelState = instrSelectState
          , _useMap = Map.map (\(x, _, _) -> x) udsMap
          , _defMap = Map.map (\(_, x, _) -> x) udsMap
          , _succMap = Map.map (\(_, _, x) -> x) udsMap
          }
        r = -- pTrace ("funcName = " ++ Text.unpack funcName) $
            -- pTrace ("funcBody = " ++ show funcBody) $
            -- pTrace ("raState = " ++ show raState)
            -- pTrace ("useMap = " ++ show (raState ^. useMap))
            -- pTrace ("defMap = " ++ show (raState ^. defMap))
            pTrace ("udsMap = " ++ show udsMap)
                   Map.empty -- TODO

getUD :: Instr -> ([Var], [Var])
getUD (Instr "and" _ [Imm _, Reg y]) = ([Left y], [Left y])
getUD (Instr "cmp" _ [Imm _, Reg y]) = ([Left y], [])
getUD (Instr "cmp" _ [Reg x, Reg y]) = ([Left x, Left y], [])
getUD (Instr "cmpneq" _ [Reg x, Reg y]) = ([Left x, Left y], [])
getUD (Instr "div" _ [Reg x]) = ([Right RDX, Right RAX], [Left x])
getUD (Instr "idiv" _ [Reg x]) = ([Right RDX, Right RAX], [Left x])
getUD (Instr "je" _ [Addr _ _]) = ([], [])
getUD (Instr "jmp" _ [Addr _ _]) = ([], [])
getUD (Instr "lea" _ [Addr _ _, Reg y]) = ([], [Left y])
getUD (Instr "lea" _ [StackParam _, MReg _ y]) = ([], [Right y])
getUD (Instr "mov" _ [Imm _, MReg _ y]) = ([], [Right y])
getUD (Instr "mov" _ [Imm _, Reg y]) = ([], [Left y])
getUD (Instr "mov" _ [MReg _ x, Reg y]) = ([Right x], [Left y])
getUD (Instr "mov" _ [Reg x, MReg _ y]) = ([Left x], [Right y])
getUD (Instr "mov" _ [Reg x, StackParam _]) = ([Left x], [])
getUD (Instr "not" _ [Reg x]) = ([Left x], [Left x])
getUD (Instr "repmovs" _ []) = ([Right RCX, Right RSI, Right RDI], [])
getUD (Instr "ret" _ []) = ([], map Right [RAX, RDX, XMM0, XMM1]) -- TODO
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
    us = map Right [ RAX, RDI, RSI, RDX, RCX, R8, R9
                   , XMM0, XMM1, XMM2, XMM3, XMM4, XMM5, XMM6, XMM7 ]
    ds = map Right [ RAX, RDX, XMM0, XMM1 ]
getUD instr = error $ "unexpected instr for getUD: " ++ show instr
