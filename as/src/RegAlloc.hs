{-# LANGUAGE TemplateHaskell #-}

module RegAlloc where

import Control.Lens ((^.))
import Control.Lens.TH (makeLenses)
import Debug.Pretty.Simple (pTrace)

import InstrSelect (BasicBlockId, FuncBody, FuncName, Instr(..), InstrSelectState, MachineReg, Operand(..), RegIdx)

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
            u = getUse instr
            d = getDef instr
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
            pTrace ("defMap = " ++ show (raState ^. defMap))
                   Map.empty -- TODO

getUse :: Instr -> [Var]
getUse _ = [] -- TODO

getDef :: Instr -> [Var]
getDef _ = [] -- TODO
