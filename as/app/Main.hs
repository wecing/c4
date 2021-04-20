module Main where

import Data.ProtoLens (parseMessage)
import Data.ProtoLens.Encoding.Parser (Parser, runParser)
import Text.Pretty.Simple (pPrint)

import qualified Data.ByteString as BS

import qualified InstrSelect
import qualified Proto.Ir as IR
import qualified RegAlloc

main :: IO ()
main = do
  bs <- BS.getContents
  ir <- case runParser (parseMessage :: Parser IR.IrModule) bs of
    Left msg -> fail msg
    Right ir -> return ir
  pPrint ir
  pPrint $ RegAlloc.run ir $ InstrSelect.run ir