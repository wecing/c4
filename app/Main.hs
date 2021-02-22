module Main where

import qualified Data.ByteString as BS
import Data.ProtoLens (parseMessage)
import Data.ProtoLens.Encoding.Parser (Parser, runParser)
import qualified InstrSelect
import Proto.Ir as IR
import Text.Pretty.Simple (pPrint)

main :: IO ()
main = do
  bs <- BS.getContents
  ir <- case runParser (parseMessage :: Parser IR.IrModule) bs of
    Left msg -> fail msg
    Right ir -> return ir
  pPrint $ InstrSelect.run ir