module Main where

import Examples
import Data.Text.DrawADT
import Control.DeepSeq
import Control.Exception (evaluate)

main :: IO ()
main = do
  evaluate (rnf (toText (draw (LeafBinTree (testBinTree 20)))))
  return ()
