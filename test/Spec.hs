module Main where

import Examples
import Data.Text.DrawADT

main :: IO ()
main = printDraw $ draw testRoseTree
