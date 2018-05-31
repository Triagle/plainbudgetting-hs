module Main where

import Lib
import Text.Parsec
import Data.Either

processBudget :: String -> String
processBudget = either show (printBudget . runBudget) . parse (readLine `sepBy` newline :: Parsec String () [BudgetLine Double]) ""

main :: IO ()
main = interact processBudget
