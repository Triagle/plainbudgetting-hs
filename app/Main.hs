module Main where
import ClassyPrelude
import Lib
import Parser
import Text.Parsec
import Control.Monad.Trans.State (evalState)

data FormatType = CSV | Plain deriving Show

parseFormat :: LText -> Maybe FormatType
parseFormat v
    | lv == "csv" = return CSV
    | lv == "plain" = return Plain
    | otherwise = empty
    where lv = toLower v

showBudget :: [AST Double] -> Text
showBudget = unlines . map printAST

runBudget :: [AST Double] -> [AST Double]
runBudget = flip evalState emptyState . mapM evalAST

plBudget :: LText -> LText
plBudget = fromStrict . either tshow showBudget . fmap runBudget . parseBudget . toStrict
    where parseBudget = parse document ""
        
main :: IO ()
main = do
    ln - getContents
    (print . fmap runBudget . parse document "" . toStrict) ln
