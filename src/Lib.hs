module Lib (evalAST, emptyState, printAST) where

import Parser
import ClassyPrelude
import qualified Control.Monad.Trans.State as ST

data BudgetState a = BudgetState {
    flowStack :: [AST a],
    varMap :: Map Text a,
    lineCount :: Integer
} deriving Show

emptyState :: BudgetState Double
emptyState = BudgetState {flowStack = [], varMap = mempty, lineCount = 0}

flowValue :: AST Double -> Double
flowValue (Flow _ v) = v
flowValue (Sum v) = v
flowValue _ = 0

stackValue :: BudgetState Double -> Double
stackValue = sum . map flowValue . flowStack


insertVar :: Text -> ST.State (BudgetState Double) ()
insertVar k = ST.modify go
    where go bs = bs {varMap = insertMap k (stackValue bs) (varMap bs)} 

getVar :: Text -> ST.State (BudgetState Double) ()
getVar k = ST.modify go
    where go bs = bs { flowStack = varVal bs:flowStack bs }
          varVal = Flow "" . fromMaybe 0 . lookup k . varMap

evalAST :: AST Double -> ST.State (BudgetState Double) (AST Double)
evalAST EmptyLine = do
    st <- ST.get
    if lineCount st == 1 then
        ST.put (st {lineCount = 0, flowStack = []})
    else
        ST.put (st {lineCount = lineCount st + 1})
    return EmptyLine
evalAST (Sum a) = Sum . stackValue <$> ST.get
evalAST (Store var) = insertVar var >> return (Store var)
evalAST (Retrieve var) = getVar var >> return (Retrieve var)
evalAST (Flow t v) = ST.modify (\bs -> bs {flowStack = Flow t v:flowStack bs}) >> return (Flow t v)
evalAST a = return a

printAST :: AST Double -> Text
printAST (Comment c) = c
printAST EmptyLine = ""
printAST (Store var) = ">> " <> var
printAST (Retrieve var) = "<< " <> var
printAST (Sum a) = "= " <> tshow a
printAST (Flow t value) = sign value <> " " <> (tshow . abs) value <> " " <> t
    where sign v
            | v < 0 = "-"
            | otherwise = "+"
