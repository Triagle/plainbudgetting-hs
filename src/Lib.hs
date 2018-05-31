module Lib
    (lineItem,
    Flow,
    flow,
    readLine,
    runBudget,
    printBudget,
    BudgetLine
    ) where

import Text.Parsec
import qualified Data.DList as DL
import Control.Monad (void, msum, join)
import Data.Foldable (foldrM)
import qualified Control.Monad.Trans.State as ST
import Data.Functor (($>))
import Text.Parsec.Char

data Flow a = Flow {
    flowName :: String,
    flowValue :: a
} deriving (Show, Eq, Ord)

instance Functor Flow where
    fmap f flow = Flow (flowName flow) (f . flowValue $ flow)

instance Applicative Flow where
    pure = Flow ""
    (<*>) a = fmap (flowValue a)

instance Monad Flow where
    return = pure
    (>>=) flow f = f (flowValue flow)

eol :: Parsec String () ()
eol = void (char '\n') <|> eof

whitespace = oneOf " \t"

readFlow :: Read a => Char -> String -> String -> Flow a
readFlow '+' val name = Flow name (read val)
readFlow _   val name = Flow name (read $ '-' : val)

lineCount :: (Read a, Integral a) => Parsec String () a
lineCount = whitespace *> char 'x' *> whitespace *> (read <$> many1 digit)

flow :: Read a => Parsec String () (Flow a)
flow = do
  sgn <- oneOf "+-"
  void whitespace
  val <- many digit
  void whitespace
  name <- manyTill anyChar (try . lookAhead $ void lineCount <|> eol)
  return $ readFlow sgn val name

lineItem :: (Floating a, Read a) => Parsec String () (Flow a)
lineItem = do
  fl    <- flow
  count <- fromIntegral <$> (lineCount <|> return 1)
  return $ (* count) <$> fl

data BudgetLine a = LineItem (Flow a) | Comment String | Sum a deriving (Show, Eq)

sectionSum :: Num a => Parsec String () (BudgetLine a)
sectionSum = char '=' $> Sum 0

comment :: Parsec String () (BudgetLine a)
comment = Comment <$> manyTill anyChar (try . lookAhead $ eol)

budgetLine :: (Floating a, Read a) => Parsec String () (BudgetLine a)
budgetLine = LineItem <$> lineItem

readLine :: (Floating a, Read a) => Parsec String () (BudgetLine a)
readLine = try budgetLine <|> sectionSum <|> comment

data BudgetState a = BudgetState {
    currentFlows :: [Flow a],
    output :: DL.DList (BudgetLine a)
}


flush :: Floating a => ST.State (BudgetState a) ()
flush = ST.modify
  (\bs -> bs
    { currentFlows = []
    , output       = output bs
      `DL.append` (DL.fromList . map LineItem . currentFlows) bs
    }
  )

runBudgetLine :: Floating a => BudgetLine a -> ST.State (BudgetState a) ()
runBudgetLine (Comment s) =
  ST.modify (\bs -> bs { output = output bs `DL.snoc` Comment s })

runBudgetLine (LineItem f) =
  ST.modify (\bs -> bs { currentFlows = f : currentFlows bs, output = output bs `DL.snoc` LineItem f})

runBudgetLine (Sum v) = ST.modify
  (\bs -> bs
    { currentFlows = []
    , output       = output bs
      `DL.append` (DL.singleton . Sum . sum . map flowValue . currentFlows $ bs)
    }
  )

runBudget :: Floating a => [BudgetLine a] -> [BudgetLine a]
runBudget lines = ST.evalState
  (do
    mapM_ runBudgetLine lines
    flush
    ss <- ST.get
    return . DL.toList . output $ ss
  )
  (BudgetState [] (DL.fromList []))

printBudget :: (Ord a, Floating a, Show a) => [BudgetLine a] -> String
printBudget = unlines . map printLine
 where
  printLine (Comment s) = s
  printLine (LineItem (Flow name val)) =
    sign val ++ " " ++ (show . abs) val ++ " " ++ name
  printLine (Sum v) = "= " ++ show v
  sign v | v >= 0    = "+"
         | otherwise = "-"
