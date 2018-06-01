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
import Control.Monad (void, msum, join, mzero)
import Data.Foldable (foldrM)
import qualified Control.Monad.Trans.State as ST
import Data.Functor (($>))
import Text.Parsec.Char
import Data.Csv

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

instance FromField a => FromRecord (BudgetLine a) where
    parseRecord v 
        | length v < 1 = mzero
        | otherwise = v .! 0 >>= (\name -> case name of
            "Flow" -> LineItem <$> (Flow <$> v .! 1 <*> v .! 2)
            "Comment" -> Comment <$> v .! 1
            "Sum" -> Sum <$> v .! 1
            _ -> mzero)

instance ToField a => ToRecord (BudgetLine a) where
    toRecord (Sum v) = record [toField "Sum", toField v]
    toRecord (LineItem f) = record [toField "Flow", (toField . flowName) f, (toField . flowValue) f]
    toRecord (Comment s) = record [toField "Comment", toField s]

sectionSum :: Num a => Parsec String () (BudgetLine a)
sectionSum = char '=' $> Sum 0 <* optional (spaces *> manyTill anyChar (try . lookAhead $ eol))

comment :: Parsec String () (BudgetLine a)
comment = Comment <$> manyTill anyChar (try . lookAhead $ eol)

budgetLine :: (Floating a, Read a) => Parsec String () (BudgetLine a)
budgetLine = LineItem <$> lineItem

readLine :: (Floating a, Read a) => Parsec String () (BudgetLine a)
readLine = try budgetLine <|> sectionSum <|> comment

runBudgetLine :: Floating a => BudgetLine a -> ST.State [Flow a] (BudgetLine a)
runBudgetLine (Comment s) = return $ Comment s
runBudgetLine (LineItem f) = ST.modify (\bs -> f:bs) >> return (LineItem f)
runBudgetLine (Sum v) = do 
    stck <- ST.get
    ST.put []
    return . Sum . sum . map flowValue $ stck

runBudget :: Floating a => [BudgetLine a] -> [BudgetLine a]
runBudget lines = ST.evalState (mapM runBudgetLine lines) []
  
printBudget :: (Ord a, Floating a, Show a) => [BudgetLine a] -> String
printBudget = unlines . map printLine
 where
  printLine (Comment s) = s
  printLine (LineItem (Flow name val)) =
    sign val ++ " " ++ (show . abs) val ++ " " ++ name
  printLine (Sum v) = "= " ++ show v
  sign v | v >= 0    = "+"
         | otherwise = "-"
