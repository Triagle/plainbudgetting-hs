{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
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
import Data.Monoid ((<>))
import Data.Foldable (foldrM)
import qualified Control.Monad.Trans.State as ST
import qualified Control.Monad.Fail as MF
import Data.Functor (($>))
import Text.Parsec.Char
import Data.Csv
import Data.Either (either)
import qualified Data.Text.Lazy as T
import Data.Text.Lazy.Read (signed, rational)

data Flow a = Flow {
    flowName :: T.Text,
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

eol :: Parsec T.Text () ()
eol = void (char '\n') <|> eof

whitespace = oneOf " \t"

readFlow :: Char -> T.Text -> T.Text -> Either String (Flow Double)
readFlow sign val name =
  Flow name . fst <$> signed rational (T.singleton sign <> val)

eitherFail :: MF.MonadFail m => Either String b -> m b
eitherFail = either MF.fail return

lineCount :: Parsec T.Text () Double
lineCount =
  whitespace
    *> char 'x'
    *> whitespace
    *> (many1 digit >>= eitherFail . fmap fst . signed rational . T.pack)

flow :: Parsec T.Text () (Flow Double)
flow = do
  sgn <- oneOf "+-"
  void whitespace
  val <- T.pack <$> many digit
  void whitespace
  name <- T.pack <$> manyTill anyChar (try . lookAhead $ void lineCount <|> eol)
  eitherFail $ readFlow sgn val name

lineItem :: Parsec T.Text () (Flow Double)
lineItem = do
  fl    <- flow
  count <- lineCount <|> return 1
  return $ (* count) <$> fl

data BudgetLine a = LineItem (Flow a) | Comment T.Text | Sum a deriving (Show, Eq)

instance FromField a => FromRecord (BudgetLine a) where
    parseRecord v
        | length v < 1 = mzero
        | otherwise = (v .! 0 :: Parser T.Text) >>= (\case
            "Flow" -> LineItem <$> (Flow <$> v .! 1 <*> v .! 2)
            "Comment" -> Comment <$> v .! 1
            "Sum" -> Sum <$> v .! 1
            _ -> mzero)

instance ToField a => ToRecord (BudgetLine a) where
    toRecord (Sum v) = record [toField ("Sum" :: T.Text), toField v]
    toRecord (LineItem f) = record [toField ("Flow" :: T.Text), (toField . flowName) f, (toField . flowValue) f]
    toRecord (Comment s) = record [toField ("Comment" :: T.Text), toField s]

sectionSum :: Num a => Parsec T.Text () (BudgetLine a)
sectionSum = char '=' $> Sum 0 <* manyTill (whitespace <|> digit) (try . lookAhead $ eol)

comment :: Parsec T.Text () (BudgetLine a)
comment = Comment <$> (T.pack <$> manyTill anyChar (try . lookAhead $ eol))

budgetLine :: Parsec T.Text () (BudgetLine Double)
budgetLine = LineItem <$> lineItem

readLine :: Parsec T.Text () (BudgetLine Double)
readLine = try budgetLine <|> try sectionSum <|> comment

runBudgetLine :: Num a => BudgetLine a -> ST.State [Flow a] (BudgetLine a)
runBudgetLine (Comment "") = ST.put [] >> return (Comment "")
runBudgetLine (Comment  s) = return $ Comment s
runBudgetLine (LineItem f) = ST.modify (\bs -> f : bs) >> return (LineItem f)
runBudgetLine (Sum      v) = Sum . sum . map flowValue <$> ST.get 

runBudget :: (Floating a, Show a) => [BudgetLine a] -> [BudgetLine a]
runBudget lines = ST.evalState (mapM runBudgetLine lines) []

printBudget :: (Ord a, Floating a, Show a) => [BudgetLine a] -> T.Text
printBudget = T.unlines . map printLine
 where
  printLine (Comment s) = s
  printLine (LineItem (Flow name val)) =
    sign val <> " " <> (T.pack . show . abs) val <> " " <> name
  printLine (Sum v) = "= " <> (T.pack . show) v
  sign v | v >= 0    = "+"
         | otherwise = "-"
