module Main where

import Lib
import Text.Parsec
import Data.Either
import Data.Semigroup ((<>))
import Options.Applicative
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Csv as C

data FormatType = CSV | PlainText

data Options = Options {
    input :: FormatType,
    output :: FormatType
}

parseBudget :: String -> Either Text.Parsec.ParseError [BudgetLine Double]
parseBudget = parse (readLine `sepBy` newline) ""

processBudget :: FormatType -> FormatType -> String -> String
processBudget i o = either show (outputFn o . runBudget) . (formatFn i)
    where formatFn CSV = toList <$> C.decode C.NoHeader . B.pack
          formatFn PlainText = parseBudget
          outputFn CSV = B.unpack . C.encode
          outputFn PlainText = printBudget
 
optParse :: Parser Options
optParse = Option <$> flag PlainText CSV (long "csv" <> help "Set input type to csv") <*> flag PlainText CSV (long "output-csv" <> help "Set output type to CSV")

main :: IO ()
main = exec  interact processBudget
