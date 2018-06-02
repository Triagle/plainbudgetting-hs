module Main where

import Lib
import qualified Text.Parsec as P
import qualified Text.Parsec.Error as E
import Data.List as L
import Data.Char (toLower)
import Data.Bifunctor
import Data.Either
import qualified Data.Vector as V
import Data.Semigroup ((<>))
import Options.Applicative
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Csv as C

data FormatType = CSV | Plain deriving (Show)

instance Read FormatType where
    readsPrec _ str 
        | "csv" `L.isPrefixOf` map toLower str = [(CSV, drop 3 str)]
        | "plain" `L.isPrefixOf` map toLower str = [(Plain, drop 5 str)]
        | otherwise = []

data Options = Options {
    input :: FormatType,
    output :: FormatType
}

parseBudget :: String -> Either P.ParseError [BudgetLine Double]
parseBudget = P.parse (readLine `P.sepBy` P.newline) ""

processBudget :: FormatType -> FormatType -> String -> String
processBudget i o = either show (outputFn o . runBudget) . formatFn i
 where
  formatFn CSV = (>>= return . V.toList) . C.decode C.NoHeader . B.pack
  formatFn Plain =
    first (mconcat . map ((++) "\n" . E.messageString) . E.errorMessages)
      . parseBudget
  outputFn CSV       = B.unpack . C.encode
  outputFn Plain = printBudget

opts :: Parser Options
opts =
  Options
    <$> option
          auto
          (  long "input"
          <> short 'i'
          <> help "Set input type"
          <> value Plain
          <> showDefault
          <> metavar "TYPE"
          )
    <*> option
          auto
          (  long "output"
          <> short 'o'
          <> help "Set output type"
          <> value Plain
          <> showDefault
          <> metavar "TYPE"
          )

pinfo :: ParserInfo Options
pinfo = info
  (opts <**> helper)
  (  fullDesc
  <> progDesc "Parse and manage a budget through plain text."
  <> header "plainbudgetting - a budgetting system in plain text"
  )

main :: IO ()
main = do
  opts <- execParser pinfo
  interact $ processBudget (input opts) (output opts)
