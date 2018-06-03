{-# LANGUAGE OverloadedStrings #-}
module Main where

import Lib
import qualified Text.Parsec as P
import qualified Text.Parsec.Error as E
import qualified Data.Text.Lazy as T
import Data.Text.Lazy.Encoding (decodeUtf8, encodeUtf8)
import qualified Data.Text.Lazy.IO as TIO
import Data.List as L
import Data.Char (toLower)
import Data.Bifunctor
import Data.Either
import qualified Data.Vector as V
import Data.Semigroup ((<>))
import Options.Applicative
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

parseBudget :: T.Text -> Either P.ParseError [BudgetLine Double]
parseBudget = P.parse (readLine `P.sepBy` P.newline) ""

processBudget :: FormatType -> FormatType -> T.Text -> T.Text
processBudget i o = either (T.pack . show) (outputFn o . runBudget)
  . formatFn i
 where
  formatFn CSV = fmap V.toList . C.decode C.NoHeader . encodeUtf8
  formatFn Plain =
    first (mconcat . map ((<>) "\n" . E.messageString) . E.errorMessages)
      . parseBudget
  outputFn CSV   = decodeUtf8 . C.encode
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
  TIO.interact $ processBudget (input opts) (output opts)
