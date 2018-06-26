module Parser (line, document, AST (..)) where
import ClassyPrelude as C
import Control.Monad.Fail as MF
import Data.Text as T
import Data.Text.Read
import Text.Parsec as P
import Text.Parsec.Text

data AST a = Flow Text a
    | Sum a
    | Store Text
    | Retrieve Text
    | EmptyLine
    | Comment Text deriving (Show, Eq)

whitespace :: Parser ()
whitespace = void . P.many $ oneOf " \t"

litText :: String -> Parser Text
litText = fmap T.pack . string

digits :: Parser Text
digits = T.pack <$> P.many digit

sign :: Parser Char
sign = oneOf "+-"

unsignedNum :: Parser Text
unsignedNum = digits <> option "" (litText "." <> digits)

doubleM :: MonadFail m => Text -> m Double
doubleM = fmap fst . asFail . double
 where
  asFail (Left  v) = MF.fail v
  asFail (Right v) = return v

unsignedDouble :: Parser Double
unsignedDouble = unsignedNum >>= doubleM

applySign :: Num a => Maybe Char -> a -> a
applySign (Just '-') = ((-1) *)
applySign _          = id

signedDouble :: Parser Double
signedDouble = applySign <$> optionMaybe sign <*> unsignedDouble

text :: Parser Text
text = T.pack <$> P.many (noneOf "\n")

varId :: Parser Text
varId = T.pack <$> P.many1 (letter C.<|> alphaNum)

flow :: Parser (AST Double)
flow = do
  sgn <- sign
  whitespace
  val <- unsignedDouble
  whitespace
  txt <- text
  let signedVal = applySign (Just sgn) val in return (Flow txt signedVal)


flowSum :: Parser (AST Double)
flowSum = Sum <$> (char '=' >> whitespace >> option 0 unsignedDouble)

store :: Parser (AST Double)
store = Store <$> (string ">>" >> whitespace >> varId)

retrieve :: Parser (AST Double)
retrieve = Retrieve <$> (string "<<" >> whitespace >> varId)

textLine :: Parser (AST Double)
textLine = lineDeterminant <$> text
 where
  lineDeterminant "" = EmptyLine
  lineDeterminant t  = Comment t

expr :: Parser (AST Double)
expr = choice [flow, store, retrieve, flowSum]

line :: Parser (AST Double)
line = (P.try expr P.<|> P.try textLine) <* text

document :: Parser [AST Double]
document = line `sepBy` newline
