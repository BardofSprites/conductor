module Parameters where

import qualified Syntax as C

import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char (char, space, letterChar)

type Parser = Parsec Void String

lookupParam :: String -> [(String, C.Value)] -> Either String C.Value
lookupParam key pairs =
  case lookup key pairs of
    Just v  -> Right v
    Nothing -> Left $ "Missing parameter: " ++ key

paramParser :: Parser (String, C.Value)
paramParser = do
  key <- some letterChar
  _ <- char '='
  val <- C.stringValParser <|> C.numberValParser
  return (key, val)

paramsParser :: Parser [(String, C.Value)]
paramsParser = do
  _ <- char '('
  params <- paramParser `sepBy` (char ',' >> space)
  _ <- char ')'
  return params
