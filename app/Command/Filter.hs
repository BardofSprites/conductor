module Filter where

import qualified Syntax as C
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char (char, string, space, letterChar)

type Filter = C.Comparison

data FilterExpr
  = Single Filter
  | And FilterExpr FilterExpr
  | Or  FilterExpr FilterExpr
  deriving (Show, Eq)

type Parser = Parsec Void String

filterParser :: Parser Filter
filterParser = do
  _ <- string "filter"
  _ <- char '('
  comp <- C.comparisonParser
  _ <- char ')'
  return comp  -- type Filter = Comparison
