module Commands.Filter where

import qualified Syntax as C
import Text.Megaparsec.Char (char, string)

type Filter = C.Comparison

data FilterExpr
  = Single Filter
  | And FilterExpr FilterExpr
  | Or  FilterExpr FilterExpr
  deriving (Show, Eq)

filterParser :: C.Parser Filter
filterParser = do
  _ <- string "filter"
  _ <- char '('
  comp <- C.comparisonParser
  _ <- char ')'
  return comp  -- type Filter = Comparison
