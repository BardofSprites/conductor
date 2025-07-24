module Command where

import qualified Syntax as C
import qualified Commands.Filter as F
import qualified Commands.Sort as S
import qualified Commands.Limit as L
import qualified Parameters as P

import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void

type Parser = Parsec Void String

data Command
  = FilterCmd C.Comparison
  | SortCmd S.SortExpr
  | LimitCmd L.LimitExpr
  deriving (Show, Eq)

data CommandExpr
  = CommandLeaf Command
  | CommandAnd CommandExpr CommandExpr
  | CommandOr CommandExpr CommandExpr
  deriving (Show, Eq)

filterCommandParser :: Parser CommandExpr
filterCommandParser = do
  _ <- string "filter"
  space
  comp <- between (char '(') (char ')') C.comparisonParser
  case C.validateComparison comp of
    Right c -> return $ CommandLeaf (FilterCmd c)
    Left err -> fail err

sortCommandParser :: Parser CommandExpr
sortCommandParser = do
  _ <- string "sort"
  params <- P.paramsParser
  case S.paramsToSortExpr params of
    Right expr -> return $ CommandLeaf (SortCmd expr)
    Left err -> fail err

limitCommandParser :: Parser CommandExpr
limitCommandParser = do
  _ <- string "limit"
  params <- P.paramsParser
  case L.paramsToLimitExpr params of
    Right expr -> return $ CommandLeaf (LimitCmd expr)
    Left err -> fail err

commandAtomParser :: Parser CommandExpr
commandAtomParser = try filterCommandParser
                <|> try sortCommandParser
                <|> limitCommandParser
                <?> "command"

commandAndParser :: Parser CommandExpr
commandAndParser = do
  left <- commandAtomParser
  rest <- many $ try $ do      -- <--- try added here
    space
    _ <- string "&&"
    space
    right <- commandAtomParser
    return right
  return $ foldl CommandAnd left rest


commandExprParser :: Parser CommandExpr
commandExprParser = do
  left <- commandAndParser
  rest <- many $ try $ do      -- <--- try added here
    space
    _ <- string "||"
    space
    right <- commandAndParser
    return right
  return $ foldl CommandOr left rest
