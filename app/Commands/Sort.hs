module Commands.Sort where

import qualified Syntax as C
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char (char, string, space, letterChar)
import qualified Parameters as P

data SortExpr = SortExpr
  { sortField :: C.Field
  , sortOrder :: SortOrder
  } deriving (Show, Eq)

data SortOrder = Asc | Desc deriving (Show, Eq)

type Parser = Parsec Void String

paramsToSortExpr :: [(String, C.Value)] -> Either String SortExpr
paramsToSortExpr params = do
  orderVal <- P.lookupParam "order" params
  fieldVal <- P.lookupParam "field" params

  sortOrder <- case orderVal of
    C.StringVal "asc"  -> Right Asc
    C.StringVal "desc" -> Right Desc
    _ -> Left $ "Invalid sort order: " ++ show orderVal

  sortField <- case fieldVal of
    C.StringVal "artist"   -> Right C.Artist
    C.StringVal "title"    -> Right C.Title
    C.StringVal "album"    -> Right C.Album
    C.StringVal "genre"    -> Right C.Genre
    C.StringVal "year"     -> Right C.Year
    C.StringVal "duration" -> Right C.Duration
    _ -> Left $ "Invalid sort field: " ++ show fieldVal

  return $ SortExpr sortField sortOrder

sortExprParser :: Parser (Either String SortExpr)
sortExprParser = do
  _ <- string "sort"
  params <- P.paramsParser
  return $ paramsToSortExpr params
