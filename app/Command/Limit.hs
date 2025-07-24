module Limit where

import qualified Syntax as C
import qualified Parameters as P
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char (string)

data LimitExpr = LimitExpr { lengthOption :: LengthOption }
  deriving (Show, Eq)

data LengthOption
  = ByDuration      -- limit playlist length by duration of tracks
  | ByCount         -- limit by number of tracks
  | ByField C.Field -- limit by number of a specific field (e.g. artist)
  deriving (Show, Eq)

type Parser = Parsec Void String

paramsToLimitExpr :: [(String, C.Value)] -> Either String LimitExpr
paramsToLimitExpr params = do
  optVal <- P.lookupParam "length" params
  lengthOpt <- case optVal of
    C.StringVal "duration" -> Right ByDuration
    C.StringVal "count"    -> Right ByCount
    C.StringVal fieldName  -> case fieldFromString fieldName of
      Just f  -> Right (ByField f)
      Nothing -> Left $ "Invalid field for length: " ++ fieldName
    _ -> Left $ "Invalid length value: " ++ show optVal
  return $ LimitExpr lengthOpt

-- Helper to parse string to C.Field
fieldFromString :: String -> Maybe C.Field
fieldFromString s = case s of
  "artist"   -> Just C.Artist
  "title"    -> Just C.Title
  "album"    -> Just C.Album
  "genre"    -> Just C.Genre
  "year"     -> Just C.Year
  "duration" -> Just C.Duration
  _          -> Nothing

limitExprParser :: Parser (Either String LimitExpr)
limitExprParser = do
  _ <- string "limit"
  params <- P.paramsParser
  return $ paramsToLimitExpr params
