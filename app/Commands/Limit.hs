module Commands.Limit where

import qualified Syntax as C
import qualified Parameters as P
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char (string)

data LimitExpr = LimitExpr
  { limitCount    :: Maybe Int
  , limitDuration :: Maybe Int
  , limitUnique   :: Maybe C.Field
  } deriving (Show, Eq)

type Parser = Parsec Void String

paramsToLimitExpr :: [(String, C.Value)] -> Either String LimitExpr
paramsToLimitExpr params = do
  let countVal    = lookup "count" params
      durationVal = lookup "duration" params
      uniqueVal   = lookup "unique" params

  limitCount <- case countVal of
    Just (C.NumberVal n) -> Right (Just n)
    Just _               -> Left "Expected number for count"
    Nothing              -> Right Nothing

  limitDuration <- case durationVal of
    Just (C.NumberVal n) -> Right (Just n)
    Just _               -> Left "Expected number for duration"
    Nothing              -> Right Nothing

  limitUnique <- case uniqueVal of
    Just (C.StringVal s) ->
      case fieldFromString s of
        Just f  -> Right (Just f)
        Nothing -> Left $ "Invalid field in unique: " ++ s
    Just _  -> Left "Expected string for unique"
    Nothing -> Right Nothing

  return $ LimitExpr limitCount limitDuration limitUnique

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
