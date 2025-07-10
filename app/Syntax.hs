module Syntax where

import Text.Megaparsec
import Text.Megaparsec.Char (char, string, space)
import Data.Void
import Control.Applicative ((<|>))
import qualified Text.Megaparsec.Char.Lexer as L
 
-- the comparison part: artist == "Artist Name"
-- example:             field  op  value
data Comparison = Comparison
  { field    :: Field
  , operator :: Operator
  , value    :: Value
  } deriving (Show, Eq)

data Operator = Eq | Neq | Gt | Lt | Gte | Lte
  deriving (Show, Eq)

data Value = StringVal String | NumberVal Int
  deriving (Show, Eq)

data Field = Title | Artist | Album | Genre | Year | Duration
  deriving (Show, Eq)

data FieldType = StringType | NumberType deriving (Show, Eq)

fieldType :: Field -> FieldType
fieldType Artist   = StringType
fieldType Title    = StringType
fieldType Artist   = StringType 
fieldType Album    = StringType
fieldType Genre    = StringType
fieldType Year     = NumberType
fieldType Duration = NumberType

-- example: validOperation (fieldType Artist) Gte returns false
-- (you can't have greater than artist)
validOperation :: FieldType -> Operator -> Bool
validOperation StringType op = op == Eq
validOperator NumberType op = op `elem` [Eq, Neq, Gt, Lt, Gte, Lte]

-- example: valMatchesField NumberType (StringVal "Queen")
valMatchesField :: FieldType -> Value -> Bool
valMatchesField StringType (StringVal _) = True
valMatchesField NumberType (NumberVal _) = True
valMatchesField _ _ = False

validateComparison :: Comparison -> Either String Comparison
validateComparison comp =
  let ftype = fieldType (field comp) -- extract field out of comp struct, find type
      op = operator comp -- get the operator out of 
      val = value comp
  in if validOperation ftype op && valMatchesField ftype val
        then Right comp
        else Left $ "Invalid operator or value type for field: " ++ show (field comp)

----------------------
-- DEFINING PARSERS --
----------------------

type Parser = Parsec Void String

-- operators Eq, Neq, Gt, Lt, Gte, Lte
operatorParser :: Parser Operator
operatorParser =
  (string "==" >> return Eq)
  <|> (string "!=" >> return Neq)
  <|> (string ">"  >> return Gt)
  <|> (string "<"  >> return Lt)
  <|> (string ">=" >> return Gte )
  <|> (string "<=" >> return Lte)

--  fields
fieldParser =
  (string "artist" >> return Artist)
  <|> (string "title"    >> return Title) 
  <|> (string "artist"   >> return Artist) 
  <|> (string "album"    >> return Album) 
  <|> (string "genre"    >> return Genre) 
  <|> (string "year"     >> return Year) 
  <|> (string "duration" >> return Duration)
  <?> "field name"

-- string values; reads inside "" until closed
stringValParser :: Parser Value
stringValParser = do
  _ <- char '"'
  content <- manyTill L.charLiteral (char '"') -- read until closed
  return $ StringVal content

-- 
numberValParser :: Parser Value
numberValParser = NumberVal <$> L.decimal

valueParser :: Parser Value
valueParser = stringValParser <|> numberValParser  

comparisonParser :: Parser Comparison
comparisonParser = do
  field <- fieldParser
  space
  op <- operatorParser
  space
  val <- valueParser
  return $ Comparison field op val
