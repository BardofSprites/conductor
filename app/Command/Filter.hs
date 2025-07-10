module Filter where

import qualified Syntax as S

-- the entire expression
-- example: filter(artist == "Artist Name") OR filter(genre == "Hip Hop")
--          |_         FilterExp         _| |  |_      FilterExp       _|
--                                          |
--                                      comparison
data FilterExpr
 = Filter Comparison
 | And FilterExpr FilterExpr
 | Or FilterExpr FilterExpr
 | Parens FilterExpr
 deriving (Show, Eq)

