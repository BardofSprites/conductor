module Sort where

import qualified Syntax as S

data SortExpr = SortExpr
  { sortField :: C.Field
  , sortOrder :: SortOrder
  }

data SortOrder = Asc | Desc deriving (Show, Eq)

