module Query where

import qualified Commands.Filter as F
import qualified Commands.Sort as S
import qualified Commands.Limit as L

import qualified Syntax as C

-- Top level structure
data Query
 = QFilter F.FilterExpr
 | QSort S.SortExpre
 | QLimit L.LimitExpr
 | QLength LengthOption
 | QGroup Field

-- implement query parser using filterexpr and sortexpr parsers
