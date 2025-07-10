module Limit where

import qualified Syntax as S

data LimitExpr = LimitExpr
  { lengthOption :: LengthOption
  , limitValue :: Int
  }

data LengthOption
  = ByDuration    -- limit playlist length duration of track
  | ByCount       -- limit number of tracks in playlist
  | ByField C.Field -- limit by number of field (artists)

