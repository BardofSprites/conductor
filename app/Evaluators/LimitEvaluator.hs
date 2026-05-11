module Evaluators.LimitEvaluator where

import Commands.Limit
import Syntax
import Song
import qualified Data.Set as Set

applyLimit :: LimitExpr -> [Song] -> [Song]
applyLimit (LimitExpr mCount mDuration mUnique) =
  applyCount mCount . applyDuration mDuration . applyUnique mUnique

applyCount :: Maybe Int -> [Song] -> [Song]
applyCount Nothing  = id
applyCount (Just n) = take n

applyDuration :: Maybe Int -> [Song] -> [Song]
applyDuration Nothing = id
applyDuration (Just limitTime) = takeWhileAcc 0
  where
    takeWhileAcc _ [] = []
    takeWhileAcc acc (s:ss)
      | acc + fromIntegral (duration s) <= limitTime = s : takeWhileAcc (acc + fromIntegral (duration s)) ss
      | otherwise = []

applyUnique :: Maybe Field -> [Song] -> [Song]
applyUnique Nothing = id
applyUnique (Just fld) = go Set.empty
  where
    go _ [] = []
    go seen (s:ss) =
      let key = extractFieldValue fld s
      in if key `Set.member` seen
         then go seen ss
         else s : go (Set.insert key seen) ss

extractFieldValue :: Field -> Song -> String
extractFieldValue Artist   = artist
extractFieldValue Title    = title
extractFieldValue Album    = album
extractFieldValue Genre    = genre
extractFieldValue Year     = show . year
extractFieldValue Duration = show . duration
