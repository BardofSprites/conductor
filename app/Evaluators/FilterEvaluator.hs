module Evaluators.FilterEvaluator where

import Syntax
import Song

compareValues :: Operator -> Value -> Value -> Bool
compareValues Eq (StringVal s1) (StringVal s2) = s1 == s2
compareValues Eq (NumberVal n1) (NumberVal n2) = n1 == n2
compareValues Neq v1 v2 = not (compareValues Eq v1 v2)

compareValues Gt (NumberVal n1) (NumberVal n2) = n1 > n2
compareValues Lt (NumberVal n1) (NumberVal n2) = n1 < n2
compareValues Gte (NumberVal n1) (NumberVal n2) = n1 >= n2
compareValues Lte (NumberVal n1) (NumberVal n2) = n1 <= n2
-- fallback if types don't match or op isn't supported
compareValues _ _ _ = False

extractFieldValue :: Field -> Song -> Value
extractFieldValue fld song =
  case fld of
    Artist   -> StringVal (artist song)
    Title    -> StringVal (title song)
    Album    -> StringVal (album song)
    Genre    -> StringVal (genre song)
    Year     -> NumberVal (fromIntegral $ year song)
    Duration -> NumberVal (fromIntegral $ duration song)

evalComparison :: Comparison -> Song -> Bool
evalComparison (Comparison fld op val) song =
  let songVal = extractFieldValue fld song
  in compareValues op songVal val

applySingleFilter :: Comparison -> [Song] -> [Song]
applySingleFilter comp songs =
  filter (\song -> evalComparison comp song) songs
