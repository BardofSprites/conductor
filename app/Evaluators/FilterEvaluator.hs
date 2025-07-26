module FilterEvaluator where

import qualified Commands.Filter as F
import Syntax
import Song

import Data.List (sortBy)
import Data.Ord (comparing)
import Data.Maybe

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
extractFieldValue field song =
  case field of
    Artist   -> StringVal (artist song)
    Title    -> StringVal (title song)
    Album    -> StringVal (album song)
    Genre    -> StringVal (genre song)
    Year     -> NumberVal (fromIntegral $ year song)
    Duration -> NumberVal (fromIntegral $ duration song)

evalComparison :: Comparison -> Song -> Bool
evalComparison (Comparison field op val) song =
  let songVal = extractFieldValue field song
  in compareValues op songVal val

applySingleFilter :: Comparison -> [Song] -> [Song]
applySingleFilter comp songs =
  filter (\song -> evalComparison comp song) songs

-- testFunc :: IO ()
-- testFunc = do
--   song1 <- readSong "/home/bard/Music/Phonk/KSLV Noh - Chase.mp3"
--   song2 <- readSong "/home/bard/Music/HipHop/MINMI - 四季ノ唄.mp3"
--   song3 <- readSong "/home/bard/Music/EDM/Daft Punk - Around the World.mp3"

--   let maybeSongs = [song1, song2, song3]
--       mySongs = catMaybes maybeSongs

--       -- Here's a test filter: genre == "Hip Hop"
--       testComparison = Comparison Genre Eq (StringVal "J-Pop")
--       filteredSongs = applySingleFilter testComparison mySongs

--   -- putStrLn "Filtered songs (genre == Hip Hop):"
--   mapM_ print filteredSongs
