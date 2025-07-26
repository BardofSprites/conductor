module SortEvaluator where

import Commands.Sort
import Syntax
import Song

import Data.List (sortBy)
import Data.Ord (comparing)
import Data.Maybe

applySort :: SortExpr -> [Song] -> [Song]
applySort (SortExpr field order) songs =
  case field of
    Artist   -> sortBy (compareField order artist) songs
    Title    -> sortBy (compareField order title) songs
    Album    -> sortBy (compareField order album) songs
    Genre    -> sortBy (compareField order genre) songs
    Year     -> sortBy (compareField order year) songs
    Duration -> sortBy (compareField order duration) songs

compareField :: Ord b => SortOrder -> (Song -> b) -> Song -> Song -> Ordering
compareField Asc  f = comparing f
compareField Desc f = flip (comparing f)

testFunc :: IO ()
testFunc = do
  song1 <- readSong "/home/bard/Music/Phonk/KSLV Noh - Chase.mp3"
  song2 <- readSong "/home/bard/Music/HipHop/MINMI - 四季ノ唄.mp3"
  song3 <- readSong "/home/bard/Music/EDM/Daft Punk - Around the World.mp3"

  let maybeSongs = [song1, song2, song3]
      mySongs = catMaybes maybeSongs
      sortExpression = SortExpr Artist Asc
      sortedSongs = applySort sortExpression mySongs

  mapM_ (putStrLn . artist) sortedSongs
