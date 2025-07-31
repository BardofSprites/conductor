module LimitEvaluator where

import Commands.Limit
import Syntax
import Song
import Data.Maybe (catMaybes)

-- applyLimit :: LimitExpr -> [Song] -> [Song]
-- applyLimit (LimitExpr mCount mDuration mUnique) =
--   applyCount mCount . applyDuration mDuration . applyUnique mUnique

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

applyUnique :: Nothing = id

-- testFunc :: IO ()
-- testFunc = do
--   song1 <- readSong "/home/bard/Music/Phonk/KSLV Noh - Chase.mp3"
--   song2 <- readSong "/home/bard/Music/HipHop/MINMI - 四季ノ唄.mp3"
--   song3 <- readSong "/home/bard/Music/EDM/Daft Punk - Around the World.mp3"

--   let maybeSongs = [song1, song2, song3]
--       mySongs = catMaybes maybeSongs
--       myLimitedSongs = applyDuration (Just 425) mySongs

--   mapM_ print maybeSongs
--   mapM_ printFormatted myLimitedSongs

-- printFormatted song = putStrLn (artist song ++ " - " ++ title song ++ " (" ++ show (duration song) ++ ")")
  
