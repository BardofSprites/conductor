module Main where

import System.Environment (getArgs)
import Data.Maybe (catMaybes)
import Song (readSong)

-- Entry point
main :: IO ()
main = do
  args <- getArgs
  songs <- catMaybes <$> mapM readSong args
  mapM_ print songs
