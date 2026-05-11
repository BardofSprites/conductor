module Main where

import System.Environment (getArgs)
import Data.Maybe (catMaybes)
import Song (readSong)
import Song
import Syntax
import Commands.Limit
import Commands.Sort
import Files

import qualified Evaluators.LimitEvaluator as L
import qualified Evaluators.SortEvaluator as S

-- Entry point
main :: IO ()
main = do
  args <- getArgs
  songs <- catMaybes <$> mapM readSong args
  mapM_ print songs
