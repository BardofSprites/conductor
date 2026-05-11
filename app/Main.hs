module Main where

import System.Environment (getArgs)
import Data.Maybe (catMaybes)
import Song
import Syntax
import Commands.Limit
import Commands.Sort
import Files

import qualified Evaluators.LimitEvaluator as L
import qualified Evaluators.SortEvaluator as S

printFormatted :: Song -> IO ()
printFormatted song = putStrLn ("[" ++
                                (show . year) song ++
                                "] " ++
                                artist song ++
                                " - " ++
                                title song ++
                                " (" ++
                                (show . duration) song ++
                                ")")

main :: IO ()
main = do
  args <- getArgs
  case args of
    [dir] -> do
      filePaths <- getAllFiles dir ["git"]
      maybeSongs <- mapM readSong filePaths
      let songs = catMaybes maybeSongs
          limitExpression = LimitExpr Nothing (Just 2000) Nothing
          limitedSongs = L.applyLimit limitExpression songs
          sortExpression = SortExpr Year Asc
          sortedSongs = S.applySort sortExpression limitedSongs
      mapM_ printFormatted sortedSongs

    _ -> putStrLn "Usage: conductor <music-directory-path>"
