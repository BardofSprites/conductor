module Files where

import System.FilePath
import System.Directory
import Control.Monad

getAllFiles :: FilePath -> [FilePath] -> IO [FilePath]
getAllFiles baseDir excludedDirs = do
  entries <- listDirectory baseDir

  let fullEntries = map (baseDir </>) entries
  files <- filterM doesFileExist fullEntries
  dirs <- filterM doesDirectoryExist fullEntries
  let dirsToSearch = filter (\dir -> takeFileName dir `notElem` excludedDirs) dirs
  nestedFiles <- forM dirsToSearch $ \dir -> getAllFiles dir excludedDirs
  return (files ++ concat nestedFiles)
