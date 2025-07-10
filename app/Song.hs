module Song where

import qualified Sound.TagLib as TagLib
import Control.Applicative (liftA2)

-- Data model for structured song info
data Song = Song
  { path     :: FilePath
  , title    :: String
  , artist   :: String
  , album    :: String
  , genre    :: String
  , year     :: Integer
  , duration :: Integer
  } deriving (Show, Eq)

-- Build a Maybe Song from a file
readSong :: FilePath -> IO (Maybe Song)
readSong filepath = do
  mtagFile <- TagLib.open filepath
  case mtagFile of
    Nothing -> return Nothing
    Just tagFile -> do
      mtags <- TagLib.tag tagFile
      maudio <- TagLib.audioProperties tagFile
      case liftA2 (,) mtags maudio of
        Nothing -> return Nothing
        Just (tag, audio) -> Just <$> buildSong filepath tag audio

-- Extract metadata and build a Song value
buildSong :: FilePath -> TagLib.Tag -> TagLib.AudioProperties -> IO Song
buildSong filepath tag audio = Song filepath
  <$> TagLib.title tag
  <*> TagLib.artist tag
  <*> TagLib.album tag
  <*> TagLib.genre tag
  <*> TagLib.year tag
  <*> TagLib.duration audio
