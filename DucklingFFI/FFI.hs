module FFI
    ( someFunc
    ) where

import Data.Text (Text)
import Data.ByteString (ByteString, empty)
import Duckling.Core
-- import Duckling.Data.TimeZone
import Duckling.Resolve (DucklingTime)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text

import qualified Control.Exception as E
import Control.Monad.Extra
import Data.Either
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import Data.String
import Data.Time (TimeZone(..))
import Data.Time.LocalTime.TimeZone.Olson
import Data.Time.LocalTime.TimeZone.Series
import System.Directory
import System.FilePath

import Foreign.StablePtr
import Foreign.Ptr
import Foreign.C

import Prelude
import Curryrs.Types


newtype WrappedString = WrappedString { wrapString :: Foreign.C.CString }
newtype WrappedTimeZoneSeries = WrappedTimeZoneSeries { timeSeries :: IO (HashMap Text TimeZoneSeries) }

-- | Reference implementation for pulling TimeZoneSeries data from local
-- Olson files.
-- Many linux distros have Olson data in "/usr/share/zoneinfo/"
loadTimeZoneSeries :: FilePath -> IO (HashMap Text TimeZoneSeries)
loadTimeZoneSeries base = do
  files <- getFiles base
  tzSeries <- mapM parseOlsonFile files
  -- This data is large, will live a long time, and essentially be constant,
  -- so it's a perfect candidate for compact regions
  return $ HashMap.fromList $ rights tzSeries
  where
    -- Multiple versions of the data can exist. We intentionally ignore the
    -- posix and right formats
    ignored_dirs = HashSet.fromList $ map (base </>)
      [ "posix", "right" ]

    -- Recursively crawls a directory to list every file underneath it,
    -- ignoring certain directories as needed
    getFiles :: FilePath -> IO [FilePath]
    getFiles dir = do
      fsAll <- getDirectoryContents dir
      let
        fs = filter notDotFile fsAll
        full_fs = map (dir </>) fs
      (dirs, files) <- partitionM doesDirectoryExist full_fs

      subdirs <- concatMapM getFiles
        [ d | d <- dirs, not $ HashSet.member d ignored_dirs ]

      return $ files ++ subdirs

    -- Attempts to read a file in Olson format and returns its
    -- canonical name (file path relative to the base) and the data
    parseOlsonFile :: FilePath
                   -> IO (Either E.ErrorCall (Text, TimeZoneSeries))
    parseOlsonFile f = E.try $ do
      r <- getTimeZoneSeriesFromOlsonFile f
      return (Text.pack $ makeRelative base f, r)

    notDotFile s = not $ elem s [".", ".."]


someFunc :: IO ()
someFunc = putStrLn "someFunc"

-- parseRefTime :: Text -> DucklingTime
-- parseRefTime text = text
