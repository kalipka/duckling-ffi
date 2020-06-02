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
import Data.String.Conversions (cs)
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

-- String wrapper
newtype WrappedString = WrappedString { wrapString :: Foreign.C.CString }
foreign export ccall stringCreate :: CString -> IO (Ptr ())
foreign export ccall stringDestroy :: Ptr () -> IO ()
foreign export ccall stringGet :: Ptr () -> IO CString

stringCreate :: CString -> IO (Ptr ())
stringCreate x = castStablePtrToPtr <$> newStablePtr (WrappedString x)

stringDestroy :: Ptr () -> IO ()
stringDestroy p = freeStablePtr sp
  where sp :: StablePtr WrappedString
        sp = castPtrToStablePtr p

stringGet :: Ptr () -> IO CString
stringGet p = wrapString <$> deRefStablePtr (castPtrToStablePtr p)


-- TimeZoneSeries wrapper
newtype WrappedTimeZoneSeries = WrappedTimeZoneSeries { timeSeries :: HashMap Text TimeZoneSeries }
foreign export ccall tzdbDestroy :: Ptr () -> IO ()

tzdbCreate :: HashMap Text TimeZoneSeries -> IO(Ptr ())
tzdbCreate x = castStablePtrToPtr <$> newStablePtr (WrappedTimeZoneSeries x)

tzdbDestroy :: Ptr () -> IO ()
tzdbDestroy p = freeStablePtr sp
  where sp :: StablePtr WrappedTimeZoneSeries
        sp = castPtrToStablePtr p

tzdbGet :: Ptr () -> IO(HashMap Text TimeZoneSeries)
tzdbGet p = timeSeries <$> deRefStablePtr (castPtrToStablePtr p)

-- DucklingTime wrapper
newtype DucklingTimeWrapper = DucklingTimeWrapper { time :: DucklingTime }
foreign export ccall duckTimeDestroy :: Ptr () -> IO ()

duckTimeCreate :: DucklingTime -> IO(Ptr ())
duckTimeCreate t = castStablePtrToPtr <$> newStablePtr (DucklingTimeWrapper t)

duckTimeDestroy :: Ptr () -> IO ()
duckTimeDestroy p = freeStablePtr sp
  where sp :: StablePtr DucklingTimeWrapper
        sp = castPtrToStablePtr p

duckTimeGet :: Ptr () -> IO DucklingTime
duckTimeGet p = time <$> deRefStablePtr ( castPtrToStablePtr p )

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


parseTimeZone :: Text -> Maybe ByteString -> Text
parseTimeZone defaultTimeZone = maybe defaultTimeZone Text.decodeUtf8

someFunc :: IO ()
someFunc = putStrLn "someFunc"

foreign export ccall wcurrentReftime :: Ptr() -> Ptr() -> IO(Ptr())

wcurrentReftime  :: Ptr() -> Ptr() -> IO(Ptr())
wcurrentReftime tzdb strPtr = do
  timeSeries <- tzdbGet tzdb
  wrapString <- stringGet strPtr
  unwrappedStr <- peekCString $ wrapString
  let hsStr = cs (unwrappedStr)
  timeOut <- Duckling.Core.currentReftime timeSeries hsStr
  convertedTime <- duckTimeCreate timeOut
  return convertedTime

foreign export ccall wloadTimeZoneSeries :: Ptr() -> IO(Ptr ())

wloadTimeZoneSeries :: Ptr() -> IO(Ptr ())
wloadTimeZoneSeries pathPtr = do
  wrapString <- stringGet pathPtr
  unwrapString <- peekCString $ wrapString
  tzmap <- loadTimeZoneSeries unwrapString
  wrappedDB <- tzdbCreate tzmap
  return wrappedDB

-- parseRefTime :: Text -> DucklingTime
-- parseRefTime text = text
