module FFI
  ( someFunc
  , wcurrentReftime
  , wloadTimeZoneSeries
  )
where

import           Control.Applicative            hiding (empty)
import           Data.Text                      ( Text )
import           Data.ByteString                ( ByteString
                                                , empty
                                                )
import           Duckling.Core
-- import Duckling.Data.TimeZone
import           Duckling.Resolve               ( DucklingTime(..) )
import           Data.Aeson
import           Data.Maybe
import           Data.Tuple
import qualified Data.Text                     as Text
import qualified Data.Text.Encoding            as Text
import qualified Data.ByteString.Lazy.Char8    as C8

import qualified Control.Exception             as E
import           Control.Monad.Extra
import           Control.Monad.IO.Class
import           Data.Coerce
import           Data.Either
import           Data.HashMap.Strict            ( HashMap )
import qualified Data.HashMap.Strict           as HashMap
import qualified Data.HashSet                  as HashSet
import           Data.String
import           Data.String.Conversions        ( cs )
import           Data.Time                      ( TimeZone(..) )
import           Data.Time.LocalTime.TimeZone.Olson
import           Data.Time.LocalTime.TimeZone.Series
import           Data.Time.Format
import           Data.Time.Clock.POSIX          ( posixSecondsToUTCTime )
import           System.Directory
import           System.FilePath
import           System.IO.Unsafe
import           Text.Read

import           Foreign.StablePtr
import           Foreign.Ptr
import           Foreign.C
import           Foreign.C.Types
import           Foreign.Marshal.Array

import           Prelude
-- import Curryrs.Types

-- TimeZoneSeries wrapper
newtype WrappedTimeZoneSeries = WrappedTimeZoneSeries { timeSeries :: HashMap Text TimeZoneSeries }
foreign export ccall tzdbDestroy :: Ptr () -> IO ()

tzdbCreate :: HashMap Text TimeZoneSeries -> IO (Ptr ())
tzdbCreate x = castStablePtrToPtr <$> newStablePtr (WrappedTimeZoneSeries x)

tzdbDestroy :: Ptr () -> IO ()
tzdbDestroy p = freeStablePtr sp
 where
  sp :: StablePtr WrappedTimeZoneSeries
  sp = castPtrToStablePtr p

tzdbGet :: Ptr () -> IO (HashMap Text TimeZoneSeries)
tzdbGet p = timeSeries <$> deRefStablePtr (castPtrToStablePtr p)

-- DucklingTime wrapper
newtype DucklingTimeWrapper = DucklingTimeWrapper { time :: DucklingTime }
foreign export ccall duckTimeDestroy :: Ptr () -> IO ()

duckTimeCreate :: DucklingTime -> IO (Ptr ())
duckTimeCreate t = castStablePtrToPtr <$> newStablePtr (DucklingTimeWrapper t)

duckTimeDestroy :: Ptr () -> IO ()
duckTimeDestroy p = freeStablePtr sp
 where
  sp :: StablePtr DucklingTimeWrapper
  sp = castPtrToStablePtr p

duckTimeGet :: Ptr () -> IO DucklingTime
duckTimeGet p = time <$> deRefStablePtr (castPtrToStablePtr p)

duckTimeToZST :: DucklingTime -> ZoneSeriesTime
duckTimeToZST (DucklingTime zst) = zst

foreign export ccall duckTimeRepr :: Ptr() -> IO(CString)
duckTimeRepr p = do
  duckTime <- duckTimeGet p
  let zt = duckTimeToZST duckTime
  let dateRepr = formatTime defaultTimeLocale "%FT%T%Q%z" zt
  cRepr <- newCString dateRepr
  return cRepr

-- Lang wrapper
newtype LangWrapper = LangWrapper { lang :: Lang }
foreign export ccall langDestroy :: Ptr () -> IO ()

langCreate :: Lang -> IO (Ptr ())
langCreate l = castStablePtrToPtr <$> newStablePtr (LangWrapper l)

langDestroy :: Ptr () -> IO ()
langDestroy p = freeStablePtr sp
 where
  sp :: StablePtr LangWrapper
  sp = castPtrToStablePtr p

langGet :: Ptr () -> IO Lang
langGet p = lang <$> deRefStablePtr (castPtrToStablePtr p)

foreign export ccall langRepr :: Ptr() -> IO(CString)
langRepr p = do
  langName <- langGet p
  let langR = show langName
  cRepr <- newCString langR
  return cRepr

-- Locale wrapper
newtype LocaleWrapper = LocaleWrapper { loc :: Locale }
foreign export ccall localeDestroy :: Ptr () -> IO ()

localeCreate :: Locale -> IO (Ptr ())
localeCreate l = castStablePtrToPtr <$> newStablePtr (LocaleWrapper l)

localeDestroy :: Ptr () -> IO ()
localeDestroy p = freeStablePtr sp
 where
  sp :: StablePtr LocaleWrapper
  sp = castPtrToStablePtr p

localeGet :: Ptr () -> IO Locale
localeGet p = loc <$> deRefStablePtr (castPtrToStablePtr p)

foreign export ccall localeRepr :: Ptr() -> IO(CString)
localeRepr p = do
  localeF <- localeGet p
  let localeR = show localeF
  cRepr <- newCString localeR
  return cRepr

-- Dimension wrapper
newtype DimensionWrapper = DimensionWrapper { dimen :: Some Dimension }
foreign export ccall dimensionDestroy :: Ptr () -> IO ()

dimensionCreate :: Some Dimension -> IO (Ptr ())
dimensionCreate d = castStablePtrToPtr <$> newStablePtr (DimensionWrapper d)

dimensionDestroy :: Ptr() -> IO ()
dimensionDestroy p = freeStablePtr sp
 where
  sp :: StablePtr DimensionWrapper
  sp = castPtrToStablePtr p

dimensionGet :: Ptr () -> IO (Some Dimension)
dimensionGet p = dimen <$> deRefStablePtr (castPtrToStablePtr p)

-- DimensionList wrapper
newtype DimensionListWrapper = DimensionListWrapper {
  listDescriptor :: (Ptr(Ptr()), CInt)
}

foreign export ccall dimensionListCreate :: Ptr(Ptr()) -> CInt -> IO(Ptr())
foreign export ccall dimensionListLength :: Ptr() -> IO(CInt)
foreign export ccall dimensionListPtrs :: Ptr() -> IO(Ptr(Ptr()))
foreign export ccall dimensionListDestroy :: Ptr() -> IO ()

dimensionListCreate :: Ptr(Ptr ()) -> CInt -> IO(Ptr())
dimensionListCreate ptrs numElements = castStablePtrToPtr <$> newStablePtr (DimensionListWrapper (ptrs, numElements))

dimensionListDescr :: Ptr () -> IO (Ptr(Ptr()), CInt)
dimensionListDescr p = listDescriptor <$> deRefStablePtr (castPtrToStablePtr p)

dimensionListLength :: Ptr() -> IO(CInt)
dimensionListLength p = do
  descr <- dimensionListDescr p
  let numElements = snd descr
  return numElements

dimensionListPtrs :: Ptr() -> IO(Ptr(Ptr()))
dimensionListPtrs p = do
  descr <- dimensionListDescr p
  let dimensions = fst descr
  return dimensions

dimensionListDestroy :: Ptr() -> IO()
dimensionListDestroy p = freeStablePtr sp
 where
  sp :: StablePtr DimensionListWrapper
  sp = castPtrToStablePtr p


-- | Reference implementation for pulling TimeZoneSeries data from local
-- Olson files.
-- Many linux distros have Olson data in "/usr/share/zoneinfo/"
loadTimeZoneSeries :: FilePath -> IO (HashMap Text TimeZoneSeries)
loadTimeZoneSeries base = do
  files    <- getFiles base
  tzSeries <- mapM parseOlsonFile files
  -- This data is large, will live a long time, and essentially be constant,
  -- so it's a perfect candidate for compact regions
  return $ HashMap.fromList $ rights tzSeries
 where
    -- Multiple versions of the data can exist. We intentionally ignore the
    -- posix and right formats
  ignored_dirs = HashSet.fromList $ map (base </>) ["posix", "right"]

  -- Recursively crawls a directory to list every file underneath it,
  -- ignoring certain directories as needed
  getFiles :: FilePath -> IO [FilePath]
  getFiles dir = do
    fsAll <- getDirectoryContents dir
    let fs      = filter notDotFile fsAll
        full_fs = map (dir </>) fs
    (dirs, files) <- partitionM doesDirectoryExist full_fs

    subdirs       <- concatMapM
      getFiles
      [ d | d <- dirs, not $ HashSet.member d ignored_dirs ]

    return $ files ++ subdirs

  -- Attempts to read a file in Olson format and returns its
  -- canonical name (file path relative to the base) and the data
  parseOlsonFile :: FilePath -> IO (Either E.ErrorCall (Text, TimeZoneSeries))
  parseOlsonFile f = E.try $ do
    r <- getTimeZoneSeriesFromOlsonFile f
    return (Text.pack $ makeRelative base f, r)

  notDotFile s = not $ elem s [".", ".."]


parseTimeZone :: Text -> Maybe ByteString -> Text
parseTimeZone defaultTimeZone = maybe defaultTimeZone Text.decodeUtf8

someFunc :: IO ()
someFunc = putStrLn "someFunc"

foreign export ccall wcurrentReftime :: Ptr() -> CString -> IO(Ptr())

wcurrentReftime :: Ptr () -> CString -> IO (Ptr ())
wcurrentReftime tzdb tzStr = do
  timeSeries <- tzdbGet tzdb
  -- wrapString <- stringGet strPtr
  unwrappedStr <- peekCString $ tzStr
  let hsStr = cs (unwrappedStr)
  timeOut <- Duckling.Core.currentReftime timeSeries hsStr
  convertedTime <- duckTimeCreate timeOut
  return convertedTime

foreign export ccall wloadTimeZoneSeries :: CString -> IO(Ptr ())

wloadTimeZoneSeries :: CString -> IO (Ptr ())
wloadTimeZoneSeries pathCStr = do
  unwrapString <- peekCString $ pathCStr
  tzmap     <- loadTimeZoneSeries unwrapString
  wrappedDB <- tzdbCreate tzmap
  return wrappedDB


foreign export ccall wparseRefTime :: Ptr() -> CString -> CSUSeconds -> IO(Ptr ())

wparseRefTime :: Ptr () -> CString -> CSUSeconds -> IO (Ptr ())
wparseRefTime tzdb tzCStr refTimeC = do
  timeSeries   <- tzdbGet tzdb
  unwrappedStr <- peekCString $ tzCStr
  let utcTime = posixSecondsToUTCTime (realToFrac refTimeC)
  let refTime = makeReftime timeSeries (cs (unwrappedStr)) utcTime
  wrappedRefTime <- duckTimeCreate refTime
  return wrappedRefTime

parseLang :: Text -> Lang
parseLang l = fromMaybe EN (readMaybe (Text.unpack (Text.toUpper l)))

foreign export ccall wparseLang :: CString -> IO(Ptr ())

wparseLang :: CString -> IO (Ptr ())
wparseLang langCStr = do
  langStr <- peekCString $ langCStr
  let mappedLang = parseLang (cs (langStr))
  wrappedLang <- langCreate (mappedLang)
  return wrappedLang

foreign export ccall wmakeDefaultLocale :: Ptr() -> IO(Ptr ())

wmakeDefaultLocale :: Ptr () -> IO (Ptr ())
wmakeDefaultLocale langPtr = do
  lang <- langGet langPtr
  let loc = makeLocale lang Nothing
  wrappedLocale <- localeCreate (loc)
  return wrappedLocale

parseLocale :: Text -> Locale -> Locale
parseLocale x defaultLocale = maybe defaultLocale (`makeLocale` mregion) mlang
 where
  (mlang, mregion) = case chunks of
    [a, b] -> (readMaybe a :: Maybe Lang, readMaybe b :: Maybe Region)
    _      -> (Nothing, Nothing)
  chunks = map Text.unpack . Text.split (== '_') . Text.toUpper $ x

foreign export ccall wparseLocale :: CString -> Ptr() -> IO(Ptr ())
wparseLocale :: CString -> Ptr() -> IO(Ptr ())
wparseLocale localeCStr defaultLocalePtr = do
  unwrappedLocale <- peekCString $ localeCStr
  defaultLocale   <- localeGet defaultLocalePtr
  let loc = parseLocale (cs (unwrappedLocale)) defaultLocale
  wrappedLocale <- localeCreate (loc)
  return wrappedLocale


parseDimension :: Text -> Maybe (Some Dimension)
parseDimension x = fromName x <|> fromCustomName x
  where
    fromCustomName :: Text -> Maybe (Some Dimension)
    fromCustomName name = HashMap.lookup name m
    m = HashMap.fromList
      [ -- ("my-dimension", This (CustomDimension MyDimension))
      ]

convertDimension :: CString -> Maybe(Some Dimension)
convertDimension x = parseDimension ( cs(unsafePerformIO(peekCString (x))) )


wrapDimension :: Some Dimension -> IO(Ptr ())
wrapDimension dim = dimensionCreate (dim)

convertString :: CString -> Text
convertString x = cs(unsafePerformIO(peekCString (x)))

foreign export ccall wparseDimensions :: CInt -> Ptr(CString) -> IO(Ptr ())

wparseDimensions :: CInt -> Ptr(CString) -> IO(Ptr ())
wparseDimensions n dimensionsPtr = do
  cDimensions <- peekArray (fromInteger(toInteger n)) dimensionsPtr
  let strings = map convertString cDimensions
  let dimensions = mapMaybe convertDimension cDimensions
  wrappedDimensions <- newArray =<< traverse wrapDimension dimensions
  let cLength = toEnum(length(dimensions))
  wrappedDimList <- dimensionListCreate wrappedDimensions cLength
  return wrappedDimList

unwrapDimension :: Ptr() -> Some Dimension
unwrapDimension p = unsafePerformIO(dimensionGet p)


foreign export ccall wparseText :: CString -> Ptr() -> Ptr() -> Ptr() -> CBool -> IO(CString)
wparseText :: CString -> Ptr() -> Ptr() -> Ptr() -> CBool -> IO(CString)
wparseText cText refTimePtr localePtr dimListPtr cWithLatent = do
  refTime <- duckTimeGet refTimePtr
  locale <- localeGet localePtr
  dimLength <- dimensionListLength dimListPtr
  dimRefs <- dimensionListPtrs dimListPtr
  dimPtrs <- peekArray (fromInteger(toInteger dimLength)) dimRefs
  let dimensions = map unwrapDimension dimPtrs
  -- dimensions <- newArray =<< traverse unwrapDimension dimPtrs
  let context = Context {
    referenceTime = refTime,
    locale = locale
  }
  let options = Options {
    withLatent = (cWithLatent == 1)
  }
  let textT = convertString(cText)
  let entities = parse textT context options dimensions
  let entityStr = C8.unpack(encode entities)
  cEntities <- newCString entityStr
  return cEntities
