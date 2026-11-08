module Kiwi.Utils
  ( -- * Server state access
    asksK
  , getDB
    -- * Metadata
  , parseLangSetting
  , pageContent
    -- * Lists
  , nodup
  , fastListDiff
    -- * DocIndex queries
  , qAny
  , qOnly
  , qHasField
    -- * Files and pages ids
  , isDotFile
  , isValidMdFilename
  , pathToPageId
    -- * Caching
  , initWAICaching
    -- * IO
  , traverseDir
  , getFileContent
  , timeIO
    -- * Misc
  , splitOnFirst
  , fst3
  ) where

import           Control.Monad (forM)
import           Control.Monad.Reader
import           Data.Char (isAlphaNum)
import           Data.IORef
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.IO as TSIO
import           GHC.Clock (getMonotonicTime)
import qualified Network.Wai.Middleware.Static as Static
import           Network.HTTP.Types.Header (RequestHeaders)
import qualified System.Directory as D
import qualified System.FilePath as FP
import qualified Text.Pandoc.Definition as PD
import qualified Text.Pandoc.Walk as PW
import           Text.Read (readMaybe)

import           Kiwi.Types
import qualified Utils.DocIndex as DI

serverM :: MonadTrans t => ServerM a -> t ServerM a
serverM = lift

getDB :: ActM PagesDB
getDB = do
  dbR <- serverM $ asks pagesDB
  liftIO $ readIORef dbR

asksK :: (ServerState -> a) -> ActM a
asksK = serverM . asks


nodup :: Ord a => [a] -> [a]
nodup = S.toList . S.fromList

fastListDiff :: Ord a => [a] -> [a] -> [a]
fastListDiff [] _  = []
fastListDiff l1 [] = l1
fastListDiff l1 l2 = S.toList $ S.difference (S.fromList l1) (S.fromList l2)

-- | Parse a lang field: "french", "auto/french", or "auto" alone, which
-- falls back to english
parseLangSetting :: T.Text -> Either String LangSetting
parseLangSetting t = case map T.strip $ T.splitOn "/" $ T.toLower t of
  ["auto"]    -> Right $ LangAuto English
  ["auto", l] -> LangAuto <$> readLang l
  [l]         -> LangFixed <$> readLang l
  _           -> Left $ "invalid lang : " ++ T.unpack t
  where
    readLang l = maybe (Left $ "unknown lang : " ++ T.unpack l) Right $
                 readMaybe (T.unpack l)

-- | Text of the page content, for indexing
pageContent :: PD.Pandoc -> T.Text
pageContent = T.unwords . T.words . PW.query textOf
  where
    textOf :: PD.Inline -> T.Text
    textOf (PD.Str s)    = s
    textOf (PD.Code _ s) = s
    textOf (PD.Math _ s) = s
    textOf PD.Space      = " "
    textOf PD.SoftBreak  = " "
    textOf PD.LineBreak  = " "
    textOf _          = ""



-- DocIndex request helpers

-- qAll :: (FieldKey key) =>
--         field ->
--         [key] ->
--         DI.Query field DocIndexKey
-- qAll f keys = DI.All f $ S.fromList $ map toKey keys

qAny :: (FieldKey key) =>
        field ->
        [key] ->
        DI.Query field DocIndexKey
qAny f keys = DI.Any f $ S.fromList $ map toKey keys

qOnly :: (FieldKey key) =>
         field ->
         key ->
         DI.Query field DocIndexKey
qOnly f key = DI.Only f $ toKey key

qHasField :: field ->
         DI.Query field DocIndexKey
qHasField f = DI.HasField f


-- qRange :: (FieldKey key) =>
--          field ->
--          key -> key ->
--          DI.Query field DocIndexKey
-- qRange f key1 key2 = DI.Range f (toKey key1) (toKey key2)

-- qGreaterThanOrEq, qLowerThanOrEq :: (FieldKey key) =>
--                                     field ->
--                                     key ->
--                                     DI.Query field DocIndexKey
-- qGreaterThanOrEq f key = DI.GreaterThanOrEq f $ toKey key
-- qLowerThanOrEq f key = DI.LowerThanOrEq f $ toKey key


isDotFile :: FP.FilePath -> Bool
isDotFile ('.':_) = True
isDotFile _ = False

isValidMdFilename :: FP.FilePath -> Bool
isValidMdFilename [] = False
isValidMdFilename fp@(first:_) = isAlphaNum first &&
                                 elem (FP.takeExtension fp) [".md", ".markdown"]

pathToPageId :: FP.FilePath -> T.Text
pathToPageId = normalizePageId . T.pack

normalizePageId :: T.Text -> T.Text
normalizePageId = T.replace "/" ":"

-- links written in pages are resolved by Kiwi.Pandoc.absoluteSegments
-- joinPathT :: [T.Text] -> T.Text
-- joinPathT = T.pack . FP.joinPath . map T.unpack

-- Caching helpers

initWAICaching :: IO Static.CacheContainer
initWAICaching = Static.initCaching (Static.CustomCaching staticCachingHeaders)
  where
    staticCachingHeaders :: Static.FileMeta -> RequestHeaders
    staticCachingHeaders fm =
      [ ("Cache-Control", "no-transform,public,max-age=3600,s-maxage=3600")
      , ("Last-Modified", Static.fm_lastModified fm)
      , ("ETag", Static.fm_etag fm)
      , ("Vary", "Accept-Encoding")
      ]


-- IO

traverseDir :: FP.FilePath -> (FP.FilePath -> Bool) -> IO [FP.FilePath]
traverseDir top include = do
  ds <- D.listDirectory top
  paths <- forM (filter include ds) $ \d -> do
    let path = FP.joinPath [top, d]
    s <- D.doesDirectoryExist path
    if s then traverseDir path include else return [path]
  return (concat paths)


getFileContent :: FP.FilePath -> IO (Maybe T.Text)
getFileContent p = do
    fileExists <- D.doesFileExist p
    if fileExists then do
      content <- TSIO.readFile p
      return $ Just content
    else return Nothing


-- | Result of the action, and the time elapsed while running it, in seconds
timeIO :: IO a -> IO (a, Double)
timeIO x = do
  bef <- getMonotonicTime
  res <- x
  aft <- getMonotonicTime
  return (res, aft - bef)



-- association list merge, left-priority
-- mergeAssoc :: Eq a => [[(a,b)]] -> [(a,b)]
-- mergeAssoc = L.nubBy (\(x,_) (y,_) -> x == y) . concat

splitOnFirst :: T.Text -> T.Text -> Maybe (T.Text, T.Text)
splitOnFirst sep t = case T.breakOn sep t of
  (_, "") -> Nothing
  (pref,rst) -> Just (pref, T.drop (T.length sep) rst)


fst3 :: (a,b,c) -> a
fst3 (a,_,_) = a
