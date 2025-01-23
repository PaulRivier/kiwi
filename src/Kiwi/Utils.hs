{-# LANGUAGE OverloadedStrings #-}

module Kiwi.Utils where

import           Control.Monad.Reader
import qualified Crypto.Hash.SHA256 as H
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.UTF8 as BS8
import           Data.Char (isAlphaNum)
import           Data.IORef
import qualified Data.List as L
import qualified Data.Map.Strict as M
import           Data.Maybe (catMaybes, fromMaybe)
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TSIO
import qualified Data.Time as Time
import qualified Network.HTTP.Types.URI as URI
import qualified Network.Wai.Middleware.Static as Static
import           Network.HTTP.Types.Header (RequestHeaders)
import           System.Random (randomRIO)
import           System.CPUTime
import qualified System.Directory as D
import qualified System.FilePath as FP
import           Text.Read (readMaybe)

import           Kiwi.Types
import qualified Utils.Cookies as CK
import qualified Utils.DocIndex as DI

serverM :: MonadTrans t => ServerM a -> t ServerM a
serverM = lift

getDB :: ActM PagesDB
getDB = do
  dbR <- serverM $ asks pagesDB
  liftIO $ readIORef dbR

getSessions :: ActM Sessions
getSessions = do
  sR <- serverM $ asks sessions
  liftIO $ readIORef sR

updateSessions :: Sessions -> ActM ()
updateSessions newS = do
  sR <- serverM $ asks sessions
  liftIO $ atomicWriteIORef sR newS

getCookieName :: ActM T.Text
getCookieName = do
  n <- asksK kiwiName
  return $ T.append "kiwi-" $ T.filter isAlphaNum n

setKiwiCookie :: T.Text -> ActM ()
setKiwiCookie v = do
  cn <- getCookieName
  let year = 365 * 24 * 60 * 60 -- number of seconds in a year
  CK.setSimpleCookie cn v year

getKiwiCookie :: ActM (Maybe T.Text)
getKiwiCookie  = CK.getCookie =<< getCookieName

deleteKiwiCookie :: ActM ()
deleteKiwiCookie = CK.deleteCookie =<< getCookieName


asksK :: (ServerState -> a) -> ActM a
asksK = serverM . asks

verifyLogin :: [Account] -> T.Text -> T.Text -> Maybe (T.Text, [T.Text])
verifyLogin accounts' u p =
  L.find (\a -> accountId a == u) accounts' >>= \a -> 
  let block = T.concat [accountSalt a, p]
      hashCandidate = TE.decodeUtf8 $ B16.encode $ H.hash $ TE.encodeUtf8 block
  in case hashCandidate == accountHash a of
    True -> Just (accountId a, accountGroups a)
    False -> Nothing

isAdmin :: ActM Bool
isAdmin = do
  loginM <- asksK login
  return $ case loginM of
    Nothing -> False
    Just (_,g,_) -> elem "admin" g
      

-- loisir > sport > escalade =>
--   [ loisir, loisir > sport, loisir > sport > escalade ]
tagSubSegments :: TagId -> [TagSegments]
tagSubSegments = tail . L.inits . tagToSegments

tagToSegments :: TagId -> TagSegments
tagToSegments = splitTextEsc '>'

tagsListSubSegments :: [TagId] -> [TagSegments]
tagsListSubSegments = nodup . concatMap tagSubSegments

segmentsToTag :: TagSegments -> TagId
segmentsToTag = T.intercalate (T.pack ">") .
                map (T.replace ">" "\\>")

prettyTag :: TagId -> TagId
prettyTag = T.intercalate (T.pack " > ") . tagToSegments

normalizeTag :: TagId -> TagId
normalizeTag = segmentsToTag . tagToSegments

prefixNormalizeTag :: TagSegments -> TagId -> TagId
prefixNormalizeTag prefix = segmentsToTag . (prefix ++) . tagToSegments

filterRootTags :: [TagSegments] -> [TagSegments]
filterRootTags tss = let set = S.fromList tss in filter (isRoot set) tss
  where isRoot _ (_:[]) = True
        isRoot set ts = not $ S.member (init ts) set

nodup :: Ord a => [a] -> [a]
nodup = S.toList . S.fromList

fastListDiff :: Ord a => [a] -> [a] -> [a]
fastListDiff [] _  = []
fastListDiff l1 [] = l1
fastListDiff l1 l2 = S.toList $ S.difference (S.fromList l1) (S.fromList l2)

splitMeta :: T.Text -> [TagId]
splitMeta = splitTextEsc ','

splitTextEsc :: Char -> T.Text -> [T.Text]
splitTextEsc sep = filter (not . T.null) . map T.strip . 
                   joinAfterEsc . T.split (== sep)
  where
    joinAfterEsc []       = []
    joinAfterEsc (a:[])   = [a]
    joinAfterEsc (a:b:rs) = case T.stripSuffix "\\" a of
      Nothing -> a : joinAfterEsc (b:rs)
      Just ax -> (T.concat [ax, T.singleton sep, b]) : joinAfterEsc rs




-- DocIndex request helpers

qAll, qAny :: (Ord field, Ord key, FieldKey key) =>
              field ->
              [key] ->
              DI.Query field DocIndexKey
qAll f keys = DI.All f $ S.fromList $ map toKey keys
qAny f keys = DI.Any f $ S.fromList $ map toKey keys

qOnly :: (Ord field, Ord key, FieldKey key) =>
         field ->
         key ->
         DI.Query field DocIndexKey
qOnly f key = DI.Only f $ toKey key

qHasField :: (Ord field) =>
         field ->
         DI.Query field DocIndexKey
qHasField f = DI.HasField f


qRange :: (Ord field, Ord key, FieldKey key) =>
         field ->
         key -> key ->
         DI.Query field DocIndexKey
qRange f key1 key2 = DI.Range f (toKey key1) (toKey key2)

qGreaterThanOrEq, qLowerThanOrEq :: (Ord field, Ord key, FieldKey key) =>
                                    field ->
                                    key ->
                                    DI.Query field DocIndexKey
qGreaterThanOrEq f key = DI.GreaterThanOrEq f $ toKey key
qLowerThanOrEq f key = DI.LowerThanOrEq f $ toKey key


-- Others DocIndex and URI construction utils
type BrowseAllReq = M.Map MetaField [DocIndexKey]

reqToQuery :: BrowseAllReq -> DI.Query MetaField DocIndexKey
reqToQuery r = DI.And $ map (\(f,ks) -> DI.All f $ S.fromList ks) $ M.toList r

createReq :: (MetaField, DocIndexKey) -> BrowseAllReq
createReq (f,k) = M.fromList [(f,[k])]

addToReq :: BrowseAllReq -> (MetaField, DocIndexKey) -> BrowseAllReq
addToReq r (f,k) = M.insertWith (++) f [k] r

decodeReq :: T.Text -> BrowseAllReq
decodeReq r = case readMaybe $ BS8.toString $ URI.urlDecode False $ TE.encodeUtf8 r of
  Just m -> M.fromList m
  Nothing -> M.empty

encodeReq :: BrowseAllReq -> T.Text
encodeReq m = TE.decodeUtf8 $ URI.urlEncode False $ BS8.fromString $ show $ M.toList m

showKeyLabel :: DocIndexKey -> T.Text
showKeyLabel k = case k of
  KeyText t -> t
  KeyInt i -> T.pack $ show i
  KeyDay d -> T.pack $ show d
  KeyBool b -> T.pack $ show b
  KeyLang l -> T.pack $ show l
  KeyPairText p -> T.pack $ show p

isDotFile :: FP.FilePath -> Bool
isDotFile ('.':_) = True
isDotFile _ = False

isValidMdFilename :: FP.FilePath -> Bool
isValidMdFilename fp = let first = head fp
                           ext = FP.takeExtension fp
                       in isAlphaNum first &&
                          elem ext [".md", ".markdown"]

pathToPageId :: FP.FilePath -> T.Text
pathToPageId = normalizePageId . T.pack

normalizePageId :: T.Text -> T.Text
normalizePageId = T.replace "/" ":"

pageUrl :: Page -> T.Text
pageUrl p = let (s,i) = pageUID p in
  T.concat ["/page/", urlEncodeText s, "/", urlEncodeText i]

pageUIDLink :: Page -> T.Text
pageUIDLink p = let (_,i) = pageUID p in
  T.concat ["/", i]

urlEncodeText :: T.Text -> T.Text
urlEncodeText = TE.decodeUtf8 . URI.urlEncode False . TE.encodeUtf8 

urlDecodeText :: T.Text -> T.Text
urlDecodeText = TE.decodeUtf8 . URI.urlDecode False . TE.encodeUtf8 

joinPathT :: [T.Text] -> T.Text
joinPathT = T.pack . FP.joinPath . map T.unpack

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


randomText :: Int -> IO T.Text
randomText l = T.pack <$> replicateM l randAlphaNum
 where
   randAlphaNum = do
     i <- randomRIO (0, (length alphaNum)  - 1)
     return $ alphaNum !! i
   alphaNum = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']

timeIO :: IO a -> IO (a, Double)
timeIO x = do
  bef <- liftIO getCPUTime
  res <- x
  aft <- liftIO getCPUTime
  let t = fromIntegral (aft-bef) * 1e-12
  return (res, t)


loadAccounts :: FP.FilePath -> IO [Account]
loadAccounts p = do
  fcM <- getFileContent p
  case fcM of
    Nothing -> return []
    Just c -> return $ catMaybes $ map readAccount $ T.lines c
  where
    readAccount l = case T.splitOn ":" l of
      (lg:salt:hash:grps:[]) -> Just $
        Account lg salt hash (T.splitOn "," grps)
      _ -> Nothing

dumpAccounts :: [Account] -> FP.FilePath -> IO ()
dumpAccounts accounts' p =
  let c = T.intercalate "\n" $ map dumpAccount accounts'
  in TSIO.writeFile p c
  where dumpAccount a = T.intercalate ":" [ accountId a, accountSalt a, accountHash a,
                                            T.intercalate "," $ accountGroups a ]
  
loadSessions :: FP.FilePath -> IO Sessions
loadSessions p = do
  fcM <- getFileContent p
  td <- Time.getCurrentTime
  let dd = Time.addUTCTime (-100000) td
  case fcM of
    Nothing -> return M.empty
    Just c -> return $ M.fromList $ catMaybes $ map (readSession dd) $ T.lines c
  where
    -- sessionId:user:groups
    readSession dd l = case T.splitOn ":" l of
      (sId:user:groups:time:[]) -> 
        let time' = fromMaybe dd $
                       Time.parseTimeM False Time.defaultTimeLocale "%s" (T.unpack time)
        in Just (sId, (user, T.splitOn "," groups, time'))
      _ -> Nothing

dumpSessions :: Sessions -> FP.FilePath -> IO ()
dumpSessions sessions' p =
  let c = T.intercalate "\n" $ map dumpSession $ M.toList sessions'
  in TSIO.writeFile p c
  where dumpSession (sId, (user,groups,time)) =
          T.intercalate ":" [ sId, user, T.intercalate "," groups,
                              T.pack $ Time.formatTime Time.defaultTimeLocale "%s" time ]



-- association list merge, left-priority
mergeAssoc :: Eq a => [[(a,b)]] -> [(a,b)]
mergeAssoc = L.nubBy (\(x,_) (y,_) -> x == y) . concat

splitOnFirst :: T.Text -> T.Text -> Maybe (T.Text, T.Text)
splitOnFirst sep t = case T.breakOn sep t of
  (_, "") -> Nothing
  (pref,rst) -> Just (pref, T.drop (T.length sep) rst)


concatMapM :: Monad m => (a -> m [b]) -> [a] -> m [b]
concatMapM f xs = liftM concat (mapM f xs)

(>:) :: Functor f => f a -> (a -> b) -> f b
(>:) = flip fmap

for :: Functor f => f a -> (a -> b) -> f b
for = flip fmap

(>$) :: a -> (a -> b) -> b
a >$ b = b a

fst3 :: (a,b,c) -> a
fst3 (a,_,_) = a
