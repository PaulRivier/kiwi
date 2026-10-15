module Kiwi.Utils
  ( -- * Server state access
    asksK
  , getDB
  , getSessions
  , modifySessions
  , isSessionValid
    -- * Cookies
  , setKiwiCookie
  , getKiwiCookie
  , deleteKiwiCookie
    -- * Accounts and passwords
  , verifyLogin
  , hashPassword
  , isAdmin
  , randomText
    -- * Tags and metadata
  , tagSubSegments
  , tagToSegments
  , tagsListSubSegments
  , segmentsToTag
  , prettyTag
  , prefixNormalizeTag
  , filterRootTags
  , splitMeta
    -- * Lists
  , nodup
  , fastListDiff
    -- * DocIndex queries
  , qAny
  , qOnly
  , qHasField
    -- * Browse requests
  , BrowseAllReq
  , reqToQuery
  , createReq
  , addToReq
  , removeFromReq
  , browseUrl
  , decodeReq
  , showKeyLabel
    -- * Files, pages ids and URLs
  , isDotFile
  , isValidMdFilename
  , pathToPageId
  , normalizePageId
  , pageUrl
  , pageUIDLink
  , joinPathT
    -- * Caching
  , initWAICaching
    -- * IO
  , traverseDir
  , getFileContent
  , timeIO
  , loadAccounts
  , dumpAccounts
  , loadSessions
    -- * Misc
  , splitOnFirst
  , fst3
  ) where

import           Control.Concurrent.MVar (readMVar, modifyMVar_)
import           Control.Monad (forM, foldM)
import           Control.Monad.Reader
import qualified Crypto.Error as CE
import qualified Crypto.KDF.Argon2 as Argon2
import qualified Crypto.Random as CR
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import           Data.Char (isAlphaNum)
import           Data.IORef
import qualified Data.List as L
import qualified Data.Map.Strict as M
import           Data.Maybe (mapMaybe, fromMaybe)
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TSIO
import qualified Data.Time as Time
import qualified Network.HTTP.Types.URI as URI
import qualified Network.Wai.Middleware.Static as Static
import           Network.HTTP.Types.Header (RequestHeaders)
import           System.CPUTime
import qualified System.Directory as D
import qualified System.FilePath as FP
import           Text.Read (readMaybe)
import qualified Web.Scotty.Cookie as CK

import           Kiwi.Types
import qualified Utils.DocIndex as DI

serverM :: MonadTrans t => ServerM a -> t ServerM a
serverM = lift

getDB :: ActM PagesDB
getDB = do
  dbR <- serverM $ asks pagesDB
  liftIO $ readIORef dbR

getSessions :: ActM Sessions
getSessions = do
  sM <- asksK sessions
  liftIO $ readMVar sM

-- | Modify sessions and save them to disk. The MVar is held during the
-- whole operation, so concurrent modifications (login, logout) are
-- serialized and the file always matches the memory state. Expired
-- sessions are purged at the same time.
modifySessions :: (Sessions -> Sessions) -> ActM ()
modifySessions f = do
  sM <- asksK sessions
  kd <- asksK kiwiDir
  liftIO $ modifyMVar_ sM $ \s -> do
    now <- Time.getCurrentTime
    let newS = M.filter (isSessionValid now) $ f s
    dumpSessions newS $ FP.combine kd "_sessions"
    return newS

-- | Maximum duration of a session, server side and for the cookie.
sessionMaxAge :: Time.NominalDiffTime
sessionMaxAge = 365 * Time.nominalDay

-- | A session is valid if it has been opened less than 'sessionMaxAge' ago.
isSessionValid :: Time.UTCTime -> Login -> Bool
isSessionValid now (_, _, loginTime) =
  Time.diffUTCTime now loginTime < sessionMaxAge

getCookieName :: ActM T.Text
getCookieName = do
  n <- asksK kiwiName
  return $ T.append "kiwi-" $ T.filter isAlphaNum n

-- | Session cookie: not readable from javascript (HttpOnly), not sent
-- on cross-site POST requests (SameSite=Lax), valid for the whole site,
-- and only sent over HTTPS if secure-cookie is set in config.
kiwiCookie :: T.Text -> ActM CK.SetCookie
kiwiCookie value = do
  cn <- getCookieName
  secure <- asksK secureCookie
  return $ (CK.makeSimpleCookie cn value)
             { CK.setCookiePath     = Just "/"
             , CK.setCookieHttpOnly = True
             , CK.setCookieSameSite = Just CK.sameSiteLax
             , CK.setCookieSecure   = secure
             }

setKiwiCookie :: T.Text -> ActM ()
setKiwiCookie v = do
  c <- kiwiCookie v
  CK.setCookie $ c { CK.setCookieMaxAge = Just (realToFrac sessionMaxAge) }

getKiwiCookie :: ActM (Maybe T.Text)
getKiwiCookie  = CK.getCookie =<< getCookieName

deleteKiwiCookie :: ActM ()
deleteKiwiCookie = do
  c <- kiwiCookie ""
  CK.setCookie $ c { CK.setCookieMaxAge = Just 0 }


asksK :: (ServerState -> a) -> ActM a
asksK = serverM . asks

verifyLogin :: [Account] -> T.Text -> T.Text -> Maybe (T.Text, [T.Text])
verifyLogin accounts' u p =
  L.find (\a -> accountId a == u) accounts' >>= \a ->
  case hashPassword (accountSalt a) p == Just (accountHash a) of
    True -> Just (accountId a, accountGroups a)
    False -> Nothing

-- | hex encoded Argon2id hash of password, with given salt
-- (fails if salt is shorter than 8 bytes)
hashPassword :: T.Text -> T.Text -> Maybe T.Text
hashPassword salt pass =
  let hashM = Argon2.hash argon2Options (TE.encodeUtf8 pass) (TE.encodeUtf8 salt) 32
  in TE.decodeUtf8 . B16.encode <$> CE.maybeCryptoError hashM
  where
    -- OWASP recommended parameters: 19 MiB, 2 iterations, 1 thread
    argon2Options = Argon2.Options { Argon2.iterations = 2
                                   , Argon2.memory = 19 * 1024
                                   , Argon2.parallelism = 1
                                   , Argon2.variant = Argon2.Argon2id
                                   , Argon2.version = Argon2.Version13 }

isAdmin :: ActM Bool
isAdmin = do
  loginM <- asksK login
  return $ case loginM of
    Nothing -> False
    Just (_,g,_) -> elem "admin" g
      

-- loisir > sport > escalade =>
--   [ loisir, loisir > sport, loisir > sport > escalade ]
tagSubSegments :: TagId -> [TagSegments]
tagSubSegments = drop 1 . L.inits . tagToSegments

tagToSegments :: TagId -> TagSegments
tagToSegments = splitTextEsc '>'

tagsListSubSegments :: [TagId] -> [TagSegments]
tagsListSubSegments = nodup . concatMap tagSubSegments

segmentsToTag :: TagSegments -> TagId
segmentsToTag = T.intercalate (T.pack ">") .
                map (T.replace ">" "\\>")

prettyTag :: TagId -> TagId
prettyTag = T.intercalate (T.pack " > ") . tagToSegments

-- normalizeTag :: TagId -> TagId
-- normalizeTag = segmentsToTag . tagToSegments

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

-- | Split on separator, unless it is escaped by a backslash. Other
-- backslashes are kept as is. Parts are stripped, empty parts are dropped.
splitTextEsc :: Char -> T.Text -> [T.Text]
splitTextEsc sep = filter (not . T.null) . map (T.strip . T.pack . reverse) .
                   go [] . T.unpack
  where
    -- acc is the current part, reversed
    go acc ('\\':c:rs) | c == sep = go (c:acc) rs
    go acc (c:rs)      | c == sep = acc : go [] rs
                       | otherwise = go (c:acc) rs
    go acc []          = [acc]




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


-- Browse requests

-- | Filter of the browse page: the pages must have all the keys of every
-- field. In URLs, it is a query string with one parameter per key, e.g.
-- @/browse?source=s&tag=a%3Eb&tag=c&m.author=Smith@: "source", "tag", and
-- "m." followed by the name of a custom metadata field.
type BrowseAllReq = M.Map MetaField (S.Set DocIndexKey)

reqToQuery :: BrowseAllReq -> DI.Query MetaField DocIndexKey
reqToQuery r = DI.And $ map (uncurry DI.All) $ M.toList r

createReq :: (MetaField, DocIndexKey) -> BrowseAllReq
createReq = addToReq M.empty

-- | Add a key to a request. The parents of a new tag are removed, since
-- pages are indexed with all the parents of their tags.
addToReq :: BrowseAllReq -> (MetaField, DocIndexKey) -> BrowseAllReq
addToReq r (f,k) = M.insertWith S.union f (S.singleton k) $ case (f,k) of
  (FieldTag, KeyText t) -> M.adjust (S.filter (not . isParentOf t)) FieldTag r
  _ -> r
  where
    isParentOf t (KeyText p) = let ps = tagToSegments p
                                   ts = tagToSegments t
                               in ps /= ts && L.isPrefixOf ps ts
    isParentOf _ _ = False

removeFromReq :: BrowseAllReq -> (MetaField, DocIndexKey) -> BrowseAllReq
removeFromReq r (f,k) = M.update remove f r
  where remove ks = let ks' = S.delete k ks
                    in if S.null ks' then Nothing else Just ks'

-- | Link to the browse page of a request. Fields and keys are always in the
-- same order, so a request has only one URL.
browseUrl :: BrowseAllReq -> T.Text
browseUrl r = T.append "/browse" $ TE.decodeUtf8 $ URI.renderQuery True
  [ (TE.encodeUtf8 n, Just $ TE.encodeUtf8 v)
  | (f, ks) <- M.toList r, Just n <- [fieldParam f]
  , k <- S.toList ks, Just v <- [keyParam k] ]
  where
    fieldParam FieldSource = Just "source"
    fieldParam FieldTag = Just "tag"
    fieldParam (FieldCustom n) = Just $ T.append "m." n
    fieldParam _ = Nothing
    keyParam (KeyText t) = Just t
    keyParam (KeyInt i) = Just $ T.pack $ show i
    keyParam (KeyDay d) = Just $ T.pack $ Time.showGregorian d
    keyParam (KeyBool b) = Just $ if b then "true" else "false"
    keyParam _ = Nothing

-- | Decode the parameters of a browse URL (see 'BrowseAllReq'). Custom
-- fields must be in the config, and their keys are read according to its
-- type. Unknown parameters and invalid keys are errors.
decodeReq :: [CustomMetaConfig] -> [(T.Text, T.Text)] -> Either T.Text BrowseAllReq
decodeReq cmcs = foldM addParam M.empty
  where
    addParam r (n, v) = do
      (f, readKey) <- maybe (Left $ T.append "unknown parameter: " n) Right $
                      fieldReader n
      k <- maybe (Left $ T.concat ["invalid value for ", n, ": ", v]) Right $
           readKey v
      return $ addToReq r (f, k)

    fieldReader "source" = Just (FieldSource, readText)
    fieldReader "tag" = Just (FieldTag, readText . segmentsToTag . tagToSegments)
    fieldReader n = do
      name <- T.stripPrefix "m." n
      cmc <- L.find ((== name) . cmcName) cmcs
      return (FieldCustom name, readCustom $ cmcType cmc)

    readText t = if T.null t then Nothing else Just $ KeyText t

    readCustom CmtText = readText
    readCustom CmtInt = fmap KeyInt . readMaybe . T.unpack
    readCustom CmtDate = fmap KeyDay .
      Time.parseTimeM False Time.defaultTimeLocale "%Y-%-m-%-d" . T.unpack
    readCustom CmtBool = \b -> case b of
      "true" -> Just $ KeyBool True
      "false" -> Just $ KeyBool False
      _ -> Nothing

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
isValidMdFilename [] = False
isValidMdFilename fp@(first:_) = isAlphaNum first &&
                                 elem (FP.takeExtension fp) [".md", ".markdown"]

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

-- urlDecodeText :: T.Text -> T.Text
-- urlDecodeText = TE.decodeUtf8 . URI.urlDecode False . TE.encodeUtf8

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


-- | Random alphanumeric text of length l, from a cryptographically
-- secure generator (used for session keys and password salts).
randomText :: Int -> IO T.Text
randomText l = go l T.empty
 where
   go n acc
     | n <= 0 = return acc
     | otherwise = do
         bytes <- CR.getRandomBytes (n + 8) :: IO BS.ByteString
         -- rejection sampling: keep only bytes below 248 (4 * 62), so that
         -- every character has the same probability
         let chars = T.pack $ map toChar $ filter (< limit) $ BS.unpack bytes
             new = T.take n chars
         go (n - T.length new) (acc <> new)
   limit = fromIntegral (4 * length alphaNum)
   toChar w = alphaNum !! (fromIntegral w `mod` length alphaNum)
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
    Just c -> return $ mapMaybe readAccount $ T.lines c
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
  
-- | Load sessions from file, expired sessions are dropped
loadSessions :: FP.FilePath -> IO Sessions
loadSessions p = do
  fcM <- getFileContent p
  td <- Time.getCurrentTime
  let dd = Time.addUTCTime (-100000) td
  case fcM of
    Nothing -> return M.empty
    Just c -> return $ M.filter (isSessionValid td) $ M.fromList $
              mapMaybe (readSession dd) $ T.lines c
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
-- mergeAssoc :: Eq a => [[(a,b)]] -> [(a,b)]
-- mergeAssoc = L.nubBy (\(x,_) (y,_) -> x == y) . concat

splitOnFirst :: T.Text -> T.Text -> Maybe (T.Text, T.Text)
splitOnFirst sep t = case T.breakOn sep t of
  (_, "") -> Nothing
  (pref,rst) -> Just (pref, T.drop (T.length sep) rst)


fst3 :: (a,b,c) -> a
fst3 (a,_,_) = a
