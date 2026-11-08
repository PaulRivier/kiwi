-- | Accounts, passwords, sessions and the session cookie
module Kiwi.Auth
  ( -- * Sessions
    getSessions
  , modifySessions
  , loadSessions
  , requestLogin
  , hashToken
  , newSessionToken
  , newSession
  , parseSessions        -- exported for tests
  , renderSessions       -- exported for tests
  , SessionStatus (..)   -- exported for tests
  , sessionStatus        -- exported for tests
    -- * Cookies
  , setKiwiCookie
  , getKiwiCookie
  , deleteKiwiCookie
  , cookieName           -- exported for tests
    -- * Accounts and passwords
  , checkLogin
  , verifyLogin          -- exported for tests
  , hashPassword
  , isAdmin
  , readableQuery
  , randomText
  , loadAccounts
  , reloadAccounts
  , dumpAccounts
  , parseAccounts        -- exported for tests
  , renderAccounts       -- exported for tests
  , validateLogin
  , parseGroups
  , ownerWarning         -- exported for tests
    -- * Logs
  , clientAddress
  , sanitizeLogText
  ) where

import           Control.Concurrent.MVar (readMVar, modifyMVar_, withMVar)
import           Control.Concurrent.QSem (QSem, waitQSem, signalQSem)
import           Control.Exception (IOException, bracket_, bracketOnError, evaluate, try)
import           Control.Monad (join, void)
import           Control.Monad.Reader
import qualified Crypto.Error as CE
import           Crypto.Hash (hashWith, SHA256 (..))
import qualified Crypto.KDF.Argon2 as Argon2
import qualified Crypto.Random as CR
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import           Data.Char (isAlphaNum, isAscii, isDigit, isPrint)
import           Data.IORef (readIORef, atomicWriteIORef)
import qualified Data.List as L
import qualified Data.Map.Strict as M
import           Data.Maybe (fromMaybe, isJust, mapMaybe)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TSIO
import qualified Data.Time as Time
import qualified System.Directory as D
import qualified System.FilePath as FP
import           System.IO (hClose, hSetEncoding, openTempFile, utf8)
import qualified System.Posix.Files as PF
import           System.Posix.Types (UserID)
import qualified Web.Scotty.Cookie as CK

import qualified Kiwi.Log as Log
import           Kiwi.Types
import           Kiwi.Utils (asksK, getFileContent)
import qualified Kiwi.Utils as U
import qualified Utils.DocIndex as DI


--------------------------------------------------------------------------------
-- Sessions

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
  logger' <- asksK logger
  liftIO $ modifyMVar_ sM $ \s -> do
    now <- Time.getCurrentTime
    let newS = M.filter (isSessionValid now) $ f s
    warningM <- dumpSessions newS $ FP.combine kd "_sessions"
    mapM_ (logger' Log.Warn . T.pack) warningM
    return newS

-- | Maximum duration of a session, server side and for the cookie.
sessionMaxAge :: Time.NominalDiffTime
sessionMaxAge = 365 * Time.nominalDay

-- | A session is valid if it has been opened less than 'sessionMaxAge' ago.
isSessionValid :: Time.UTCTime -> Session -> Bool
isSessionValid now s =
  Time.diffUTCTime now (sessionTime s) < sessionMaxAge

-- | Short fingerprint of the salt of an account, kept in its sessions: the
-- salt changes with the password, and when the account is created again,
-- which closes the sessions opened before
saltFingerprint :: T.Text -> T.Text
saltFingerprint = T.take 16 . hashToken

-- | New session of an account
newSession :: Account -> Time.UTCTime -> Session
newSession a = Session (accountId a) (saltFingerprint $ accountSalt a)

-- | Key of a session in 'Sessions' and in the _sessions file: the hex
-- SHA-256 of the token of the cookie, so that reading the file does not
-- give the tokens. The token is random and long, a fast hash is enough.
hashToken :: T.Text -> T.Text
hashToken = T.pack . show . hashWith SHA256 . TE.encodeUtf8

-- | A new random session token, for the cookie
newSessionToken :: IO T.Text
newSessionToken = randomText 128

-- | What a session gives to a request
data SessionStatus = NoSession              -- ^ unknown or expired session
                   | RevokedSession T.Text  -- ^ valid session of an account that
                                            -- no longer exists, or whose
                                            -- password has changed
                   | ActiveSession Login    -- ^ with the current groups of the account
  deriving (Eq, Show)

sessionStatus :: [Account] -> Time.UTCTime -> Maybe Session -> SessionStatus
sessionStatus accounts' now sessionM = case sessionM of
  Nothing -> NoSession
  Just s
    | not (isSessionValid now s) -> NoSession
    | otherwise -> case L.find (\a -> accountId a == user) accounts' of
        Just a | saltFingerprint (accountSalt a) == sessionSaltFp s ->
                   ActiveSession (user, accountGroups a, sessionTime s)
        _ -> RevokedSession user
    where user = sessionUser s

-- | Login of the request, from the session cookie. The groups are those of
-- the account in memory (read again at each login and reload), and the
-- session of a removed account, or of a changed password, is closed.
requestLogin :: ActM (Maybe Login)
requestLogin = do
  tokenM <- getKiwiCookie
  sess <- getSessions
  let found = do token <- tokenM
                 let key = hashToken token
                 session <- M.lookup key sess
                 return (key, session)
  case found of
    Nothing -> return Nothing
    Just (key, session) -> do
      accounts' <- liftIO . readIORef =<< asksK accounts
      now <- liftIO Time.getCurrentTime
      case sessionStatus accounts' now (Just session) of
        NoSession -> return Nothing
        ActiveSession l -> return (Just l)
        RevokedSession user -> do
          modifySessions (M.delete key)
          deleteKiwiCookie
          logger' <- asksK logger
          liftIO $ logger' Log.Info $
            "session closed, the account no longer exists or its password has changed: " <> user
          return Nothing

-- | Load sessions from file. Expired sessions, and those with an unreadable
-- login time, are dropped.
loadSessions :: FP.FilePath -> IO Sessions
loadSessions p = do
  fcM <- getFileContent p
  td <- Time.getCurrentTime
  return $ M.filter (isSessionValid td) $ maybe M.empty parseSessions fcM

-- | One session per line: token hash:user:salt fingerprint:login time
-- (seconds since the epoch). Lines of former formats (token:user:groups:time)
-- are ignored.
parseSessions :: T.Text -> Sessions
parseSessions = M.fromList . mapMaybe readSession . T.lines
  where
    readSession l = case T.splitOn ":" l of
      [key, user, saltFp, time]
        | isHex 64 key && not (T.null user) && isHex 16 saltFp -> do
            time' <- Time.parseTimeM False Time.defaultTimeLocale "%s" (T.unpack time)
            Just (key, Session user saltFp time')
      _ -> Nothing
    -- as made by 'hashToken': lowercase hex digits
    isHex n k = T.length k == n && T.all (`elem` ("0123456789abcdef" :: String)) k

renderSessions :: Sessions -> T.Text
renderSessions = T.unlines . map renderSession . M.toList
  where renderSession (key, s) =
          T.intercalate ":" [ key, sessionUser s, sessionSaltFp s
                            , T.pack $ Time.formatTime Time.defaultTimeLocale "%s" (sessionTime s) ]

-- | Save sessions to file (see 'parseSessions'), with a warning if its
-- owner had to change (see 'writePrivateFile')
dumpSessions :: Sessions -> FP.FilePath -> IO (Maybe String)
dumpSessions sessions' p = writePrivateFile p $ renderSessions sessions'


--------------------------------------------------------------------------------
-- Cookies

-- | Name of the session cookie: kiwi-NAME-PORT, with the alphanumeric
-- characters of the name only. Cookies do not depend on the port, so that
-- two kiwis of the same host need different names.
cookieName :: T.Text -> Int -> T.Text
cookieName name port' = T.concat ["kiwi-", T.filter isAlphaNum name, "-", T.pack (show port')]

getCookieName :: ActM T.Text
getCookieName = cookieName <$> asksK kiwiName <*> asksK kiwiPort

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


--------------------------------------------------------------------------------
-- Accounts and passwords

-- | The account, if the password is right. A hash is computed even for an
-- unknown login (with a dummy account), so that the response time does not
-- tell whether the account exists. A login or password longer than
-- 'maxCredentialBytes' fails at once, without any hash.
verifyLogin :: [Account] -> T.Text -> T.Text -> Maybe Account
verifyLogin accounts' u p
  | credentialsTooLong u p = Nothing
  | otherwise =
  let accountM = L.find (\acc -> accountId acc == u) accounts'
      a = fromMaybe dummyAccount accountM
      -- evaluated first, whether the account exists or not
      passwordOk = hashPassword (accountSalt a) p == Just (accountHash a)
  in if passwordOk && isJust accountM then accountM else Nothing
  where
    -- no Argon2 hash is made of zeros only
    dummyAccount = Account "" "kiwi-dummy-salt0" (T.replicate 64 "0") []

-- | 'verifyLogin', with at most as many hashes computed at the same time as
-- the semaphore allows (each one takes 19 MiB of memory)
checkLogin :: QSem -> [Account] -> T.Text -> T.Text -> IO (Maybe Account)
checkLogin sem accounts' u p
  | credentialsTooLong u p = return Nothing
  | otherwise =
      bracket_ (waitQSem sem) (signalQSem sem) $ evaluate $ verifyLogin accounts' u p

-- | Maximum size of a login or a password, in UTF-8 bytes
maxCredentialBytes :: Int
maxCredentialBytes = 1024

credentialsTooLong :: T.Text -> T.Text -> Bool
credentialsTooLong u p = any ((> maxCredentialBytes) . BS.length . TE.encodeUtf8) [u, p]

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

-- | Whether the user of the request is an admin
isAdmin :: ActM Bool
isAdmin = loginIsAdmin <$> asksK login

-- | Whether the user is logged in, and in the "admin" group
loginIsAdmin :: Maybe Login -> Bool
loginIsAdmin loginM = case loginM of
  Nothing -> False
  Just (_,g,_) -> elem "admin" g

-- | The pages the user can read, archived or not: "public" ones for anonymous
-- users, those of their groups too once logged in, all of them for admins
readableQuery :: Maybe Login -> Bool -> DI.Query MetaField DocIndexKey
readableQuery loginM archive
  | loginIsAdmin loginM = world
  | otherwise = case loginM of
      Nothing        -> readable []
      Just (_, g, _) -> readable g
  where
    world = U.qOnly FieldArchive archive
    readable groups = DI.And [U.qAny FieldAccess ("public" : groups :: [T.Text]), world]


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


loadAccounts :: FP.FilePath -> IO [Account]
loadAccounts p = maybe [] parseAccounts <$> getFileContent p

-- | Read the accounts file again into the server state (at each login and
-- reload). Reloads are serialized, so that an older read can not replace a
-- newer one. On a read error, the accounts are kept as they are, and the
-- error is logged.
reloadAccounts :: ActM ()
reloadAccounts = do
  accR <- asksK accounts
  lock <- asksK accountsLock
  kd <- asksK kiwiDir
  logger' <- asksK logger
  let p = FP.combine kd "_accounts"
  liftIO $ withMVar lock $ \_ -> do
    r <- try $ loadAccounts p
    case r of
      Right accs -> atomicWriteIORef accR accs
      Left e -> logger' Log.Error $ T.pack $
                "unable to read " ++ p ++ ", the accounts are kept as they were: "
                ++ show (e :: IOException)

-- | One account per line: login:salt:hash:groups (comma separated).
-- Unreadable lines are ignored.
parseAccounts :: T.Text -> [Account]
parseAccounts = mapMaybe readAccount . T.lines
  where
    readAccount l = case T.splitOn ":" l of
      [lg, salt, hash, grps] | not (T.null lg) -> Just $
        Account lg salt hash (filter (not . T.null) $ T.splitOn "," grps)
      _ -> Nothing

renderAccounts :: [Account] -> T.Text
renderAccounts = T.unlines . map renderAccount
  where renderAccount a = T.intercalate ":" [ accountId a, accountSalt a, accountHash a,
                                              T.intercalate "," $ accountGroups a ]

-- | Save accounts to file, with a warning if its owner had to change (see
-- 'writePrivateFile')
dumpAccounts :: [Account] -> FP.FilePath -> IO (Maybe String)
dumpAccounts accounts' p = writePrivateFile p $ renderAccounts accounts'

-- | A login is made of ASCII letters, digits, '-' and '_'
validateLogin :: T.Text -> Either String T.Text
validateLogin l
  | T.null l = Left "the login can not be empty"
  | T.all allowed l = Right l
  | otherwise = Left "invalid login, only letters, digits, '-' and '_' are allowed"
  where allowed c = isAscii c && (isAlphaNum c || c == '-' || c == '_')

-- | Comma separated groups, at least one, without ':' (the separator of the
-- accounts file)
parseGroups :: T.Text -> Either String [T.Text]
parseGroups t =
  let groups = map T.strip $ T.splitOn "," t
  in case () of
    _ | all T.null groups -> Left "at least one group is needed"
      | any T.null groups -> Left "a group name can not be empty"
      | any (T.any (== ':')) groups -> Left "a group name can not contain ':'"
      | any (T.any (not . isPrint)) groups -> Left "a group name can not contain control characters"
      | otherwise -> Right groups


-- | Write a file atomically, only readable by its owner: the content goes
-- to a temporary file of the same folder, created with mode 0600 (by
-- 'openTempFile'), which then replaces the file. The new file keeps the
-- owner and group of the former one when possible (kiwi access run with
-- sudo); otherwise, if its owner changes, a warning is returned.
writePrivateFile :: FP.FilePath -> T.Text -> IO (Maybe String)
writePrivateFile p content = do
  oldOwnerM <- either (\e -> const Nothing (e :: IOException))
                      (\st -> Just (PF.fileOwner st, PF.fileGroup st))
               <$> try (PF.getFileStatus p)
  bracketOnError (openTempFile dir (FP.takeFileName p ++ ".tmp"))
                 (\(tmp, h) -> hClose h >> void (try (D.removeFile tmp) :: IO (Either IOException ())))
                 (\(tmp, h) -> do
                     hSetEncoding h utf8
                     TSIO.hPutStr h content
                     hClose h
                     warningM <- traverse (keepOwner tmp) oldOwnerM
                     D.renameFile tmp p
                     return $ join warningM)
  where
    dir = FP.takeDirectory p
    keepOwner tmp (u, g) = do
      st <- PF.getFileStatus tmp
      if (PF.fileOwner st, PF.fileGroup st) == (u, g) then return Nothing
      else do
        r <- try $ PF.setOwnerAndGroup tmp u g
        return $ case r of
          Right () -> Nothing
          Left e -> const (ownerWarning p u (PF.fileOwner st)) (e :: IOException)

-- | Warning when the owner of a file changes from the first user id to the
-- second one
ownerWarning :: FP.FilePath -> UserID -> UserID -> Maybe String
ownerWarning p old new
  | old == new = Nothing
  | otherwise = Just $ concat
      [ "warning: the owner of ", p, " changes from user id ", show old, " to "
      , show new, ", a kiwi server running as user id ", show old
      , " may no longer be able to read it" ]


--------------------------------------------------------------------------------
-- Logs

-- | Address of the client, without the port, from the show of the WAI
-- remote host ("1.2.3.4:5678", "[::1]:5678"). Kiwi trusts no proxy, so
-- X-Forwarded-For is not read.
clientAddress :: String -> T.Text
clientAddress s =
  let t = T.pack s
      (hostColon, port') = T.breakOnEnd ":" t
  in if not (T.null hostColon) && not (T.null port') && T.all isDigit port'
     then T.dropEnd 1 hostColon
     else t

-- | Text given by the client, safe to write in a log line: without control
-- or other non printable characters (no forged lines), and not too long
sanitizeLogText :: T.Text -> T.Text
sanitizeLogText = T.take 100 . T.filter isPrint
