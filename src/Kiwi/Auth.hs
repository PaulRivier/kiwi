-- | Accounts, passwords, sessions and the session cookie
module Kiwi.Auth
  ( -- * Sessions
    getSessions
  , modifySessions
  , isSessionValid
  , loadSessions
    -- * Cookies
  , setKiwiCookie
  , getKiwiCookie
  , deleteKiwiCookie
    -- * Accounts and passwords
  , verifyLogin
  , hashPassword
  , isAdmin
  , loginIsAdmin
  , randomText
  , loadAccounts
  , dumpAccounts
  ) where

import           Control.Concurrent.MVar (readMVar, modifyMVar_)
import           Control.Monad.Reader
import qualified Crypto.Error as CE
import qualified Crypto.KDF.Argon2 as Argon2
import qualified Crypto.Random as CR
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import           Data.Char (isAlphaNum)
import qualified Data.List as L
import qualified Data.Map.Strict as M
import           Data.Maybe (mapMaybe)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TSIO
import qualified Data.Time as Time
import qualified System.FilePath as FP
import qualified Web.Scotty.Cookie as CK

import           Kiwi.Types
import           Kiwi.Utils (asksK, getFileContent)


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

-- | Whether the user of the request is an admin
isAdmin :: ActM Bool
isAdmin = loginIsAdmin <$> asksK login

-- | Whether the user is logged in, and in the "admin" group
loginIsAdmin :: Maybe Login -> Bool
loginIsAdmin loginM = case loginM of
  Nothing -> False
  Just (_,g,_) -> elem "admin" g


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

-- | Load sessions from file. Expired sessions, and those with an unreadable
-- login time, are dropped.
loadSessions :: FP.FilePath -> IO Sessions
loadSessions p = do
  fcM <- getFileContent p
  td <- Time.getCurrentTime
  case fcM of
    Nothing -> return M.empty
    Just c -> return $ M.filter (isSessionValid td) $ M.fromList $
              mapMaybe readSession $ T.lines c
  where
    -- sessionId:user:groups:login time (seconds since the epoch)
    readSession l = case T.splitOn ":" l of
      (sId:user:groups:time:[]) -> do
        time' <- Time.parseTimeM False Time.defaultTimeLocale "%s" (T.unpack time)
        Just (sId, (user, T.splitOn "," groups, time'))
      _ -> Nothing

-- | Save sessions to file, one per line (see 'loadSessions')
dumpSessions :: Sessions -> FP.FilePath -> IO ()
dumpSessions sessions' p =
  let c = T.intercalate "\n" $ map dumpSession $ M.toList sessions'
  in TSIO.writeFile p c
  where dumpSession (sId, (user,groups,time)) =
          T.intercalate ":" [ sId, user, T.intercalate "," groups,
                              T.pack $ Time.formatTime Time.defaultTimeLocale "%s" time ]
