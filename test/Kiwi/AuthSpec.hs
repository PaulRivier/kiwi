module Kiwi.AuthSpec (passwordSpec, accountsSpec, sessionsSpec, readableQuerySpec) where

import           Control.Concurrent.QSem (newQSem)
import           Data.Either (isLeft)
import           Data.Char (isAscii, isAlphaNum)
import           Data.List (isInfixOf, sort)
import qualified Data.Map as M
import           Data.Maybe (fromMaybe)
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Time as Time
import qualified System.Directory as D
import qualified System.FilePath as FP
import           Test.Hspec

import qualified Kiwi.Auth as Auth
import           Kiwi.Auth (readableQuery)
import           Kiwi.Types
import qualified Utils.DocIndex as DI
import           TestUtils

passwordSpec :: Spec
passwordSpec = do
  let salt = "0123456789abcdef"
      hash = fromMaybe (error "hashPassword failed") $ Auth.hashPassword salt "secret"
      paul = Account "paul" salt hash ["admin", "family"]
      accounts = [paul]

  it "hashes passwords deterministically, as hex" $ do
    Auth.hashPassword salt "secret" `shouldBe` Just hash
    T.length hash `shouldBe` 64
    Auth.hashPassword salt "other" `shouldNotBe` Just hash
    Auth.hashPassword "fedcba9876543210" "secret" `shouldNotBe` Just hash

  it "refuses salts shorter than 8 bytes" $
    Auth.hashPassword "short" "secret" `shouldBe` Nothing

  it "verifies logins" $ do
    Auth.verifyLogin accounts "paul" "secret" `shouldBe` Just paul
    Auth.verifyLogin accounts "paul" "wrong" `shouldBe` Nothing
    Auth.verifyLogin accounts "nobody" "secret" `shouldBe` Nothing
    Auth.verifyLogin [] "" "" `shouldBe` Nothing

  it "refuses logins and passwords longer than 1024 bytes" $ do
    let long = T.replicate 1025 "a"
        longAccount = Account long salt (fromMaybe "" $ Auth.hashPassword salt long) []
    Auth.verifyLogin [longAccount] long long `shouldBe` Nothing
    Auth.verifyLogin [paul] "paul" (T.replicate 513 "é") `shouldBe` Nothing
    sem <- newQSem 1
    Auth.checkLogin sem [longAccount] long long `shouldReturn` Nothing

  it "checks logins under the semaphore, and releases it" $ do
    sem <- newQSem 1
    r1 <- Auth.checkLogin sem accounts "paul" "secret"
    r2 <- Auth.checkLogin sem accounts "nobody" "secret"
    r3 <- Auth.checkLogin sem accounts "paul" "secret"
    (r1, r2, r3) `shouldBe` (Just paul, Nothing, Just paul)

  it "generates random alphanumeric texts" $ do
    t0 <- Auth.randomText 0
    t0 `shouldBe` ""
    t1 <- Auth.randomText 128
    t2 <- Auth.randomText 128
    T.length t1 `shouldBe` 128
    T.all (\c -> isAscii c && isAlphaNum c) t1 `shouldBe` True
    t1 `shouldNotBe` t2

accountsSpec :: Spec
accountsSpec = do
  let acc1 = Account "paul" "0123456789abcdef" (T.replicate 64 "a") ["admin", "family"]
      acc2 = Account "zoé-x_1" "fedcba9876543210" (T.replicate 64 "b") ["friends"]

  it "reads the accounts it writes" $
    Auth.parseAccounts (Auth.renderAccounts [acc1, acc2]) `shouldBe` ([acc1, acc2], [])

  it "writes account lines of version 1" $
    Auth.renderAccounts [acc1] `shouldBe`
      "1:paul:0123456789abcdef:" <> T.replicate 64 "a" <> ":admin,family\n"

  it "refuses the account lines without a known version, and gives their numbers" $ do
    Auth.parseAccounts "bad line\n1:paul:salt:hash:a,,b\n\n1::salt:hash:a\n1:x:s:h:g:extra\n\
                       \paul:salt:hash:a\n2:paul:salt:hash:a\n"
      `shouldBe` ([Account "paul" "salt" "hash" ["a", "b"]], [1, 4, 5, 6, 7])
    Auth.refusedLinesWarning "_accounts" [] `shouldBe` Nothing
    Auth.refusedLinesWarning "_accounts" [1, 4] `shouldBe` Just
      "accounts file _accounts: lines 1, 4 ignored, an account line is \
      \1:login:salt:hash:groups"

  it "writes the accounts file, only readable by its owner" $
    withContentDir $ \cd -> do
      let p = FP.takeDirectory cd FP.</> "_accounts"
      Auth.dumpAccounts [acc1] p `shouldReturn` Nothing
      -- same owner: no warning
      Auth.dumpAccounts [acc1, acc2] p `shouldReturn` Nothing
      Auth.loadAccounts p `shouldReturn` ([acc1, acc2], [])
      -- no temporary file left
      D.listDirectory (FP.takeDirectory cd) >>= (`shouldMatchList` ["content", "_accounts"])

  it "warns when the owner of the accounts file would change" $ do
    Auth.ownerWarning "_accounts" 1000 1000 `shouldBe` Nothing
    Auth.ownerWarning "_accounts" 1000 0 `shouldSatisfy` maybe False ("user id 1000" `isInfixOf`)

  it "shows accounts without their salt and hash" $
    show acc1 `shouldBe` "Account \"paul\" <salt> <hash> [\"admin\",\"family\"]"

  it "validates logins" $ do
    Auth.validateLogin "paul-R_2" `shouldBe` Right "paul-R_2"
    mapM_ (\l -> Auth.validateLogin l `shouldSatisfy` isLeft)
      ["", "a b", "a:b", "zoé", "a,b", "a\n"]

  it "validates groups" $ do
    Auth.parseGroups "admin, family ,friends" `shouldBe` Right ["admin", "family", "friends"]
    Auth.parseGroups "family" `shouldBe` Right ["family"]
    mapM_ (\g -> Auth.parseGroups g `shouldSatisfy` isLeft)
      ["", "  ", "a,,b", "a,", "a:b", "a\tb"]

sessionsSpec :: Spec
sessionsSpec = do
  let t0 = Time.UTCTime (Time.fromGregorian 2026 1 1) 0
      now = Time.addUTCTime (10 * Time.nominalDay) t0
      key = Auth.hashToken "token"
      salt = "0123456789abcdef"
      saltFp = "9f9f5111f7b27a78"   -- 16 first hex digits of the SHA-256 of salt
      account groups = Account "paul" salt (T.replicate 64 "a") groups
      session = Auth.newSession (account ["family"]) t0
      status accs = Auth.sessionStatus accs now (Just session)

  it "keys sessions with the hex SHA-256 of the token" $ do
    key `shouldBe` "3c469e9d6c5875d37a43f353d4f88e61fcf812c66eee3457465a40b0da4153e0"
    Auth.hashToken "other" `shouldNotBe` key

  it "keeps a fingerprint of the salt of the account in the session" $
    session `shouldBe` Session "paul" saltFp t0

  it "reads the sessions it writes" $ do
    let s = M.fromList [ (key, session)
                       , (Auth.hashToken "t2", Session "zoé" saltFp (Time.addUTCTime 60 t0)) ]
    Auth.parseSessions (Auth.renderSessions s) `shouldBe` s

  it "writes no token and no group in the sessions" $ do
    let t = Auth.renderSessions $ M.fromList [(key, session)]
    t `shouldBe` T.concat [key, ":paul:", saltFp, ":1767225600\n"]

  it "ignores the lines of former formats, and unreadable ones" $
    Auth.parseSessions (T.unlines
      [ "plaintoken:paul:admin,family:1767225600"   -- first format
      , key <> ":paul:1767225600"                   -- without salt fingerprint
      , "plaintoken:paul:" <> saltFp <> ":1767225600"  -- not a hash
      , T.toUpper key <> ":paul:" <> saltFp <> ":1767225600"  -- not made by hashToken
      , key <> ":paul:admin:1767225600"             -- not a fingerprint
      , key <> "::" <> saltFp <> ":1767225600"
      , key <> ":paul:" <> saltFp <> ":yesterday" ])
      `shouldBe` M.empty

  it "gives the current groups of the account, the password unchanged" $ do
    status [account ["friends"]] `shouldBe` Auth.ActiveSession ("paul", ["friends"], t0)
    status [account ["admin"]] `shouldBe` Auth.ActiveSession ("paul", ["admin"], t0)

  it "revokes the sessions of removed accounts" $
    status [Account "other" salt "" []] `shouldBe` Auth.RevokedSession "paul"

  it "revokes the sessions of a changed password, or of an account created again" $
    -- both make a new salt
    status [Account "paul" "fedcba9876543210" (T.replicate 64 "b") ["family"]]
      `shouldBe` Auth.RevokedSession "paul"

  it "ignores unknown and expired sessions" $ do
    Auth.sessionStatus [account []] now Nothing `shouldBe` Auth.NoSession
    Auth.sessionStatus [account []] (Time.addUTCTime (366 * Time.nominalDay) t0)
      (Just session) `shouldBe` Auth.NoSession
    -- even if the account no longer exists
    Auth.sessionStatus [] (Time.addUTCTime (366 * Time.nominalDay) t0)
      (Just session) `shouldBe` Auth.NoSession

  it "names the cookie after the kiwi and its port" $ do
    Auth.cookieName "My Kiwi!" 3456 `shouldBe` "kiwi-MyKiwi-3456"
    Auth.cookieName "My Kiwi" 3457 `shouldBe` "kiwi-MyKiwi-3457"

  it "logs the client address without its port, and a safe login" $ do
    Auth.clientAddress "192.168.1.2:54321" `shouldBe` "192.168.1.2"
    Auth.clientAddress "[::1]:54321" `shouldBe` "[::1]"
    Auth.clientAddress "<socket>" `shouldBe` "<socket>"
    Auth.sanitizeLogText "paul\nINFO login: admin\r\t\x2028" `shouldBe` "paulINFO login: admin"
    T.length (Auth.sanitizeLogText (T.replicate 1000 "a")) `shouldBe` 100

readableQuerySpec :: Spec
readableQuerySpec = do
  let pages = [ mkPage ("s", "public") [] ["public"] False 1
              , mkPage ("s", "family") [] ["family"] False 1
              , mkPage ("s", "admin") [] ["admin"] False 1
              , mkPage ("s", "both") [] ["family", "friends"] False 1
              , mkPage ("s", "old-public") [] ["public"] True 1
              , mkPage ("s", "old-family") [] ["family"] True 1
              , mkPage ("s", "old-admin") [] ["admin"] True 1 ]
      idx = pagesIndex $ mkPagesDB pages
      t0 = Time.UTCTime (Time.fromGregorian 2026 1 1) 0
      user groups = Just ("user", groups, t0)
      readable loginM archive =
        sort $ map snd $ S.toList $ DI.queryIdSet idx $ readableQuery loginM archive

  it "gives anonymous users the public pages only" $ do
    readable Nothing False `shouldBe` ["public"]
    readable Nothing True `shouldBe` ["old-public"]

  it "gives logged in users the public pages and those of their groups" $ do
    readable (user ["family"]) False `shouldBe` ["both", "family", "public"]
    readable (user ["friends"]) False `shouldBe` ["both", "public"]
    readable (user []) False `shouldBe` ["public"]
    readable (user ["family"]) True `shouldBe` ["old-family", "old-public"]

  it "gives admins every page" $ do
    readable (user ["admin"]) False `shouldBe` ["admin", "both", "family", "public"]
    readable (user ["family", "admin"]) True
      `shouldBe` ["old-admin", "old-family", "old-public"]

  it "separates the archives from the other pages" $
    mapM_ (\loginM -> do
              readable loginM False `shouldSatisfy` all (not . T.isPrefixOf "old-")
              readable loginM True `shouldSatisfy` all (T.isPrefixOf "old-"))
      [Nothing, user ["family"], user ["admin"]]

