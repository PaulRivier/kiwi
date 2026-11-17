module Kiwi.EditSpec (editSpec) where

import           Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Network.HTTP.Types as Net
import qualified Network.HTTP.Types.URI as URI
import qualified Network.Wai as Wai
import qualified Network.Wai.Test as WT
import qualified System.FilePath as FP
import           Test.Hspec

import           Kiwi.Edit
import           Kiwi.Types
import           TestUtils

editSpec :: Spec
editSpec = do
  let file = "title: T\nuid: x\nprevious-uids: old\ntags: a\naccess: fam:w, public\n\
             \archive: no\n\nHello"

  it "hides the admin metadata to the editors with the write right, and the uid to all" $ do
    editorText [] AccessWrite file `shouldBe` Right ("title: T\ntags: a\n\nHello\n", 3)
    editorText [] AccessAdmin file `shouldBe`
      Right ("title: T\ntags: a\naccess: fam:w, public\narchive: no\n\nHello\n", 5)
    -- without metadata, the page starts with a blank line
    editorText [] AccessWrite "\n\nHello" `shouldBe` Right ("\n\nHello\n", 2)

  it "keeps the admin metadata of the file when the write right saves, and the uid" $ do
    editedText [] AccessWrite file "title: New\naccess: public\n\nBye"
      `shouldBe` Right ("title: New\nuid: x\nprevious-uids: old\naccess: fam:w, public\n\
                        \archive: no\n\nBye\n", ["access"])
    editedText [] AccessAdmin file "title: New\naccess: public\n\nBye"
      `shouldBe` Right ("title: New\nuid: x\nprevious-uids: old\naccess: public\n\nBye\n", [])
    editedText [] AccessAdmin file "title: New\nuid: y\nprevious-uids:\n\nBye"
      `shouldBe` Right ("title: New\nuid: x\nprevious-uids: old\n\nBye\n", ["previous-uids", "uid"])
    editedText [] AccessWrite file "not a header\n\nBye"
      `shouldBe` Left "invalid metadata header, line 1"

  it "checks a page to save: same file, valid text" $ do
    let db = mkPagesDB [mkPage ("src", "other") [] [] False 1, mkPage ("src", "x") [] [] False 1]
        bytes = TE.encodeUtf8 file
        check hash sent = checkSave "/k/content" db (sentText AccessAdmin hash sent)
                            "/k/content/src/p.md" (bytes, file)
    check (fileHash bytes) "title: New\n\nBye"
      `shouldBe` Right ("title: New\nuid: x\nprevious-uids: old\n\nBye\n", [])
    check "0000" "title: New\n\nBye" `shouldBe` Left SaveConflict
    check (fileHash bytes) "not a header\n\nBye"
      `shouldBe` Left (SaveInvalid "invalid metadata header, line 1")

  -- requests to a kiwi with accounts: ann (admin), bob (family), joe (friends)
  let pages = [ ("p.md", "title: Page\nuid: p\naccess: family:w, friends, public\n\n\
                        \Hello\n\nWorld")
              , ("q.md", "uid: q\ntitle: Other\naccess: family:w\n\nQ")
              , ("secret.md", "uid: secret\ntitle: Secret\naccess: nobody\n\nS") ]
      onKiwi test = withKiwiState [ ("ann", "secret", ["admin"]), ("bob", "secret", ["family"])
                                  , ("joe", "secret", ["friends"]) ] pages $
                      \cd _ app -> WT.runSession (test cd) app
      request method headers path form = WT.srequest $ WT.SRequest
        (WT.setPath Wai.defaultRequest
           { Wai.requestMethod = method
           , Wai.requestHeaders = ("Content-Type", "application/x-www-form-urlencoded") : headers }
           path)
        form
      get path = request "GET" [] path ""
      post path params = request "POST" [] path (encode params)
      encode params = BL.fromStrict $ URI.renderSimpleQuery False
        [ (BC.pack k, TE.encodeUtf8 v) | (k, v) <- params ]
      login user = post "/login" [("username", user), ("password", "secret")]
      body = TL.toStrict . TLE.decodeUtf8 . WT.simpleBody
      status = Net.statusCode . WT.simpleStatus
      pagePath cd name = cd FP.</> "s" FP.</> name
      hashOf cd name = fileHash <$> liftIO (BS.readFile $ pagePath cd name)
      save h text = post "/save-page" [("source", "s"), ("id", "p"), ("hash", h), ("text", text)]

  it "gives the editor to the users with the write right" $ onKiwi $ \_ -> do
    anonymous <- get "/edit/s/p"
    liftIO $ status anonymous `shouldBe` 401
    _ <- login "joe"
    reader <- get "/edit/s/p"
    liftIO $ status reader `shouldBe` 403
    _ <- login "bob"
    editor <- get "/edit/s/p?line=3"
    liftIO $ status editor `shouldBe` 200
    liftIO $ body editor `shouldSatisfy` T.isInfixOf "title: Page\n\nHello\n\nWorld\n</textarea>"
    -- the line 3 of the content is the line 5 of the text
    liftIO $ body editor `shouldSatisfy` T.isInfixOf "data-line=\"5\""
    liftIO $ body editor `shouldSatisfy` (not . T.isInfixOf "access:")
    liftIO $ lookup "Cache-Control" (WT.simpleHeaders editor) `shouldBe` Just "no-store"
    -- a page that bob can not read is not found
    secret <- get "/edit/s/secret"
    liftIO $ status secret `shouldBe` 404
    _ <- login "ann"
    admin <- get "/edit/s/p"
    liftIO $ body admin `shouldSatisfy` T.isInfixOf "access: family:w, friends, public"

  it "shows the edition links to the users with the write right" $ onKiwi $ \_ -> do
    anonymous <- body <$> get "/page/s/p"
    liftIO $ anonymous `shouldSatisfy` (not . T.isInfixOf "/edit/s/p")
    liftIO $ anonymous `shouldSatisfy` T.isInfixOf "<p data-line=\"1\">Hello</p>"
    _ <- login "bob"
    editor <- body <$> get "/page/s/p"
    liftIO $ editor `shouldSatisfy` T.isInfixOf "href=\"/edit/s/p\" class=\"icon edit-page\""
    liftIO $ editor `shouldSatisfy` T.isInfixOf "data-edit-url=\"/edit/s/p\""

  it "saves a page, with the admin metadata of its file, and reloads it" $ onKiwi $ \cd -> do
    _ <- login "bob"
    h <- hashOf cd "p.md"
    saved <- save h "title: New\naccess: public\n\nBye"
    liftIO $ status saved `shouldBe` 200
    liftIO $ body saved `shouldBe` "/page/s/p"
    liftIO $ lookup "Kiwi-Ignored-Keys" (WT.simpleHeaders saved) `shouldBe` Just "access"
    content <- liftIO $ BS.readFile $ pagePath cd "p.md"
    liftIO $ content `shouldBe` "title: New\nuid: p\naccess: family:w, friends, public\n\nBye\n"
    page <- body <$> get "/page/s/p"
    liftIO $ page `shouldSatisfy` T.isInfixOf "Bye"
    -- the file has changed since the hash was read
    conflict <- save h "title: Again\n\nBye"
    liftIO $ status conflict `shouldBe` 409
    h' <- hashOf cd "p.md"
    invalid <- save h' "not a header\n\nBye"
    liftIO $ status invalid `shouldBe` 400
    liftIO $ body invalid `shouldBe` "invalid metadata header, line 1"

  it "keeps the uid of the page, even when an admin sends another one" $ onKiwi $ \cd -> do
    _ <- login "ann"
    h <- hashOf cd "p.md"
    moved <- save h "title: Page\nuid: q\naccess: public\n\nHello"
    liftIO $ status moved `shouldBe` 200
    liftIO $ body moved `shouldBe` "/page/s/p"
    liftIO $ lookup "Kiwi-Ignored-Keys" (WT.simpleHeaders moved) `shouldBe` Just "uid"
    content <- liftIO $ BS.readFile $ pagePath cd "p.md"
    liftIO $ content `shouldBe` "title: Page\nuid: p\naccess: public\n\nHello\n"

  it "saves the text sent with windows line endings" $ onKiwi $ \cd -> do
    _ <- login "ann"
    h <- hashOf cd "p.md"
    saved <- save h "title: Page\r\naccess: public\r\n\r\nHello\r\nAgain"
    liftIO $ status saved `shouldBe` 200
    content <- liftIO $ BS.readFile $ pagePath cd "p.md"
    liftIO $ content `shouldBe` "title: Page\nuid: p\naccess: public\n\nHello\nAgain\n"

  it "refuses the edition requests from other sites" $ onKiwi $ \cd -> do
    _ <- login "bob"
    h <- hashOf cd "p.md"
    let params = encode [("source", "s"), ("id", "p"), ("hash", h), ("text", "title: X\n\nX")]
    other <- request "POST" [("Origin", "https://evil.example"), ("Host", "kiwi.example")]
                     "/save-page" params
    liftIO $ status other `shouldBe` 403
    same <- request "POST" [("Origin", "https://kiwi.example"), ("Host", "kiwi.example")]
                    "/save-page" params
    liftIO $ status same `shouldBe` 200

  let onRepo kiwiLines test = withGit $
        withKiwiConfig kiwiLines [("bob", "secret", ["family"])] pages $ \cd _ app -> do
          git cd ["init", "-q"]
          git cd ["add", "."]
          git cd ["commit", "-q", "-m", "init"]
          WT.runSession (test cd) app

  it "commits a saved page in its git repository" $ onRepo [] $ \cd -> do
    _ <- login "bob"
    h <- hashOf cd "p.md"
    saved <- save h "title: New\n\nBye"
    liftIO $ status saved `shouldBe` 200
    liftIO $ gitOut cd ["log", "-1", "--format=%an <%ae>|%cn <%ce>|%s"]
      `shouldReturn` "bob via Kiwi <>|Kiwi <>|Edit s:p (kiwi kiwi)\n"
    -- no pre-update commit: the file was not changed outside of kiwi
    liftIO $ gitOut cd ["rev-list", "--count", "HEAD"] `shouldReturn` "2\n"
    liftIO $ gitOut cd ["status", "--porcelain"] `shouldReturn` ""

  let logOf cd n = gitOut cd ["log", "-" ++ show (n :: Int), "--format=%an <%ae>|%cn <%ce>|%s"]
      saveCommits = "bob via Kiwi <>|Kiwi <>|Edit s:p (kiwi kiwi)\n\
                    \Kiwi <>|Kiwi <>|Pre-update commit of s:p (kiwi kiwi)\n"

  it "first commits the changes made outside of kiwi, as Kiwi" $ onRepo [] $ \cd -> do
    _ <- login "bob"
    liftIO $ appendFile (pagePath cd "p.md") "\n\nOutside"
    outside <- liftIO $ readFile (pagePath cd "p.md")
    h <- hashOf cd "p.md"
    saved <- save h "title: New\n\nBye"
    liftIO $ status saved `shouldBe` 200
    liftIO $ logOf cd 3 `shouldReturn` (saveCommits ++ "test <test@test>|test <test@test>|init\n")
    liftIO $ gitOut cd ["show", "HEAD~1:s/p.md"] `shouldReturn` outside
    liftIO $ gitOut cd ["status", "--porcelain"] `shouldReturn` ""

  it "first commits a page file that is not tracked yet, as Kiwi" $ onRepo [] $ \cd -> do
    _ <- login "bob"
    liftIO $ git cd ["rm", "-q", "--cached", "s/p.md"]
    liftIO $ git cd ["commit", "-q", "-m", "untrack"]
    h <- hashOf cd "p.md"
    saved <- save h "title: New\n\nBye"
    liftIO $ status saved `shouldBe` 200
    liftIO $ logOf cd 2 `shouldReturn` saveCommits

  it "does not commit a saved page with git-commit: no" $ onRepo ["git-commit: no"] $ \cd -> do
    _ <- login "bob"
    h <- hashOf cd "p.md"
    saved <- save h "title: New\n\nBye"
    liftIO $ status saved `shouldBe` 200
    liftIO $ gitOut cd ["rev-list", "--count", "HEAD"] `shouldReturn` "1\n"
    liftIO $ gitOut cd ["status", "--porcelain"] `shouldReturn` " M s/p.md\n"

  it "previews the content of a text, relative to its page" $ onKiwi $ \_ -> do
    _ <- login "bob"
    preview <- post "/preview" [ ("source", "s"), ("id", "p")
                               , ("text", "title: T\n\n*Hi* [o](page:q) [x](<page: none>)") ]
    liftIO $ status preview `shouldBe` 200
    liftIO $ body preview `shouldSatisfy` T.isInfixOf "<em>Hi</em>"
    liftIO $ body preview `shouldSatisfy`
      T.isInfixOf "<a href=\"/page/s/none\" class=\"kiwi-link-page kiwi-link-broken\">x</a>"
    invalid <- post "/preview" [("source", "s"), ("id", "p"), ("text", "not a header\n\nx")]
    liftIO $ status invalid `shouldBe` 400
