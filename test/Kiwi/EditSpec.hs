module Kiwi.EditSpec (editSpec) where

import           Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Network.HTTP.Types as Net
import qualified Network.HTTP.Types.URI as URI
import qualified Network.Wai as Wai
import qualified Network.Wai.Test as WT
import qualified System.Directory as D
import qualified System.FilePath as FP
import           Test.Hspec

import           Kiwi.Edit
import           Kiwi.Types
import           TestUtils

editSpec :: Spec
editSpec = do
  let file = "title: T\nuid: x\nprevious-uids: old\ntags: a\naccess: fam:w, public\n\
             \archive: no\n\nHello"

  it "hides the access to the editors with the write right, and the uid to all" $ do
    editorText [] AccessWrite file `shouldBe` Right ("title: T\ntags: a\narchive: no\n\nHello\n", 4)
    editorText [] AccessAdmin file `shouldBe`
      Right ("title: T\ntags: a\naccess: fam:w, public\narchive: no\n\nHello\n", 5)
    -- without metadata, the page starts with a blank line
    editorText [] AccessWrite "\n\nHello" `shouldBe` Right ("\n\nHello\n", 2)

  it "keeps the access of the file when the write right saves, and the uid" $ do
    editedText [] AccessWrite file "title: New\naccess: public\narchive: yes\n\nBye"
      `shouldBe` Right ("title: New\nuid: x\nprevious-uids: old\naccess: fam:w, public\n\
                        \archive: yes\n\nBye\n", ["access"])
    editedText [] AccessAdmin file "title: New\naccess: public\n\nBye"
      `shouldBe` Right ("title: New\nuid: x\nprevious-uids: old\naccess: public\n\nBye\n", [])
    editedText [] AccessAdmin file "title: New\nuid: y\nprevious-uids:\n\nBye"
      `shouldBe` Right ("title: New\nuid: x\nprevious-uids: old\n\nBye\n", ["previous-uids", "uid"])
    editedText [] AccessWrite file "not a header\n\nBye"
      `shouldBe` Left "invalid metadata header, line 1"

  it "splits a file into its header and body as written, and joins them back" $ do
    splitFile "title: T\nuid: x\n\n# A\n\n\nB\n" `shouldBe` ("title: T\nuid: x", "# A\n\n\nB\n")
    -- a header alone, or a body alone
    splitFile "title: T\n" `shouldBe` ("title: T", "")
    splitFile "\n\nHello\n" `shouldBe` ("", "Hello\n")
    uncurry joinFile (splitFile file) `shouldBe` file
    joinFile "title: T" "" `shouldBe` "title: T\n\n"

  it "reads a header sent apart from its body: no blank line inside" $ do
    metaText "\n\ntitle: T\ntags: a\n\n \n" `shouldBe` Right "title: T\ntags: a"
    metaText "title: T\n\ntags: a\n" `shouldSatisfy` either (const True) (const False)
    let hashOf = textHash . fst . splitFile
        change level h sent = metaChange level h sent [] (TE.encodeUtf8 file, file)
    -- the body as written, the protected keys kept
    change AccessWrite (hashOf file) "title: New\naccess: public\n"
      `shouldBe` Right ("title: New\nuid: x\nprevious-uids: old\naccess: fam:w, public\n\nHello", ["access"])
    change AccessWrite "0000" "title: New\n" `shouldBe` Left SaveConflict
    bodyText (textHash "Hello") "Bye" [] (TE.encodeUtf8 file, file)
      `shouldBe` Right ("title: T\nuid: x\nprevious-uids: old\ntags: a\naccess: fam:w, public\n\
                        \archive: no\n\nBye\n", ())
    bodyText (textHash "Bye") "Bye" [] (TE.encodeUtf8 file, file) `shouldBe` Left SaveConflict

  it "edits lines by their numbers in the version read, in one pass" $ do
    let six = "1\n2\n3\n4\n5\n6\n"
        linesOf = fmap (T.lines . fst)
        failsWith m r = either (m `L.isInfixOf`) (const False) r
    -- the numbers are those of the version read, whatever the order
    applyLineEdits [ReplaceLines 2 3 "a", InsertAfter 5 "x\ny"] six
      `shouldBe` Right ("1\na\n4\n5\nx\ny\n6\n", [LineResult (2, 2) (2, 3), LineResult (5, 6) (5, 5)])
    linesOf (applyLineEdits [InsertAfter 5 "x\ny", ReplaceLines 2 3 "a"] six)
      `shouldBe` Right ["1", "a", "4", "5", "x", "y", "6"]
    -- a deletion: an empty new passage, between two lines
    applyLineEdits [ReplaceLines 1 2 ""] six `shouldBe` Right ("3\n4\n5\n6\n", [LineResult (1, 0) (1, 2)])
    -- at the start and at the end
    applyLineEdits [InsertAfter 0 "a", InsertAfter 6 "z"] six
      `shouldBe` Right ("a\n1\n2\n3\n4\n5\n6\nz\n", [LineResult (1, 1) (0, 0), LineResult (8, 8) (6, 6)])
    -- at the same place, in the order of the list; around a replacement
    linesOf (applyLineEdits [ReplaceLines 2 3 "r", InsertAfter 1 "i", InsertAfter 3 "j"] six)
      `shouldBe` Right ["1", "r", "i", "j", "4", "5", "6"]
    linesOf (applyLineEdits [InsertAfter 1 "i", ReplaceLines 2 3 "r"] six)
      `shouldBe` Right ["1", "i", "r", "4", "5", "6"]
    -- moves, down and up
    applyLineEdits [MoveAfter 1 2 4] six `shouldBe` Right ("3\n4\n1\n2\n5\n6\n", [LineResult (3, 4) (1, 2)])
    linesOf (applyLineEdits [MoveAfter 5 6 0, ReplaceLines 3 3 "c"] six)
      `shouldBe` Right ["5", "6", "1", "2", "c", "4"]
    -- a text without line end at its end, a new text without line end
    applyLineEdits [ReplaceLines 2 2 "x"] "a\nb" `shouldBe` Right ("a\nx\n", [LineResult (2, 2) (2, 2)])
    applyLineEdits [InsertAfter 0 "x"] "" `shouldBe` Right ("x\n", [LineResult (1, 1) (0, 0)])
    -- refused: lines out of the text, overlaps, an insertion between two
    -- deleted lines, a move that does not move, an empty insertion
    applyLineEdits [ReplaceLines 0 1 "x"] six `shouldSatisfy` failsWith "edit 1: the lines 0-1 are not lines"
    applyLineEdits [ReplaceLines 3 2 "x"] six `shouldSatisfy` failsWith "edit 1: the lines 3-2"
    applyLineEdits [InsertAfter 1 "x", InsertAfter 7 "x"] six `shouldSatisfy` failsWith "edit 2: no line 7"
    applyLineEdits [ReplaceLines 2 4 "", MoveAfter 4 5 0] six
      `shouldSatisfy` failsWith "edit 2: its lines 4-5 overlap those of edit 1 (2-4)"
    applyLineEdits [ReplaceLines 2 4 "x", InsertAfter 3 "y"] six
      `shouldSatisfy` failsWith "edit 2: it inserts after line 3, between the lines 2-4 of edit 1"
    applyLineEdits [MoveAfter 2 3 1] six `shouldSatisfy` failsWith "would not move"
    applyLineEdits [MoveAfter 2 3 3] six `shouldSatisfy` failsWith "would not move"
    applyLineEdits [InsertAfter 2 ""] six `shouldSatisfy` failsWith "the new text is empty"
    -- with the hash of the body read, the header kept
    linesChange (textHash "Hello") [InsertAfter 1 "World"] [] (TE.encodeUtf8 file, file)
      `shouldBe` Right ( "title: T\nuid: x\nprevious-uids: old\ntags: a\naccess: fam:w, public\n\
                         \archive: no\n\nHello\nWorld\n", [LineResult (2, 2) (1, 1)] )
    linesChange "0000" [InsertAfter 1 "World"] [] (TE.encodeUtf8 file, file) `shouldBe` Left SaveConflict

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
      -- the git repository of the source s
      repoOf cd = cd FP.</> "s"
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
    liftIO $ body editor `shouldSatisfy` T.isInfixOf "name=\"message\""
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
          git (repoOf cd) ["add", "."]
          git (repoOf cd) ["commit", "-q", "-m", "init"]
          WT.runSession (test cd) app

  it "commits a saved page in its git repository" $ onRepo [] $ \cd -> do
    _ <- login "bob"
    h <- hashOf cd "p.md"
    saved <- save h "title: New\n\nBye"
    liftIO $ status saved `shouldBe` 200
    liftIO $ gitOut (repoOf cd) ["log", "-1", "--format=%an <%ae>|%cn <%ce>|%s"]
      `shouldReturn` "bob via Kiwi <>|Kiwi <>|Edit s:p (kiwi kiwi)\n"
    -- no pre-update commit: the file was not changed outside of kiwi
    liftIO $ gitOut (repoOf cd) ["rev-list", "--count", "HEAD"] `shouldReturn` "2\n"
    liftIO $ gitOut (repoOf cd) ["status", "--porcelain"] `shouldReturn` ""

  let logOf cd n = gitOut (repoOf cd) ["log", "-" ++ show (n :: Int), "--format=%an <%ae>|%cn <%ce>|%s"]
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
    liftIO $ gitOut (repoOf cd) ["show", "HEAD~1:p.md"] `shouldReturn` outside
    liftIO $ gitOut (repoOf cd) ["status", "--porcelain"] `shouldReturn` ""

  it "first commits a page file that is not tracked yet, as Kiwi" $ onRepo [] $ \cd -> do
    _ <- login "bob"
    liftIO $ git (repoOf cd) ["rm", "-q", "--cached", "p.md"]
    liftIO $ git (repoOf cd) ["commit", "-q", "-m", "untrack"]
    h <- hashOf cd "p.md"
    saved <- save h "title: New\n\nBye"
    liftIO $ status saved `shouldBe` 200
    liftIO $ logOf cd 2 `shouldReturn` saveCommits

  it "adds the message of the editor to the commit" $ onRepo [] $ \cd -> do
    _ <- login "bob"
    h <- hashOf cd "p.md"
    saved <- post "/save-page" [ ("source", "s"), ("id", "p"), ("hash", h)
                               , ("message", "Typo fixed"), ("text", "title: New\n\nBye") ]
    liftIO $ status saved `shouldBe` 200
    liftIO $ gitOut (repoOf cd) ["log", "-1", "--format=%B"]
      `shouldReturn` "Edit s:p (kiwi kiwi)\n\nTypo fixed\n\n"
    -- an empty message adds nothing
    h' <- hashOf cd "p.md"
    again <- post "/save-page" [ ("source", "s"), ("id", "p"), ("hash", h')
                               , ("message", " "), ("text", "title: New\n\nBye again") ]
    liftIO $ status again `shouldBe` 200
    liftIO $ gitOut (repoOf cd) ["log", "-1", "--format=%B"] `shouldReturn` "Edit s:p (kiwi kiwi)\n\n"

  it "writes nothing in a read-only source" $
    onRepo ["[source s]", "read-only: yes"] $ \cd -> do
      _ <- login "bob"
      page <- body <$> get "/page/s/p"
      liftIO $ page `shouldSatisfy` (not . T.isInfixOf "/edit/s/p")
      editor <- get "/edit/s/p"
      liftIO $ status editor `shouldBe` 403
      h <- hashOf cd "p.md"
      saved <- save h "title: New\n\nBye"
      liftIO $ status saved `shouldBe` 403
      creation <- get "/new/s"
      liftIO $ status creation `shouldBe` 403
      liftIO $ gitOut (repoOf cd) ["rev-list", "--count", "HEAD"] `shouldReturn` "1\n"
      liftIO $ gitOut (repoOf cd) ["status", "--porcelain"] `shouldReturn` ""

  it "writes nothing in a source that is not the top of a git repository" $
    withKiwiConfig [] [("ann", "secret", ["admin"]), ("bob", "secret", ["family"])] pages $ \cd _ app -> do
      D.removeDirectoryRecursive (repoOf cd FP.</> ".git")
      flip WT.runSession app $ do
        -- the mode of the sources is read at each load
        _ <- login "bob"
        h <- hashOf cd "p.md"
        unclassified <- save h "title: New\n\nBye"
        liftIO $ status unclassified `shouldBe` 500
        _ <- login "ann"
        reloaded <- post "/reload" []
        liftIO $ status reloaded `shouldBe` 200
        _ <- login "bob"
        refused <- save h "title: New\n\nBye"
        liftIO $ status refused `shouldBe` 403
        content <- liftIO $ readFile (pagePath cd "p.md")
        liftIO $ content `shouldSatisfy` (not . L.isInfixOf "Bye")

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

  -- creation of pages: bob (family) can write in the source s
  let sourceLines = ["[source s]", "access: family:w, public"]
      onNewKiwi test = withKiwiConfig sourceLines
                         [ ("ann", "secret", ["admin"]), ("bob", "secret", ["family"])
                         , ("joe", "secret", ["friends"]) ] pages $
                         \cd _ app -> WT.runSession (test cd) app
      create params = post "/create-page" (("source", "s") : params)
      -- the uid of the page of an URL /page/s/UID
      uidOf = T.drop (T.length "/page/s/")

  it "gives the form of a new page to the users who can write in the source" $ onNewKiwi $ \_ -> do
    anonymous <- get "/new/s"
    liftIO $ status anonymous `shouldBe` 401
    anonymousSources <- get "/new"
    liftIO $ status anonymousSources `shouldBe` 401
    _ <- login "joe"
    reader <- get "/new/s"
    liftIO $ status reader `shouldBe` 403
    readerPage <- body <$> get "/page/s/p"
    liftIO $ readerPage `shouldSatisfy` (not . T.isInfixOf "new-page")
    _ <- login "bob"
    form <- get "/new/s"
    liftIO $ status form `shouldBe` 200
    liftIO $ body form `shouldSatisfy` T.isInfixOf "action=\"/create-page\""
    liftIO $ body form `shouldSatisfy` T.isInfixOf "title: \n\n</textarea>"
    liftIO $ body form `shouldSatisfy` (not . T.isInfixOf "name=\"path\"")
    unknown <- get "/new/none"
    liftIO $ status unknown `shouldBe` 404
    sources <- get "/new"
    liftIO $ body sources `shouldSatisfy` T.isInfixOf "href=\"/new/s\""
    -- the button of the navigation bar goes to the form of the source of the page
    page <- body <$> get "/page/s/p"
    liftIO $ page `shouldSatisfy` T.isInfixOf "href=\"/new/s\" class=\"icon new-page\""
    _ <- login "ann"
    adminForm <- get "/new/s"
    liftIO $ body adminForm `shouldSatisfy` T.isInfixOf "name=\"path\""

  it "creates a page named after its title, and reloads the pages" $ onNewKiwi $ \cd -> do
    _ <- login "bob"
    created <- create [("text", "title: Hello world\n\nBody"), ("path", "")]
    liftIO $ status created `shouldBe` 200
    let url = body created
    liftIO $ url `shouldSatisfy` T.isPrefixOf "/page/s/"
    content <- liftIO $ BS.readFile $ pagePath cd "Hello world.md"
    liftIO $ content `shouldBe` TE.encodeUtf8 ("title: Hello world\nuid: " <> uidOf url <> "\n\nBody\n")
    page <- get (TE.encodeUtf8 url)
    liftIO $ status page `shouldBe` 200
    liftIO $ body page `shouldSatisfy` T.isInfixOf "Body"
    -- a title is needed
    untitled <- create [("text", "tags: a\n\nBody")]
    liftIO $ status untitled `shouldBe` 400
    blank <- create [("text", "title:  \n\nBody")]
    liftIO $ status blank `shouldBe` 400
    -- only the admins choose the path
    withPath <- create [("text", "title: T\n\nBody"), ("path", "sub/t")]
    liftIO $ status withPath `shouldBe` 403
    _ <- login "joe"
    reader <- create [("text", "title: T\n\nBody")]
    liftIO $ status reader `shouldBe` 403
    _ <- login "ann"
    admin <- create [("text", "title: T\n\nBody"), ("path", "sub/t")]
    liftIO $ status admin `shouldBe` 200
    adminContent <- liftIO $ BS.readFile $ pagePath cd ("sub" FP.</> "t.md")
    liftIO $ adminContent `shouldBe` TE.encodeUtf8 ("title: T\nuid: " <> uidOf (body admin) <> "\n\nBody\n")

  it "refuses the creation requests from other sites" $ onNewKiwi $ \_ -> do
    _ <- login "bob"
    other <- request "POST" [("Origin", "https://evil.example"), ("Host", "kiwi.example")]
                     "/create-page" (encode [("source", "s"), ("text", "title: X\n\nX")])
    liftIO $ status other `shouldBe` 403

  it "previews the content of a new page, in its source" $ onNewKiwi $ \_ -> do
    _ <- login "bob"
    preview <- post "/preview" [ ("source", "s"), ("id", "")
                               , ("text", "title: T\n\n[o](page:q) [x](<page: none>)") ]
    liftIO $ status preview `shouldBe` 200
    liftIO $ body preview `shouldSatisfy`
      T.isInfixOf "<a href=\"/page/s/none\" class=\"kiwi-link-page kiwi-link-broken\">x</a>"
    _ <- login "joe"
    reader <- post "/preview" [("source", "s"), ("id", ""), ("text", "title: T\n\nx")]
    liftIO $ status reader `shouldBe` 403

  it "commits a created page, with the message of the form" $ withGit $
    withKiwiConfig sourceLines [("bob", "secret", ["family"])] pages $ \cd _ app -> do
      git (repoOf cd) ["add", "."]
      git (repoOf cd) ["commit", "-q", "-m", "init"]
      flip WT.runSession app $ do
        _ <- login "bob"
        created <- create [("text", "title: New\n\nBody"), ("message", "From the web")]
        liftIO $ status created `shouldBe` 200
        liftIO $ gitOut (repoOf cd) ["log", "-1", "--format=%an|%B"] `shouldReturn`
          T.unpack ("bob via Kiwi|Create s:" <> uidOf (body created) <> " (kiwi kiwi)\n\nFrom the web\n\n")
        liftIO $ gitOut (repoOf cd) ["status", "--porcelain"] `shouldReturn` ""

  -- deletion of pages: carl has the admin right on the pages d and e, bob the
  -- write right; d links two files, pic only by it, doc also by e
  let delPages = [ ("d.md", "title: Doomed\nuid: d\naccess: family:w, editors:a, public\n\n\
                            \![p](file:ABCDEFGH12) [doc](file:ABCDEFGH13)")
                 , ("e.md", "title: Linking\nuid: e\naccess: editors:a, public\n\n\
                            \[d](page:d) [doc](file:ABCDEFGH13)")
                 , ("pic__ABCDEFGH12.png", "png")
                 , ("doc__ABCDEFGH13.txt", "doc") ]
      delAccounts = [ ("ann", "secret", ["admin"]), ("bob", "secret", ["family"])
                    , ("carl", "secret", ["editors"]) ]
      onDelKiwi kiwiLines test = withGit $
        withKiwiConfig kiwiLines delAccounts delPages $ \cd _ app -> do
          git (repoOf cd) ["add", "."]
          git (repoOf cd) ["commit", "-q", "-m", "init"]
          WT.runSession (test cd) app
      deletion h params = post "/delete-page" ([("source", "s"), ("id", "d"), ("hash", h)] ++ params)
      exists cd name = liftIO $ D.doesFileExist (pagePath cd name)

  it "asks the users with the admin right on a page to confirm its deletion" $
    onDelKiwi [] $ \_ -> do
      anonymous <- get "/delete/s/d"
      liftIO $ status anonymous `shouldBe` 401
      _ <- login "bob"
      writer <- get "/delete/s/d"
      liftIO $ status writer `shouldBe` 403
      writerEditor <- body <$> get "/edit/s/d"
      liftIO $ writerEditor `shouldSatisfy` (not . T.isInfixOf "/delete/s/d")
      _ <- login "carl"
      editor <- body <$> get "/edit/s/d"
      liftIO $ editor `shouldSatisfy` T.isInfixOf "href=\"/delete/s/d\""
      confirm <- get "/delete/s/d"
      liftIO $ status confirm `shouldBe` 200
      let page = body confirm
      -- the page linking to it, the file only it links (by its name), the box
      liftIO $ page `shouldSatisfy` T.isInfixOf "href=\"/page/s/e\">Linking</a>"
      liftIO $ page `shouldSatisfy` T.isInfixOf "<code>pic.png</code>"
      liftIO $ page `shouldSatisfy` (not . T.isInfixOf "doc.txt")
      liftIO $ page `shouldSatisfy` T.isInfixOf "name=\"delete-files\""
      liftIO $ page `shouldSatisfy` T.isInfixOf "value=\"ABCDEFGH12\""
      liftIO $ page `shouldSatisfy` T.isInfixOf "action=\"/delete-page\""
      -- the paths for the admins only
      liftIO $ page `shouldSatisfy` (not . T.isInfixOf "s/d.md")
      _ <- login "ann"
      adminPage <- body <$> get "/delete/s/d"
      liftIO $ adminPage `shouldSatisfy` T.isInfixOf "<code>s/d.md</code>"
      liftIO $ adminPage `shouldSatisfy` T.isInfixOf "<code>pic__ABCDEFGH12.png</code>"

  it "deletes a page and the files only it links, in git, then goes home" $
    onDelKiwi [] $ \cd -> do
      _ <- login "carl"
      liftIO $ appendFile (pagePath cd "d.md") "\n\nOutside"
      h <- hashOf cd "d.md"
      deleted <- deletion h [ ("message", "Obsolete"), ("delete-files", "yes")
                            , ("file", "ABCDEFGH12") ]
      liftIO $ status deleted `shouldBe` 303
      liftIO $ lookup "Location" (WT.simpleHeaders deleted) `shouldBe` Just "/"
      exists cd "d.md" >>= liftIO . (`shouldBe` False)
      exists cd "pic__ABCDEFGH12.png" >>= liftIO . (`shouldBe` False)
      exists cd "doc__ABCDEFGH13.txt" >>= liftIO . (`shouldBe` True)
      liftIO $ gitOut (repoOf cd) ["log", "-2", "--format=%an|%B"] `shouldReturn`
        "carl via Kiwi|Delete s:d Doomed (kiwi kiwi)\n\nObsolete\n\n\
        \Files deleted:\n- pic__ABCDEFGH12.png\n\n\
        \Kiwi|Pre-delete commit of s:d (kiwi kiwi)\n\n"
      -- the change made outside of kiwi is in the history
      liftIO $ gitOut (repoOf cd) ["show", "HEAD~1:d.md"] >>= (`shouldSatisfy` ("Outside" `L.isInfixOf`))
      liftIO $ gitOut (repoOf cd) ["status", "--porcelain"] `shouldReturn` ""
      gone <- get "/page/s/d"
      liftIO $ status gone `shouldBe` 404

  it "keeps the files of a deleted page unless asked" $ onDelKiwi [] $ \cd -> do
    _ <- login "carl"
    h <- hashOf cd "d.md"
    deleted <- deletion h [("file", "ABCDEFGH12")]
    liftIO $ status deleted `shouldBe` 303
    exists cd "pic__ABCDEFGH12.png" >>= liftIO . (`shouldBe` True)
    liftIO $ gitOut (repoOf cd) ["log", "-1", "--format=%B"] `shouldReturn` "Delete s:d Doomed (kiwi kiwi)\n\n"

  it "refuses a deletion when the page or its files changed" $ onDelKiwi [] $ \cd -> do
    _ <- login "carl"
    conflict <- deletion "0000" []
    liftIO $ status conflict `shouldBe` 409
    liftIO $ body conflict `shouldSatisfy` T.isInfixOf "check again what will be deleted"
    -- the confirmation again, with the hash of the file
    h <- hashOf cd "d.md"
    liftIO $ body conflict `shouldSatisfy` T.isInfixOf ("value=\"" <> h <> "\"")
    -- a file that another page links
    linked <- deletion h [("delete-files", "yes"), ("file", "ABCDEFGH13")]
    liftIO $ status linked `shouldBe` 409
    exists cd "d.md" >>= liftIO . (`shouldBe` True)
    exists cd "doc__ABCDEFGH13.txt" >>= liftIO . (`shouldBe` True)
    other <- request "POST" [("Origin", "https://evil.example"), ("Host", "kiwi.example")]
                     "/delete-page" (encode [("source", "s"), ("id", "d"), ("hash", h)])
    liftIO $ status other `shouldBe` 403
    _ <- login "bob"
    writer <- deletion h []
    liftIO $ status writer `shouldBe` 403
    exists cd "d.md" >>= liftIO . (`shouldBe` True)

  it "deletes no page of a read-only source" $
    onDelKiwi ["[source s]", "read-only: yes"] $ \cd -> do
      _ <- login "carl"
      editor <- get "/edit/s/d"
      liftIO $ status editor `shouldBe` 403
      confirm <- get "/delete/s/d"
      liftIO $ status confirm `shouldBe` 403
      h <- hashOf cd "d.md"
      refused <- deletion h []
      liftIO $ status refused `shouldBe` 403
      exists cd "d.md" >>= liftIO . (`shouldBe` True)

  it "deletes nothing when a file is ignored by git" $ onDelKiwi [] $ \cd -> do
    liftIO $ writeFile (repoOf cd FP.</> ".gitignore") "*.png\n"
    liftIO $ git (repoOf cd) ["rm", "-q", "--cached", "pic__ABCDEFGH12.png"]
    liftIO $ git (repoOf cd) ["add", ".gitignore"]
    liftIO $ git (repoOf cd) ["commit", "-q", "-m", "ignore"]
    _ <- login "carl"
    h <- hashOf cd "d.md"
    -- the commit before the deletion fails
    refused <- deletion h [("delete-files", "yes"), ("file", "ABCDEFGH12")]
    liftIO $ status refused `shouldBe` 500
    exists cd "d.md" >>= liftIO . (`shouldBe` True)
    exists cd "pic__ABCDEFGH12.png" >>= liftIO . (`shouldBe` True)
