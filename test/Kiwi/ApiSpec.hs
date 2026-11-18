module Kiwi.ApiSpec (apiSpec) where

import           Control.Monad.IO.Class (liftIO)
import           Data.IORef (readIORef)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import qualified Data.List as L
import qualified Data.Map.Strict as M
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

import qualified Kiwi.Api.Search as AS
import qualified Kiwi.Auth as Auth
import qualified Kiwi.Edit as Edit
import           Kiwi.Edit (fileHash)
import           Kiwi.Types
import           TestUtils


apiSpec :: Spec
apiSpec = do
  -- a kiwi with accounts: ann (admin), bob (family), and a token for each one
  let pages = [ ("p.md", "title: Page\nuid: p\nsummary: About p\ntags: a > b, c\n\
                         \access: family:w, public\n\n# One\n\nHello\n\n## Two\n\nWorld\n\n\
                         \# Three\n\nEnd [q](page:q)\n")
              , ("sub/q.md", "uid: q\ntitle: Other\ntags: c\naccess: family\nzone: north\n\n\
                             \Q [p](page:p)\n")
              , ("secret.md", "uid: secret\ntitle: Secret\naccess: nobody\n\nS\n") ]
      onKiwi test = withKiwiConfig ["agent-guide: s/q"]
                      [("ann", "secret", ["admin"]), ("bob", "secret", ["family"])] pages $
                      \cd ss app -> do
        accs <- readIORef (accounts ss)
        made <- mapM Auth.newApiToken accs
        _ <- Auth.dumpTokens (M.fromList $ map snd made) (Auth.tokensFile $ kiwiDir ss)
        let tokenOf l = case [ t | (a, (t, _)) <- zip accs made, accountId a == l ] of
              t : _ -> t
              [] -> error $ "no token for " ++ T.unpack l
        WT.runSession (test cd tokenOf) app
      get token path = WT.srequest $ WT.SRequest
        (WT.setPath Wai.defaultRequest
           { Wai.requestHeaders = [("Authorization", "Bearer " <> TE.encodeUtf8 token)] }
           path) ""
      body = TL.toStrict . TLE.decodeUtf8 . WT.simpleBody
      status = Net.statusCode . WT.simpleStatus

  it "needs a known token, or the session of an admin for a GET" $ onKiwi $ \_ _ -> do
    let plain path = WT.request $ WT.setPath Wai.defaultRequest path
        send' method path = WT.request $ WT.setPath Wai.defaultRequest { Wai.requestMethod = method } path
        login who = WT.srequest $ WT.SRequest
          (WT.setPath Wai.defaultRequest
             { Wai.requestMethod = "POST"
             , Wai.requestHeaders = [("Content-Type", "application/x-www-form-urlencoded")] }
             "/login")
          ("username=" <> who <> "&password=secret")
    none <- plain "/api/pages"
    liftIO $ status none `shouldBe` 401
    liftIO $ lookup "WWW-Authenticate" (WT.simpleHeaders none) `shouldBe` Just "Bearer"
    wrong <- get "nope" "/api/pages"
    liftIO $ status wrong `shouldBe` 401
    -- the session of an account that is not admin: no
    _ <- login "bob"
    bob <- plain "/api/pages"
    liftIO $ status bob `shouldBe` 401
    _ <- send' "POST" "/logout"
    -- the session of an admin: the reads, not the writes
    _ <- login "ann"
    ann <- plain "/api/pages"
    liftIO $ status ann `shouldBe` 200
    liftIO $ body ann `shouldSatisfy` T.isInfixOf "s/secret"
    write <- send' "POST" "/api/metadata"
    liftIO $ status write `shouldBe` 401

  it "lists the pages readable by the account, with their tags, summary and fields" $
    onKiwi $ \_ tokenOf -> do
      r <- get (tokenOf "bob") "/api/pages"
      liftIO $ status r `shouldBe` 200
      liftIO $ lookup "Cache-Control" (WT.simpleHeaders r) `shouldBe` Just "no-store"
      let ls = T.lines (body r)
      liftIO $ take 1 ls `shouldBe` ["2 pages, the most recently modified first"]
      liftIO $ ls `shouldSatisfy` any (\l -> "s/p  " `T.isPrefixOf` l &&
                                             "  Page  [a > b, c]" `T.isSuffixOf` l)
      liftIO $ ls `shouldSatisfy` elem "  summary: About p"
      tagged <- get (tokenOf "ann") "/api/pages?tag=a&limit=5"
      liftIO $ take 1 (T.lines $ body tagged) `shouldBe` ["1 page, the most recently modified first"]
      -- no path in the lists, even for the admins
      liftIO $ body tagged `shouldSatisfy` (not . T.isInfixOf "path:")
      -- the values of the fields asked, under the pages that have them
      shown <- get (tokenOf "bob") "/api/pages?has=zone&show=zone"
      liftIO $ case T.lines (body shown) of
        [h, q, z] -> do
          h `shouldBe` "1 page, the most recently modified first"
          q `shouldSatisfy` T.isPrefixOf "s/q  "
          z `shouldBe` "  zone: north"
        other -> expectationFailure (show other)
      unknownField <- get (tokenOf "bob") "/api/pages?show=nope"
      liftIO $ status unknownField `shouldBe` 400
      limited <- get (tokenOf "ann") "/api/pages?limit=1"
      liftIO $ take 1 (T.lines $ body limited)
        `shouldBe` ["3 pages, the most recently modified first, 1 shown (limit)"]
      bad <- get (tokenOf "ann") "/api/pages?limit=x"
      liftIO $ status bad `shouldBe` 400

  it "gives the facets of the pages selected, with the number of pages" $
    onKiwi $ \_ tokenOf -> do
      r <- get (tokenOf "bob") "/api/facets"
      liftIO $ T.lines (body r) `shouldBe`
        [ "source s: 2 pages", "tags (pages):", "  a (1)", "  a > b (1)", "  c (2)"
        , "lang: English (2)", "zone (text; the area): north (1)" ]
      -- narrowed by the filters of /api/pages
      tagged <- get (tokenOf "bob") "/api/facets?tag=c&tag=a"
      liftIO $ T.lines (body tagged) `shouldBe`
        [ "source s: 1 page", "tags (pages):", "  a (1)", "  a > b (1)", "  c (1)"
        , "lang: English (1)" ]
      zoned <- get (tokenOf "bob") "/api/facets?m.zone=north"
      liftIO $ take 1 (T.lines $ body zoned) `shouldBe` ["source s: 1 page"]
      none <- get (tokenOf "bob") "/api/facets?tag=nope"
      liftIO $ body none `shouldBe` "no page\n"
      bad <- get (tokenOf "bob") "/api/facets?x=y"
      liftIO $ status bad `shouldBe` 400

  it "gives the text of a page without its uid, its hash as ETag, and parts of it" $
    onKiwi $ \cd tokenOf -> do
      r <- get (tokenOf "bob") "/api/page/s/p"
      bytes <- liftIO $ BS.readFile (cd FP.</> "s" FP.</> "p.md")
      liftIO $ lookup "ETag" (WT.simpleHeaders r)
        `shouldBe` Just (BC.pack $ "\"" ++ T.unpack (fileHash bytes) ++ "\"")
      -- bob has the write right: access is hidden, as in the editor
      liftIO $ body r `shouldBe` "title: Page\nsummary: About p\ntags: a > b, c\n\n# One\n\n\
                                 \Hello\n\n## Two\n\nWorld\n\n# Three\n\nEnd [q](page:q)\n"
      outline <- get (tokenOf "bob") "/api/page/s/p?part=outline"
      liftIO $ T.lines (body outline) `shouldSatisfy` \ls ->
        L.isSubsequenceOf [ "s/p  Page", "tags: a > b, c", "summary: About p", "lines: 15"
                          , "backlinks: s/q Other", "sections (lines):"
                          , "  5-12  # One", "  9-12  ## Two", "  13-15  # Three" ] ls
      section <- get (tokenOf "bob") "/api/page/s/p?part=section&title=two"
      liftIO $ body section `shouldBe` "## Two\n\nWorld\n\n"
      liftIO $ lookup "Kiwi-Lines" (WT.simpleHeaders section) `shouldBe` Just "9-12"
      numbered <- get (tokenOf "bob") "/api/page/s/p?part=lines&from=13&to=20&numbered=yes"
      liftIO $ body numbered `shouldBe` "    13\t# Three\n    14\t\n    15\tEnd [q](page:q)\n"
      missing <- get (tokenOf "bob") "/api/page/s/p?part=section&title=Four"
      liftIO $ status missing `shouldBe` 404
      meta <- get (tokenOf "ann") "/api/page/s/p/meta"
      liftIO $ body meta `shouldBe` "title: Page\nsummary: About p\ntags: a > b, c\n\
                                    \access: family:w, public\n"
      secret <- get (tokenOf "bob") "/api/page/s/secret"
      liftIO $ status secret `shouldBe` 404

  it "lists the folders of a source for the admins only" $ onKiwi $ \_ tokenOf -> do
    r <- get (tokenOf "ann") "/api/folders/s"
    liftIO $ T.lines (body r) `shouldBe` ["source s: folders (pages)", "  . (2)", "  sub (1)"]
    refused <- get (tokenOf "bob") "/api/folders/s"
    liftIO $ status refused `shouldBe` 403

  it "gives the guide of kiwi, then the one of the user" $ onKiwi $ \_ tokenOf -> do
    r <- get (tokenOf "bob") "/api/guide"
    liftIO $ body r `shouldSatisfy` T.isPrefixOf "# Kiwi: guide for the agents"
    liftIO $ body r `shouldSatisfy` T.isSuffixOf "# Guide of the user: Other\n\nQ [p](page:p)\n"

  it "refuses the token of a removed account, or of a changed password" $ do
    let ann = Account "ann" "salt-1" "hash" ["admin"]
    (token, entry) <- Auth.newApiToken ann
    let tokens = M.fromList [entry]
    fmap loginUser (Auth.tokenLogin [ann] tokens token) `shouldBe` Just "ann"
    fmap loginGroups (Auth.tokenLogin [ann { accountGroups = ["x"] }] tokens token)
      `shouldBe` Just ["x"]
    fmap loginUser (Auth.tokenLogin [ann { accountSalt = "salt-2" }] tokens token)
      `shouldBe` Nothing
    fmap loginUser (Auth.tokenLogin [] tokens token) `shouldBe` Nothing

  it "reads the phrases and the excluded words of a query" $ do
    AS.parseQuery "first \"aid  kit\" -baby burns \"open"
      `shouldBe` AS.Query ["first", "aid", "kit", "burns", "open"] ["aid kit", "open"] ["baby"]
    AS.parseQuery " - x" `shouldBe` AS.Query ["-", "x"] [] []

  let searchKiwi docs test =
        withKiwiConfig [] [("bob", "secret", ["family"])] docs $ \_ ss app -> do
          accs <- readIORef (accounts ss)
          made <- mapM Auth.newApiToken accs
          _ <- Auth.dumpTokens (M.fromList $ map snd made) (Auth.tokensFile $ kiwiDir ss)
          let token = case made of
                (t, _) : _ -> t
                [] -> error "no token"
              search q = do
                r <- WT.srequest $ WT.SRequest
                       (WT.setPath Wai.defaultRequest
                          { Wai.requestHeaders = [("Authorization", "Bearer " <> TE.encodeUtf8 token)] }
                          ("/api/search?q=" <> URI.urlEncode True (TE.encodeUtf8 q))) ""
                return $ T.lines $ TL.toStrict $ TLE.decodeUtf8 $ WT.simpleBody r
          WT.runSession (test search) app

  it "finds a term in half of the pages, whose score is 0" $
    searchKiwi [ ("a.md", "title: A\nuid: a\n\nA link.\n"), ("b.md", "title: B\nuid: b\n\nNothing.\n") ] $
      \search -> do
        r <- search "link"
        liftIO $ r `shouldSatisfy` any (\l -> "s/a  " `T.isPrefixOf` l && "  1/1 in content" `T.isSuffixOf` l)

  it "searches the pages with all the terms first, with passages" $ do
    let docs = [ ("aid.md", "title: First aid\nuid: aid\ntags: health\n\n# Wounds\n\n\
                            \Clean the wound with water.\n\n## Burns\n\n\
                            \Cool the burn with water for long minutes.\n")
               , ("baby.md", "title: Baby\nuid: baby\n\nA first aid kit for the baby.\n")
               , ("snow.md", "title: Snow\nuid: snow\n\nThe first row of blocks.\n") ]
    withKiwiConfig [] [("bob", "secret", ["family"])] docs $ \_ ss app -> do
      accs <- readIORef (accounts ss)
      made <- mapM Auth.newApiToken accs
      _ <- Auth.dumpTokens (M.fromList $ map snd made) (Auth.tokensFile $ kiwiDir ss)
      let token = case made of
            (t, _) : _ -> t
            [] -> error "no token"
          search q = do
            r <- WT.srequest $ WT.SRequest
                   (WT.setPath Wai.defaultRequest
                      { Wai.requestHeaders = [("Authorization", "Bearer " <> TE.encodeUtf8 token)] }
                      ("/api/search?q=" <> URI.urlEncode True (TE.encodeUtf8 q))) ""
            return $ T.lines $ TL.toStrict $ TLE.decodeUtf8 $ WT.simpleBody r
      flip WT.runSession app $ do
        burns <- search "water burns"
        liftIO $ burns `shouldSatisfy` \ls -> case ls of
          h : p : rest -> h == "1 page for water burns (english), with all the terms" &&
                          "s/aid  " `T.isPrefixOf` p && "  2/2 in content" `T.isSuffixOf` p &&
                          -- the uid is not in the text: the content starts at line 4
                          rest == [ "  l.6 § Wounds: Clean the wound with water."
                                  , "  l.8: ## Burns"
                                  , "  l.10 § Burns: Cool the burn with water for long minutes." ]
          _ -> False
        aid <- search "first aid"
        liftIO $ take 1 aid `shouldBe`
          ["2 pages for first aid (english), with all the terms; 1 other pages have only some of them"]
        phrase <- search "\"aid kit\""
        liftIO $ map (T.take 7) (drop 1 phrase) `shouldBe` ["s/baby ", "  l.3: "]
        excluded <- search "first -baby"
        liftIO $ excluded `shouldSatisfy` \ls -> length (filter ("s/" `T.isPrefixOf`) ls) == 2 &&
                                                 not (any ("s/baby" `T.isPrefixOf`) ls)
        partial <- search "snow burns"
        liftIO $ take 1 partial `shouldBe` ["2 pages for snow burns (english), none with all the terms"]
        liftIO $ partial `shouldSatisfy` any (\l -> "s/snow  " `T.isPrefixOf` l &&
                                                    "  1/2 (snow) in title" `T.isSuffixOf` l)

  it "replaces exact passages, each one found once, and gives their new lines" $ do
    Edit.applyEdits [("b\n", "B1\nB2\n"), ("d", "D")] "a\nb\nc\nd\n"
      `shouldBe` Right ("a\nB1\nB2\nc\nD\n", [(2, 3), (5, 5)])
    -- a later edit moves the lines of an earlier one
    Edit.applyEdits [("d", "D"), ("a\n", "")] "a\nb\nc\nd\n"
      `shouldBe` Right ("b\nc\nD\n", [(3, 3), (1, 1)])
    Edit.applyEdits [("x", "y")] "a\n" `shouldSatisfy` either ("not in the page" `L.isInfixOf`) (const False)
    Edit.applyEdits [("a", "y")] "a\na\n"
      `shouldSatisfy` either ("2 times (lines 1, 2)" `L.isInfixOf`) (const False)
    Edit.applyEdits [("", "y")] "a\n" `shouldSatisfy` either (const True) (const False)

  it "names the file of a new page after its title" $ do
    Edit.titleFileName "Premiers soins : brûlures / plaies" `shouldBe` "Premiers soins  brûlures  plaies"
    Edit.titleFileName "  _#Notes~" `shouldBe` "Notes"
    Edit.titleFileName "???" `shouldBe` "page"
    map Edit.validPagePath ["a/b", "b", "../b", "a/.b", "_a/b", "/a", "a/-b", ""]
      `shouldBe` [True, True, False, False, False, False, False, False]

  -- writes, with a token of ann (admin), bob (family: write right on p) and
  -- joe (friends: read only)
  let writePages = [ ("p.md", "title: Page\nuid: p\naccess: family:w, friends\n\nHello\n\nWorld\n") ]
      onWriteKiwi test = withKiwiConfig []
                           [ ("ann", "secret", ["admin"]), ("bob", "secret", ["family"])
                           , ("joe", "secret", ["friends"]) ] writePages $ \cd ss app -> do
        accs <- readIORef (accounts ss)
        made <- mapM Auth.newApiToken accs
        _ <- Auth.dumpTokens (M.fromList $ map snd made) (Auth.tokensFile $ kiwiDir ss)
        let tokenOf l = case [ t | (a, (t, _)) <- zip accs made, accountId a == l ] of
              t : _ -> t
              [] -> error $ "no token for " ++ T.unpack l
        WT.runSession (test cd tokenOf) app
      send method token headers path b = WT.srequest $ WT.SRequest
        (WT.setPath Wai.defaultRequest
           { Wai.requestMethod = method
           , Wai.requestHeaders = ("Authorization", "Bearer " <> TE.encodeUtf8 token) : headers }
           path)
        (TLE.encodeUtf8 $ TL.fromStrict b)
      fileOf cd name = liftIO $ TE.decodeUtf8 <$> BS.readFile (cd FP.</> "s" FP.</> name)

  it "replaces a page with the hash it was read with" $ onWriteKiwi $ \cd tokenOf -> do
    bytes <- liftIO $ BS.readFile (cd FP.</> "s" FP.</> "p.md")
    let hash = TE.encodeUtf8 (fileHash bytes)
    noMatch <- send "PUT" (tokenOf "bob") [] "/api/page/s/p" "title: New\n\nBye\n"
    liftIO $ status noMatch `shouldBe` 428
    stale <- send "PUT" (tokenOf "bob") [("If-Match", "\"0000\"")] "/api/page/s/p" "title: New\n\nBye\n"
    liftIO $ status stale `shouldBe` 412
    readOnly <- send "PUT" (tokenOf "joe") [("If-Match", hash)] "/api/page/s/p" "title: New\n\nBye\n"
    liftIO $ status readOnly `shouldBe` 403
    ok <- send "PUT" (tokenOf "bob") [("If-Match", "\"" <> hash <> "\"")] "/api/page/s/p"
            "title: New\naccess: public\n\nBye\n"
    liftIO $ status ok `shouldBe` 200
    liftIO $ T.lines (body ok) `shouldSatisfy` \ls ->
      take 1 ls == ["saved s/p"] && "ignored keys (not allowed): access" `elem` ls
    -- the uid and the access are kept
    file <- fileOf cd "p.md"
    liftIO $ file `shouldBe` "title: New\nuid: p\naccess: family:w, friends\n\nBye\n"
    -- the index is up to date
    r <- get (tokenOf "bob") "/api/pages"
    liftIO $ body r `shouldSatisfy` T.isInfixOf "  New  ["

  it "replaces exact passages of a page" $ onWriteKiwi $ \cd tokenOf -> do
    let patchWith edits = send "PATCH" (tokenOf "bob") [] "/api/page/s/p" edits
    missing <- patchWith "{\"edits\": [{\"old\": \"Hullo\", \"new\": \"Hi\"}]}"
    liftIO $ status missing `shouldBe` 409
    bad <- patchWith "{\"old\": \"Hello\"}"
    liftIO $ status bad `shouldBe` 400
    ok <- patchWith "{\"edits\": [{\"old\": \"Hello\\n\", \"new\": \"Hello\\n\\nNew line\\n\"}]}"
    liftIO $ status ok `shouldBe` 200
    -- the text starts with "title: Page" (the uid is not shown): Hello is line 3
    liftIO $ T.lines (body ok) `shouldSatisfy` elem "lines: 3-5"
    file <- fileOf cd "p.md"
    liftIO $ file `shouldBe` "title: Page\nuid: p\naccess: family:w, friends\n\nHello\n\nNew line\n\nWorld\n"

  it "creates a page with a new uid, named after its title" $ onWriteKiwi $ \cd tokenOf -> do
    -- the default access of the pages is public (read only): bob can not
    -- write a page of this access, the admin can
    refused <- send "POST" (tokenOf "bob") [] "/api/pages/s" "title: Burns\n\nCool it.\n"
    liftIO $ status refused `shouldBe` 403
    noSource <- send "POST" (tokenOf "ann") [] "/api/pages/nope" "title: Burns\n\nCool it.\n"
    liftIO $ status noSource `shouldBe` 400
    first' <- send "POST" (tokenOf "ann") [] "/api/pages/s" "title: Burns\nuid: mine\n\nCool it.\n"
    liftIO $ status first' `shouldBe` 201
    second <- send "POST" (tokenOf "ann") [] "/api/pages/s" "title: Burns\n\nAgain.\n"
    liftIO $ T.lines (body second) `shouldSatisfy` elem "path: Burns (2).md"
    file <- fileOf cd "Burns.md"
    liftIO $ case T.lines file of
      ["title: Burns", u, "", "Cool it."] -> do
        u `shouldSatisfy` T.isPrefixOf "uid: "
        u `shouldNotBe` "uid: mine"
        T.lines (body first') `shouldSatisfy` elem ("created s/" <> T.drop 5 u)
      other -> expectationFailure $ show other
    -- bob gets the page when it gives him the write right
    byBob <- send "POST" (tokenOf "ann") [] "/api/pages/s?path=notes/first"
               "title: Notes\naccess: family:w\n\nN\n"
    liftIO $ T.lines (body byBob) `shouldSatisfy` elem "path: notes/first.md"
    pathRefused <- send "POST" (tokenOf "bob") [] "/api/pages/s?path=x" "title: X\n\nX\n"
    liftIO $ status pathRefused `shouldBe` 403

  it "commits a page created or changed by the API, with the account as author" $ withGit $
    onWriteKiwi $ \cd tokenOf -> do
      liftIO $ do
        git cd ["init", "-q"]
        git cd ["add", "."]
        git cd ["commit", "-q", "-m", "init"]
      created <- send "POST" (tokenOf "ann") [] "/api/pages/s" "title: Burns\n\nCool it.\n"
      liftIO $ status created `shouldBe` 201
      _ <- send "PATCH" (tokenOf "bob") [] "/api/page/s/p"
             "{\"edits\": [{\"old\": \"World\", \"new\": \"Earth\"}]}"
      logs <- liftIO $ gitOut cd ["log", "--format=%an|%s"]
      liftIO $ take 2 (lines logs) `shouldSatisfy` \ls -> case ls of
        [edit, create] -> edit == "bob via Kiwi|Edit s:p (kiwi kiwi)" &&
                          "ann via Kiwi|Create s:" `L.isPrefixOf` create
        _ -> False

  it "applies a metadata script to the pages of its pages lines, all or nothing" $
    withKiwiConfig [] [("ann", "secret", ["admin"]), ("bob", "secret", ["family"])]
      [ ("a.md", "title: A\nuid: a\ntags: cuisine > matériel, x\naccess: family:w\n\nA\n")
      , ("b.md", "title: B\nuid: b\ntags: cuisine > matériel > poêle\naccess: family:w\n\nB\n")
      , ("c.md", "title: C\nuid: c\ntags: cuisine > matériel\naccess: family\n\nC\n") ] $ \cd ss app -> do
      accs <- readIORef (accounts ss)
      made <- mapM Auth.newApiToken accs
      _ <- Auth.dumpTokens (M.fromList $ map snd made) (Auth.tokensFile $ kiwiDir ss)
      let tokenOf l = case [ t | (a, (t, _)) <- zip accs made, accountId a == l ] of
            t : _ -> t
            [] -> error $ "no token for " ++ T.unpack l
          script' path token s = WT.srequest $ WT.SRequest
            (WT.setPath Wai.defaultRequest
               { Wai.requestMethod = "POST"
               , Wai.requestHeaders = [("Authorization", "Bearer " <> TE.encodeUtf8 token)] }
               path)
            (TLE.encodeUtf8 $ TL.fromStrict s)
          script = script' "/api/metadata"
          fileOf' name = TE.decodeUtf8 <$> BS.readFile (cd FP.</> "s" FP.</> name)
      flip WT.runSession app $ do
        -- a typo stops everything, with the close tags
        typo <- script (tokenOf "bob") "pages s/*\nmeta tags move \"cuisine > matériel\" \"matériel > cuisine\"\n\
                                       \meta tags delete \"x > matériel\"\n"
        liftIO $ status typo `shouldBe` 400
        liftIO $ body typo `shouldSatisfy` T.isInfixOf "line 3"
        liftIO $ body typo `shouldSatisfy` T.isInfixOf "tags close to it: cuisine > matériel"
        a0 <- liftIO $ fileOf' "a.md"
        liftIO $ a0 `shouldSatisfy` T.isInfixOf "cuisine > matériel"
        -- bob can not write c, nor read a source t
        refused <- script (tokenOf "bob") "pages s/c\nmeta summary set Cuisine\n"
        liftIO $ status refused `shouldBe` 403
        unknown <- script (tokenOf "bob") "pages t/*\nmeta tags delete x\n"
        liftIO $ status unknown `shouldBe` 400
        liftIO $ body unknown `shouldSatisfy` T.isInfixOf "no source t; sources: s"
        missing <- script (tokenOf "bob") "pages s/zz\nmeta tags delete x\n"
        liftIO $ body missing `shouldSatisfy` T.isInfixOf "line 1: no page s/zz"
        syntax <- script (tokenOf "bob") "pages *\nmeta tags move x\n"
        liftIO $ status syntax `shouldBe` 400
        -- the check writes nothing, and lists the pages
        let ok' = "pages s/*\nmeta tags move \"cuisine > matériel\" \"matériel > cuisine\"\n\
                  \pages s/a\nmeta tags remove x\nmeta summary set \"About A\"\n"
        -- the check runs on the pages that bob can read, c marked
        checked <- script' "/api/metadata?check=yes" (tokenOf "bob") ok'
        liftIO $ status checked `shouldBe` 200
        liftIO $ T.lines (body checked) `shouldBe`
          [ "check only, nothing written: 3 pages would change"
          , "1 of them read only for this account: an account that can change them must apply the script"
          , "line 2: meta tags move \"cuisine > matériel\" \"matériel > cuisine\": 3 pages"
          , "  s/a A", "  s/b B", "  s/c C (read only)"
          , "line 4: meta tags remove x: 1 page", "  s/a A"
          , "line 5: meta summary set \"About A\": 1 page", "  s/a A" ]
        namedRO <- script' "/api/metadata?check=yes" (tokenOf "bob") "pages s/c\nmeta summary set Cuisine\n"
        liftIO $ status namedRO `shouldBe` 200
        liftIO $ T.lines (body namedRO) `shouldSatisfy` elem "  s/c C (read only)"
        a1 <- liftIO $ fileOf' "a.md"
        liftIO $ a1 `shouldBe` a0
        -- c, which bob can not write, is left out of s/*
        ok <- script (tokenOf "bob") ok'
        liftIO $ status ok `shouldBe` 200
        liftIO $ T.lines (body ok) `shouldBe`
          [ "2 pages changed"
          , "line 2: meta tags move \"cuisine > matériel\" \"matériel > cuisine\": 2 pages"
          , "line 4: meta tags remove x: 1 page"
          , "line 5: meta summary set \"About A\": 1 page" ]
        a <- liftIO $ fileOf' "a.md"
        liftIO $ a `shouldBe` "title: A\nsummary: About A\nuid: a\ntags: matériel > cuisine\n\
                              \access: family:w\n\nA\n"
        b <- liftIO $ fileOf' "b.md"
        liftIO $ b `shouldSatisfy` T.isInfixOf "tags: matériel > cuisine > poêle\n"
        c <- liftIO $ fileOf' "c.md"
        liftIO $ c `shouldSatisfy` T.isInfixOf "tags: cuisine > matériel\n"
        -- the index follows
        r <- get (tokenOf "bob") "/api/pages?tag=mat%C3%A9riel"
        liftIO $ take 1 (T.lines $ body r) `shouldBe` ["2 pages, the most recently modified first"]
        -- the syntax, for the agents
        guide <- get (tokenOf "bob") "/api/metadata-guide"
        liftIO $ body guide `shouldSatisfy` T.isInfixOf "# Metadata scripts"
