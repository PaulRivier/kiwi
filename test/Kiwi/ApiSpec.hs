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
import qualified System.Directory as D
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
      -- a kiwi of these pages (default access: public), read by an account
      withKiwiAccounts' files test = withKiwiConfig [] [("bob", "secret", ["family"])] files $
                                       \_ ss app -> do
        accs <- readIORef (accounts ss)
        made <- mapM Auth.newApiToken accs
        _ <- Auth.dumpTokens (M.fromList $ map snd made) (Auth.tokensFile $ kiwiDir ss)
        let token = case made of
              (t, _) : _ -> t
              [] -> error "no token"
        WT.runSession (test (get token)) app
      onKiwi test = withKiwiConfig ["agent-guide: agent-guide.md"]
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
      -- SOURCE/UID alone without show
      liftIO $ L.sort (T.lines (body r)) `shouldBe`
        L.sort ["2 pages, the most recently modified first", "s/p", "s/q"]
      -- the fields of show, as in a header, in the order asked
      shownP <- get (tokenOf "bob") "/api/pages?page=s/p&show=title,tags&show=summary,zone"
      liftIO $ T.lines (body shownP) `shouldBe`
        [ "1 page, the most recently modified first", "s/p", "  title: Page", "  tags: a > b, c"
        , "  summary: About p" ]
      -- the size of its content, without the header
      sized <- get (tokenOf "bob") "/api/pages?page=s/p&show=content_size,content_lines"
      liftIO $ T.lines (body sized) `shouldBe`
        [ "1 page, the most recently modified first", "s/p", "  content_size: 54", "  content_lines: 11" ]
      badShow <- get (tokenOf "bob") "/api/pages?show=nope"
      liftIO $ status badShow `shouldBe` 400
      badPage <- get (tokenOf "bob") "/api/pages?page=nope"
      liftIO $ status badPage `shouldBe` 400
      tagged <- get (tokenOf "ann") "/api/pages?tag=a&limit=5"
      liftIO $ take 1 (T.lines $ body tagged) `shouldBe` ["1 page, the most recently modified first"]
      -- no path in the lists, even for the admins
      liftIO $ body tagged `shouldSatisfy` (not . T.isInfixOf "path:")
      -- the values of the fields asked, under the pages that have them
      shown <- get (tokenOf "bob") "/api/pages?has=zone&show=zone"
      liftIO $ case T.lines (body shown) of
        [h, q, z] -> do
          h `shouldBe` "1 page, the most recently modified first"
          q `shouldBe` "s/q"
          z `shouldBe` "  zone: north"
        other -> expectationFailure (show other)
      limited <- get (tokenOf "ann") "/api/pages?limit=1"
      liftIO $ take 1 (T.lines $ body limited)
        `shouldBe` ["3 pages, the most recently modified first, 1 shown (limit): \
                    \narrow with tags, fields or since, or raise limit"]
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
      -- only the facets of show
      onlyTags <- get (tokenOf "bob") "/api/facets?show=tags"
      liftIO $ T.lines (body onlyTags) `shouldBe`
        [ "source s: 2 pages", "tags (pages):", "  a (1)", "  a > b (1)", "  c (2)" ]
      onlyZone <- get (tokenOf "bob") "/api/facets?show=zone"
      liftIO $ T.lines (body onlyZone) `shouldBe` [ "source s: 2 pages", "zone (text; the area): north (1)" ]
      badShow <- get (tokenOf "bob") "/api/facets?show=title"
      liftIO $ status badShow `shouldBe` 400

  it "gives the links and backlinks of the pages as fields of the list" $
    withKiwiAccounts' [ ("a.md", "title: A\nuid: a\n\n[b](page:b) [b2](page:b#x) [z](page:z) [s](page:s)\n")
                      , ("b.md", "title: B\nuid: b\ntags: t\n\n[a](page:a)\n")
                      , ("c.md", "title: C\nuid: c\n\nC\n")
                      , ("s.md", "title: S\nuid: s\naccess: nobody\n\nS\n") ] $ \get' -> do
      r <- get' "/api/pages?page=s/a&page=s/b&page=s/c&show=links,backlinks"
      liftIO $ L.sort (drop 1 $ T.lines (body r)) `shouldBe` L.sort
        [ "s/a", "  links: b, s (missing), z (missing)", "  backlinks: b"
        , "s/b", "  links: a", "  backlinks: a"
        , "s/c" ]

  it "gives the related pages as a field of the list, in the sources of the context" $
    withKiwiAccounts' [ ("a.md", "title: A\nuid: a\ntags: cuisine > dessert\n\nA\n")
                      , ("b.md", "title: B\nuid: b\ntags: cuisine > dessert\n\nB\n")
                      , ("c.md", "title: C\nuid: c\ntags: cuisine > plat\n\nC\n")
                      , ("d.md", "title: D\nuid: d\ntags: jardin\n\nD\n")
                      , ("s.md", "title: S\nuid: s\naccess: nobody\ntags: cuisine > dessert\n\nS\n") ] $ \get' -> do
      r <- get' "/api/pages?page=s/a&page=s/d&show=related"
      liftIO $ L.sort (drop 1 $ T.lines (body r)) `shouldBe` L.sort
        [ "s/a", "  related: s/b, s/c", "s/d" ]

  it "gives the outline of a page too long to be given whole, and its lines on demand" $
    withKiwiAccounts' [ ("big.md", "title: Big\nuid: big\n\n# Start\n\n"
                                    <> concat (replicate 1000 "A line of fifty characters, to make it long.\n")) ] $ \get' -> do
      r <- get' "/api/page/s/big"
      liftIO $ T.lines (body r) `shouldSatisfy` \ls -> case ls of
        note : ref : _ -> "this page has " `T.isPrefixOf` note && "from 1 to 1002" `T.isInfixOf` note
                          && "(part whole)" `T.isInfixOf` note
                          && ref == "s/big  Big"
        _ -> False
      whole <- get' "/api/page/s/big?part=lines&from=1&to=1002"
      liftIO $ length (T.lines $ body whole) `shouldBe` 1002
      forced <- get' "/api/page/s/big?part=whole"
      liftIO $ take 3 (T.lines $ body forced) `shouldBe` ["title: Big", "", "# Start"]
      liftIO $ length (T.lines $ body forced) `shouldBe` 1004
      liftIO $ lookup "ETag" (WT.simpleHeaders r) `shouldSatisfy` maybe False ((== 18) . BS.length)

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
        L.isSubsequenceOf [ "s/p  Page", "tags: a > b, c", "summary: About p"
                          , "content_size: 54", "content_lines: 11"
                          , "backlinks: s/q Other", "sections (lines):"
                          , "  1-8  # One", "  5-8  ## Two", "  9-11  # Three" ] ls
      -- the lines are those of the content: the same for the admin, who sees
      -- one more line of header
      outlineAnn <- get (tokenOf "ann") "/api/page/s/p?part=outline"
      liftIO $ T.lines (body outlineAnn) `shouldSatisfy` elem "  9-11  # Three"
      section <- get (tokenOf "bob") "/api/page/s/p?part=section&title=two"
      liftIO $ body section `shouldBe` "## Two\n\nWorld\n\n"
      liftIO $ lookup "Kiwi-Lines" (WT.simpleHeaders section) `shouldBe` Just "5-8"
      numbered <- get (tokenOf "bob") "/api/page/s/p?part=lines&from=9&to=20&numbered=yes"
      liftIO $ body numbered `shouldBe` "     9\t# Three\n    10\t\n    11\tEnd [q](page:q)\n"
      -- the whole page numbered: the header without numbers
      whole <- get (tokenOf "bob") "/api/page/s/p?numbered=yes"
      liftIO $ take 6 (T.lines $ body whole) `shouldBe`
        [ "      \ttitle: Page", "      \tsummary: About p", "      \ttags: a > b, c", "      \t"
        , "     1\t# One", "     2\t" ]
      missing <- get (tokenOf "bob") "/api/page/s/p?part=section&title=Four"
      liftIO $ status missing `shouldBe` 404
      meta <- get (tokenOf "ann") "/api/page/s/p/meta"
      liftIO $ body meta `shouldBe` "title: Page\nsummary: About p\ntags: a > b, c\n\
                                    \access: family:w, public\n"
      secret <- get (tokenOf "bob") "/api/page/s/secret"
      liftIO $ status secret `shouldBe` 404

  it "gives the folders of a source as a facet, for the admins only" $ onKiwi $ \_ tokenOf -> do
    r <- get (tokenOf "ann") "/api/facets?show=folder"
    liftIO $ T.lines (body r) `shouldBe` ["source s: 3 pages", "folder (pages): . (2), sub (1)"]
    refused <- get (tokenOf "bob") "/api/facets?show=folder"
    liftIO $ status refused `shouldBe` 400
    all' <- get (tokenOf "bob") "/api/facets"
    liftIO $ body all' `shouldSatisfy` (not . T.isInfixOf "folder")

  it "gives the documentation of the API, then the guide of the user, read at each request" $
    onKiwi $ \cd tokenOf -> do
      let guideFile = FP.takeDirectory cd FP.</> "agent-guide.md"
      liftIO $ writeFile guideFile "Be kind\n"
      r <- get (tokenOf "bob") "/api/guide"
      liftIO $ body r `shouldSatisfy` T.isPrefixOf "# Kiwi: the wiki of the user"
      wiki <- get (tokenOf "bob") "/api/guide?topic=wiki"
      liftIO $ body wiki `shouldBe` body r
      liftIO $ body r `shouldSatisfy` T.isSuffixOf "\n\n# Guide of the user\n\nBe kind\n"
      liftIO $ writeFile guideFile "Be brief\n"
      changed <- get (tokenOf "bob") "/api/guide"
      liftIO $ body changed `shouldSatisfy` T.isSuffixOf "\n\n# Guide of the user\n\nBe brief\n"
      -- a file that can no longer be read: the documentation alone
      liftIO $ D.removeFile guideFile
      gone <- get (tokenOf "bob") "/api/guide"
      liftIO $ status gone `shouldBe` 200
      liftIO $ body gone `shouldSatisfy` (not . T.isInfixOf "# Guide of the user")

  it "gives the other topic of the guide: the metadata scripts" $
    onKiwi $ \_ tokenOf -> do
      scripts <- get (tokenOf "bob") "/api/guide?topic=metadata-scripts"
      liftIO $ body scripts `shouldSatisfy` T.isPrefixOf "# Metadata scripts"
      unknown <- get (tokenOf "bob") "/api/guide?topic=nope"
      liftIO $ status unknown `shouldBe` 400

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
        liftIO $ r `shouldSatisfy` any (\l -> "s/a  " `T.isPrefixOf` l && "  1/1 in content" `T.isSuffixOf` l
                                             && "  content_size 8, content_lines 1  " `T.isInfixOf` l)

  it "gives the keywords of the pages, in lower case, computed on the whole wiki" $ do
    let docs = [ ("a.md", "title: Ruby\nuid: a\nlang: en\n\nRails and Rails, the Rails migrations.\n")
               , ("b.md", "title: Soup\nuid: b\nlang: en\n\nA carrot soup, the carrot.\n") ]
    withKiwiConfig [] [("bob", "secret", ["family"])] docs $ \_ ss app -> do
      accs <- readIORef (accounts ss)
      made <- mapM Auth.newApiToken accs
      _ <- Auth.dumpTokens (M.fromList $ map snd made) (Auth.tokensFile $ kiwiDir ss)
      let token = case made of
            (t, _) : _ -> t
            [] -> error "no token"
          get' path = do
            r <- WT.srequest $ WT.SRequest
                   (WT.setPath Wai.defaultRequest
                      { Wai.requestHeaders = [("Authorization", "Bearer " <> TE.encodeUtf8 token)] }
                      path) ""
            return $ T.lines $ TL.toStrict $ TLE.decodeUtf8 $ WT.simpleBody r
      flip WT.runSession app $ do
        -- seen once (migrations) or in every page (the): not keywords
        listed <- get' "/api/pages?page=s/a&page=s/b&show=keywords"
        liftIO $ L.sort listed `shouldBe`
          L.sort [ "2 pages, the most recently modified first"
                 , "s/a", "  keywords: rails, ruby", "s/b", "  keywords: soup, carrot" ]
        found <- get' "/api/search?q=carrot&show=keywords"
        liftIO $ found `shouldSatisfy` elem "  keywords: soup, carrot"
        -- only with show
        plain <- get' "/api/search?q=carrot"
        liftIO $ plain `shouldSatisfy` all (not . T.isPrefixOf "  keywords:")

  it "searches the pages with any of the terms, the best first, with passages" $ do
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
          h : p : rest -> h == "1 page for water burns (english)" &&
                          "s/aid  " `T.isPrefixOf` p && "  2/2 in content" `T.isSuffixOf` p &&
                          -- the lines of the content, without the header
                          rest == [ "  l.3 § Wounds: Clean the wound with water."
                                  , "  l.5: ## Burns"
                                  , "  l.7 § Burns: Cool the burn with water for long minutes." ]
          _ -> False
        -- the union of the terms, the pages with both first
        aid <- search "first aid"
        liftIO $ take 1 aid `shouldBe` ["3 pages for first aid (english), best first"]
        liftIO $ map (T.take 7) (filter ("s/" `T.isPrefixOf`) aid) `shouldSatisfy` \ps ->
          L.sort (take 2 ps) == ["s/aid  ", "s/baby "] && drop 2 ps == ["s/snow "]
        phrase <- search "\"aid kit\""
        liftIO $ map (T.take 7) (drop 1 phrase) `shouldBe` ["s/baby ", "  l.1: "]
        excluded <- search "first -baby"
        liftIO $ excluded `shouldSatisfy` \ls -> length (filter ("s/" `T.isPrefixOf`) ls) == 2 &&
                                                 not (any ("s/baby" `T.isPrefixOf`) ls)
        partial <- search "snow burns"
        liftIO $ take 1 partial `shouldBe` ["2 pages for snow burns (english), best first"]
        liftIO $ partial `shouldSatisfy` any (\l -> "s/snow  " `T.isPrefixOf` l &&
                                                    "  1/2 (snow) in title" `T.isSuffixOf` l)
        -- the words found in no page
        missing <- search "burns volcano"
        liftIO $ take 1 missing `shouldBe` ["1 page for burns volcano (english); not found: volcano"]
        -- the filters of the lists
        tagged <- WT.srequest $ WT.SRequest
                    (WT.setPath Wai.defaultRequest
                       { Wai.requestHeaders = [("Authorization", "Bearer " <> TE.encodeUtf8 token)] }
                       "/api/search?q=first&tag=health") ""
        liftIO $ filter ("s/" `T.isPrefixOf`) (T.lines $ TL.toStrict $ TLE.decodeUtf8 $ WT.simpleBody tagged)
          `shouldSatisfy` \ps -> map (T.take 6) ps == ["s/aid "]

  it "replaces exact passages, each one found once, and gives their new lines" $ do
    Edit.applyEdits [("b\n", "B1\nB2\n"), ("d", "D")] "a\nb\nc\nd\n"
      `shouldBe` Right ("a\nB1\nB2\nc\nD\n", [(2, 3), (5, 5)])
    -- a later edit moves the lines of an earlier one
    Edit.applyEdits [("d", "D"), ("a\n", "")] "a\nb\nc\nd\n"
      `shouldBe` Right ("b\nc\nD\n", [(3, 3), (1, 1)])
    Edit.applyEdits [("x", "y")] "a\n" `shouldSatisfy` either ("not in the page" `L.isInfixOf`) (const False)
    Edit.applyEdits [("a", "y")] "a\na\n"
      `shouldSatisfy` either ("2 times (lines 1, 2)" `L.isInfixOf`) (const False)
    -- the lines of the content, after the header, and at most 10 of them
    Edit.applyEdits [("x", "y")] ("title: x\n\n" <> T.replicate 12 "x\n")
      `shouldSatisfy` either ("13 times (lines header, 1, 2, 3, 4, 5, 6, 7, 8, 9, and 3 more)" `L.isInfixOf`)
                             (const False)
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
    r <- get (tokenOf "bob") "/api/pages?show=title"
    liftIO $ T.lines (body r) `shouldSatisfy` elem "  title: New"

  it "replaces exact passages of a page" $ onWriteKiwi $ \cd tokenOf -> do
    let patchWith edits = send "PATCH" (tokenOf "bob") [] "/api/page/s/p" edits
    missing <- patchWith "{\"edits\": [{\"old\": \"Hullo\", \"new\": \"Hi\"}]}"
    liftIO $ status missing `shouldBe` 409
    bad <- patchWith "{\"old\": \"Hello\"}"
    liftIO $ status bad `shouldBe` 400
    ok <- patchWith "{\"edits\": [{\"old\": \"Hello\\n\", \"new\": \"Hello\\n\\nNew line\\n\"}]}"
    liftIO $ status ok `shouldBe` 200
    -- the lines of the content: Hello is line 1
    liftIO $ T.lines (body ok) `shouldSatisfy` elem "lines: 1-3"
    file <- fileOf cd "p.md"
    liftIO $ file `shouldBe` "title: Page\nuid: p\naccess: family:w, friends\n\nHello\n\nNew line\n\nWorld\n"
    -- an edit of the header
    inHeader <- patchWith "{\"edits\": [{\"old\": \"title: Page\", \"new\": \"title: Page 2\"}]}"
    liftIO $ T.lines (body inHeader) `shouldSatisfy` elem "lines: header"

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
        T.lines (body first') `shouldSatisfy` L.isSubsequenceOf ["content_size: 9", "content_lines: 1"]
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
      _ <- send "PATCH" (tokenOf "bob") [] "/api/page/s/p?message=Earth%20is%20better"
             "{\"edits\": [{\"old\": \"World\", \"new\": \"Earth\"}]}"
      lastMessage <- liftIO $ gitOut cd ["log", "-1", "--format=%B"]
      liftIO $ lastMessage `shouldBe` "Edit s:p (kiwi kiwi)\n\nEarth is better\n\n"
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
        guide <- get (tokenOf "bob") "/api/guide?topic=metadata-scripts"
        liftIO $ body guide `shouldSatisfy` T.isInfixOf "# Metadata scripts"
