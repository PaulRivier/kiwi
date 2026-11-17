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

  it "needs a known token, and never the session cookie" $ onKiwi $ \_ _ -> do
    none <- WT.request $ WT.setPath Wai.defaultRequest "/api/pages"
    liftIO $ status none `shouldBe` 401
    liftIO $ lookup "WWW-Authenticate" (WT.simpleHeaders none) `shouldBe` Just "Bearer"
    wrong <- get "nope" "/api/pages"
    liftIO $ status wrong `shouldBe` 401

  it "lists the pages readable by the account, with their tags and summary" $
    onKiwi $ \_ tokenOf -> do
      r <- get (tokenOf "bob") "/api/pages"
      liftIO $ status r `shouldBe` 200
      liftIO $ lookup "Cache-Control" (WT.simpleHeaders r) `shouldBe` Just "no-store"
      let ls = T.lines (body r)
      liftIO $ take 1 ls `shouldBe` ["2 pages, the most recently modified first"]
      liftIO $ ls `shouldSatisfy` any (\l -> "s/p  " `T.isPrefixOf` l &&
                                             "  Page  [a > b, c]" `T.isSuffixOf` l)
      liftIO $ ls `shouldSatisfy` elem "  summary: About p"
      -- the paths are for the admins only
      liftIO $ body r `shouldSatisfy` (not . T.isInfixOf "path:")
      tagged <- get (tokenOf "ann") "/api/pages?tag=a&limit=5"
      liftIO $ T.lines (body tagged) `shouldSatisfy` elem "  path: p.md"
      liftIO $ take 1 (T.lines $ body tagged) `shouldBe` ["1 page, the most recently modified first"]
      limited <- get (tokenOf "ann") "/api/pages?limit=1"
      liftIO $ take 1 (T.lines $ body limited)
        `shouldBe` ["3 pages, the most recently modified first, 1 shown (limit)"]
      bad <- get (tokenOf "ann") "/api/pages?limit=x"
      liftIO $ status bad `shouldBe` 400

  it "gives the vocabulary of the sources, with the number of pages" $
    onKiwi $ \_ tokenOf -> do
      r <- get (tokenOf "bob") "/api/vocabulary"
      liftIO $ T.lines (body r) `shouldBe`
        [ "source s: 2 pages", "tags (pages):", "  a (1)", "  a > b (1)", "  c (2)"
        , "lang: English (2)", "zone (text): north (1)" ]

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
