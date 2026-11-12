module Kiwi.ViewSpec (viewSpec) where

import qualified Data.Text as T
import           Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Char8 as BC
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import           Lucid (renderText)
import qualified Network.HTTP.Types as Net
import qualified Network.Wai as Wai
import qualified Network.Wai.Test as WT
import qualified System.FilePath as FP
import           Test.Hspec
import qualified Text.Pandoc.Definition as PD
import qualified Text.Pandoc.Walk as PW

import qualified Kiwi.Locales as Loc
import           Kiwi.Server (reloadKiwi, securityHeaders)
import           Kiwi.Types
import qualified Kiwi.View as V
import           TestUtils

viewSpec :: Spec
viewSpec = do
  let page title = (mkPage ("src", "sub:p") [] ["public"] False 1) { pageTitle = title }

  it "escapes the brackets and backslashes of the title in the copied link" $
    V.cmLink (page "a [b] \\c")
      `shouldBe` "[a \\[b\\] \\\\c](<page:/sub:p>)"

  it "copies a link whose text is the title, once parsed" $ do
    let title = "a [b] ]c[ \\"
        doc = pandocDoc $ load $ "t: t\n\n" <> V.cmLink (page title)
        linkTexts = PW.query (\i -> case i of
                                PD.Link _ ils _ -> [PW.query str ils]
                                _ -> []) doc
        str i = case i of
          PD.Str s -> s
          PD.Space -> " "
          _ -> ""
    linkTexts `shouldBe` [title]

  it "sets the lang of the page on its title and document" $ do
    let p = (page "Titre") { pageLang = French }
        html = TL.toStrict $ renderText $
               V.pageView (Loc.locales UI_English) noContext (const True) p
                          (PD.Pandoc mempty []) []
    html `shouldSatisfy` T.isInfixOf "<h1 class=\"page-title\" lang=\"fr\">Titre</h1>"
    html `shouldSatisfy` T.isInfixOf "<div class=\"document\" lang=\"fr\">"

  it "adds the security headers to the responses" $ do
    let app _ respond = respond $ Wai.responseLBS Net.status404 [] "not found"
    resp <- WT.runSession (WT.request Wai.defaultRequest) (securityHeaders app)
    WT.simpleStatus resp `shouldBe` Net.status404
    lookup "Content-Security-Policy" (WT.simpleHeaders resp) `shouldBe`
      Just "script-src 'self'; object-src 'none'; base-uri 'self'; frame-ancestors 'self'"
    lookup "X-Content-Type-Options" (WT.simpleHeaders resp) `shouldBe` Just "nosniff"

  -- requests to the application of a kiwi with a page "p"
  let get path = WT.request $ WT.setPath Wai.defaultRequest path
      postForm path form = WT.srequest $ WT.SRequest
        (WT.setPath Wai.defaultRequest { Wai.requestMethod = "POST"
                                       , Wai.requestHeaders =
                                           [("Content-Type", "application/x-www-form-urlencoded")] }
                    path)
        form
      onKiwi = withKiwiApp [("p.md", "title: Page\n\nHello")] . WT.runSession
      body = TL.toStrict . TLE.decodeUtf8 . WT.simpleBody

  it "serves the pages and the search form without query" $ onKiwi $ do
    page' <- get "/page/s/p"
    liftIO $ WT.simpleStatus page' `shouldBe` Net.status200
    liftIO $ body page' `shouldSatisfy` T.isInfixOf "Hello"
    search <- get "/search"
    liftIO $ WT.simpleStatus search `shouldBe` Net.status200

  it "answers a missing parameter with the bad request page" $ onKiwi $ do
    resp <- postForm "/login" ""
    liftIO $ WT.simpleStatus resp `shouldBe` Net.status400
    liftIO $ body resp `shouldSatisfy` T.isInfixOf "<h1>Invalid request</h1>"
    liftIO $ body resp `shouldSatisfy` T.isInfixOf "missing parameter: username"
    liftIO $ lookup "X-Content-Type-Options" (WT.simpleHeaders resp) `shouldBe` Just "nosniff"

  let getWith etag path = WT.request $ WT.setPath
        Wai.defaultRequest { Wai.requestHeaders = [("If-None-Match", etag)] } path
      withAdmin = withKiwiAccounts [("ann", "secret", ["admin"])]
                                   [("p.md", "title: Page\n\nHello")] . WT.runSession
      etagOf = maybe "" id . lookup "ETag" . WT.simpleHeaders

  it "answers 401 to a wrong login, and redirects after a right one" $ withAdmin $ do
    -- the status is what a reverse proxy or fail2ban can count
    wrong <- postForm "/login" "username=ann&password=wrong"
    liftIO $ WT.simpleStatus wrong `shouldBe` Net.status401
    unknown <- postForm "/login" "username=bob&password=secret"
    liftIO $ WT.simpleStatus unknown `shouldBe` Net.status401
    right <- postForm "/login" "username=ann&password=secret"
    liftIO $ WT.simpleStatus right `shouldBe` Net.status302

  it "answers 304 to the ETag of a page, until the pages are reloaded" $ withAdmin $ do
    first <- get "/page/s/p"
    let etag = etagOf first
    liftIO $ etag `shouldSatisfy` (/= "")
    liftIO $ lookup "Cache-Control" (WT.simpleHeaders first) `shouldBe` Just "private, no-cache"
    again <- getWith etag "/page/s/p"
    liftIO $ WT.simpleStatus again `shouldBe` Net.status304
    liftIO $ WT.simpleBody again `shouldBe` ""
    liftIO $ etagOf again `shouldBe` etag
    -- another URL, another reader: rendered again
    other <- getWith etag "/page/s/p?archive=yes"
    liftIO $ WT.simpleStatus other `shouldBe` Net.status404
    _ <- postForm "/login" "username=ann&password=secret"
    logged <- getWith etag "/page/s/p"
    liftIO $ WT.simpleStatus logged `shouldBe` Net.status200
    let etag' = etagOf logged
    liftIO $ etag' `shouldSatisfy` (/= etag)
    cached <- getWith etag' "/page/s/p"
    liftIO $ WT.simpleStatus cached `shouldBe` Net.status304
    -- a reload makes the ETags obsolete, even for an unchanged page
    reload <- postForm "/reload" ""
    liftIO $ WT.simpleStatus reload `shouldBe` Net.status200
    reloaded <- getWith etag' "/page/s/p"
    liftIO $ WT.simpleStatus reloaded `shouldBe` Net.status200

  it "compresses the pages for the clients that accept it" $ onKiwi $ do
    let long = "/search?query=" <> BC.pack (replicate 2000 'a')
    resp <- WT.request $ WT.setPath
      Wai.defaultRequest { Wai.requestHeaders = [("Accept-Encoding", "gzip")] } long
    liftIO $ lookup "Content-Encoding" (WT.simpleHeaders resp) `shouldBe` Just "gzip"
    plain <- get long
    liftIO $ lookup "Content-Encoding" (WT.simpleHeaders plain) `shouldBe` Nothing

  it "lists the load errors on the doctor page, for admins only" $
    withKiwiAccounts [("ann", "secret", ["admin"]), ("bob", "secret", ["family"])]
                     [("p.md", "title: Page\n\nHello"), ("bad.md", "# No header\n\nText")] $
    WT.runSession $ do
      let section = "<h2>Load errors</h2>"
      anonymous <- get "/doctor"
      liftIO $ WT.simpleStatus anonymous `shouldBe` Net.status200
      liftIO $ body anonymous `shouldSatisfy` (not . T.isInfixOf section)
      _ <- postForm "/login" "username=bob&password=secret"
      reader <- get "/doctor"
      liftIO $ body reader `shouldSatisfy` (not . T.isInfixOf "bad.md")
      _ <- postForm "/logout" ""
      _ <- postForm "/login" "username=ann&password=secret"
      admin <- get "/doctor"
      liftIO $ body admin `shouldSatisfy` T.isInfixOf section
      liftIO $ body admin `shouldSatisfy` T.isInfixOf "<span class=\"file\">bad.md</span>"
      liftIO $ body admin `shouldSatisfy` T.isInfixOf "invalid metadata header, line 1"
      -- the page is not served
      missing <- get "/page/s/bad"
      liftIO $ WT.simpleStatus missing `shouldBe` Net.status404

  it "reloads the pages and the accounts out of a request, as the reload signal does" $
    withKiwiState [] [("p.md", "title: Page\n\nHello")] $ \cd ss app -> do
      let run = flip WT.runSession app
      missing <- run $ get "/page/s/new"
      WT.simpleStatus missing `shouldBe` Net.status404
      writeFile (cd FP.</> "s" FP.</> "pages" FP.</> "new.md") "title: New\n\nHello"
      reloadKiwi ss
      found <- run $ get "/page/s/new"
      WT.simpleStatus found `shouldBe` Net.status200

  it "shows the search results one by line, with an excerpt" $
    withKiwiApp [ ("a.md", "title: Alpha\nlang: english\n\nThe horses run in the field")
                , ("b.md", "title: Beta\n\nA horse sleeps") ] $ WT.runSession $ do
      resp <- get "/search?query=horse"
      liftIO $ WT.simpleStatus resp `shouldBe` Net.status200
      liftIO $ body resp `shouldSatisfy` T.isInfixOf "<h1>2 results</h1>"
      liftIO $ body resp `shouldSatisfy` T.isInfixOf
        "<li><a href=\"/page/s/a\"><span class=\"title\">Alpha</span>\
        \<p class=\"excerpt\" lang=\"en\">The <strong>horses</strong> run in the field</p>\
        \</a></li>"
