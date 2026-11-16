module Kiwi.ViewSpec (viewSpec) where

import qualified Data.Text as T
import           Control.Monad (forM_)
import           Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Char8 as BC
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import           Lucid (renderText)
import qualified Network.HTTP.Types as Net
import qualified Network.Wai as Wai
import qualified Network.Wai.Test as WT
import qualified System.Directory as D
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
  let page title = (mkPage ("src", "p") [] ["public"] False 1) { pageTitle = title }

  it "escapes the brackets and backslashes of the title in the copied link" $
    V.cmLink (page "a [b] \\c")
      `shouldBe` "[a \\[b\\] \\\\c](page:p)"

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
               V.pageView (Loc.locales UI_English) noContext (const True) p False
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
      onKiwi = withKiwiApp [("p.md", "uid: p\ntitle: Page\n\nHello")] . WT.runSession
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
                                   [("p.md", "uid: p\ntitle: Page\n\nHello")] . WT.runSession
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
                     [("p.md", "uid: p\ntitle: Page\n\nHello"), ("bad.md", "# No header\n\nText")] $
    WT.runSession $ do
      let section = "<h2>Load errors</h2>"
      anonymous <- get "/doctor"
      liftIO $ WT.simpleStatus anonymous `shouldBe` Net.status200
      liftIO $ body anonymous `shouldSatisfy` (not . T.isInfixOf section)
      _ <- postForm "/login" "username=bob&password=secret"
      reader <- get "/doctor"
      liftIO $ body reader `shouldSatisfy` (not . T.isInfixOf "bad.md")
      -- the reload button, for admins only, here and on the metadata page
      liftIO $ body reader `shouldNotSatisfy` T.isInfixOf "class=\"icon reload\""
      readerMeta <- get "/metadata"
      liftIO $ body readerMeta `shouldNotSatisfy` T.isInfixOf "class=\"icon reload\""
      _ <- postForm "/logout" ""
      _ <- postForm "/login" "username=ann&password=secret"
      admin <- get "/doctor"
      liftIO $ body admin `shouldSatisfy` T.isInfixOf section
      liftIO $ body admin `shouldSatisfy` T.isInfixOf "class=\"icon reload\""
      adminMeta <- get "/metadata"
      liftIO $ body adminMeta `shouldSatisfy` T.isInfixOf "class=\"icon reload\""
      liftIO $ body admin `shouldSatisfy` T.isInfixOf "<span class=\"file\">bad.md</span>"
      liftIO $ body admin `shouldSatisfy` T.isInfixOf "invalid metadata header, line 1"
      -- the page is not served
      missing <- get "/page/s/bad"
      liftIO $ WT.simpleStatus missing `shouldBe` Net.status404

  it "serves the files linked by the pages that the user can read, by uid" $
    withKiwiState [("ann", "secret", ["admin"]), ("bob", "secret", ["family"])
                  , ("eve", "secret", ["friends"])]
                  [ ("pub.md", "uid: pub\ntitle: P\n\n![a](file:AAAAAAAAAA) [t](file:BBBBBBBBBB) \
                               \[e](file:CCCCCCCCCC) [g](file:GGGGGGGGGG)")
                  , ("img/a__AAAAAAAAAA.png", "PNG"), ("notes__BBBBBBBBBB.txt", "text")
                  , ("\xe9t\xe9 1__CCCCCCCCCC.pdf", "PDF1"), (".git/x__GGGGGGGGGG.png", "G")
                  , ("priv.md", "uid: priv\ntitle: Priv\naccess: family\n\n[s](file:DDDDDDDDDD)")
                  , ("secret__DDDDDDDDDD.pdf", "PDF")
                  , ("old.md", "uid: old\ntitle: Old\narchive: yes\n\n[o](file:EEEEEEEEEE)")
                  , ("old__EEEEEEEEEE.pdf", "OLD"), ("alone__FFFFFFFFFF.pdf", "X") ] $ \cd ss app ->
    flip WT.runSession app $ do
      let status path = WT.simpleStatus <$> get path
          header h = lookup h . WT.simpleHeaders
      png <- get "/file/s/AAAAAAAAAA"
      liftIO $ do
        WT.simpleStatus png `shouldBe` Net.status200
        WT.simpleBody png `shouldBe` "PNG"
        header "Content-Type" png `shouldBe` Just "image/png"
        header "Content-Disposition" png
          `shouldBe` Just "inline; filename=\"a.png\"; filename*=UTF-8''a.png"
        header "Cache-Control" png `shouldBe` Just "private, no-cache"
        etagOf png `shouldSatisfy` (/= "")
      cached <- getWith (etagOf png) "/file/s/AAAAAAAAAA"
      liftIO $ WT.simpleStatus cached `shouldBe` Net.status304
      liftIO $ WT.simpleBody cached `shouldBe` ""
      txt <- get "/file/s/BBBBBBBBBB"
      liftIO $ header "Content-Type" txt `shouldBe` Just "text/plain; charset=utf-8"
      pdf <- get "/file/s/CCCCCCCCCC"
      liftIO $ header "Content-Disposition" pdf `shouldBe`
        Just "inline; filename=\"_t_ 1.pdf\"; filename*=UTF-8''%C3%A9t%C3%A9%201.pdf"
      -- linked by an archived page: served out of the archive mode
      status "/file/s/EEEEEEEEEE" >>= liftIO . (`shouldBe` Net.status200)
      -- not linked, ignored file, unknown uid, in another source
      forM_ [ "/file/s/FFFFFFFFFF", "/file/s/GGGGGGGGGG", "/file/s/HHHHHHHHHH"
            , "/file/x/AAAAAAAAAA", "/file/s/img/a__AAAAAAAAAA.png", "/file/s", "/file/" ] $
        \path -> do
          st <- status path
          liftIO $ (path, st) `shouldBe` (path, Net.status404)
      -- linked by a page that the user can not read
      status "/file/s/DDDDDDDDDD" >>= liftIO . (`shouldBe` Net.status401)
      _ <- postForm "/login" "username=eve&password=secret"
      status "/file/s/DDDDDDDDDD" >>= liftIO . (`shouldBe` Net.status403)
      _ <- postForm "/logout" ""
      _ <- postForm "/login" "username=bob&password=secret"
      status "/file/s/DDDDDDDDDD" >>= liftIO . (`shouldBe` Net.status200)
      _ <- postForm "/logout" ""
      _ <- postForm "/login" "username=ann&password=secret"
      status "/file/s/DDDDDDDDDD" >>= liftIO . (`shouldBe` Net.status200)
      -- moved and renamed: still served by its uid
      liftIO $ D.createDirectoryIfMissing True (cd FP.</> "s" FP.</> "docs")
      liftIO $ D.renameFile (cd FP.</> "s" FP.</> "secret__DDDDDDDDDD.pdf")
                            (cd FP.</> "s" FP.</> "docs" FP.</> "plan__DDDDDDDDDD.pdf")
      liftIO $ reloadKiwi ss
      moved <- get "/file/s/DDDDDDDDDD"
      liftIO $ WT.simpleBody moved `shouldBe` "PDF"
      -- no page links it any more
      liftIO $ D.removeFile (cd FP.</> "s" FP.</> "old.md")
      liftIO $ reloadKiwi ss
      status "/file/s/EEEEEEEEEE" >>= liftIO . (`shouldBe` Net.status404)

  it "lists the links to missing files on the doctor page" $
    withKiwiAccounts [("ann", "secret", ["admin"])]
                     [ ("p.md", "uid: p\ntitle: P\n\n[a](file:AAAAAAAAAA) [b](file:BBBBBBBBBB) \
                                \[c](<file:gone.png>)")
                     , ("a__AAAAAAAAAA.png", "PNG") ] $ WT.runSession $ do
      resp <- get "/doctor"
      liftIO $ body resp `shouldSatisfy` T.isInfixOf
        "<ul><li>file:BBBBBBBBBB</li><li>file:gone.png</li></ul>"

  it "lists the files linked by no page on the doctor page, for admins only" $
    withKiwiAccounts [("ann", "secret", ["admin"]), ("bob", "secret", ["family"])]
                     [ ("p.md", "uid: p\ntitle: P\n\n[a](file:AAAAAAAAAA)")
                     , ("old.md", "uid: old\ntitle: O\narchive: yes\n\n[c](file:CCCCCCCCCC)")
                     , ("a__AAAAAAAAAA.png", "PNG"), ("d/b[1]__BBBBBBBBBB.pdf", "PDF")
                     , ("c__CCCCCCCCCC.txt", "text"), ("e__EEEEEEEEEE.txt", "text") ] $
    WT.runSession $ do
      let section = "<h2>Files linked by no page</h2>"
          -- button copying a link to the file: name without uid, brackets escaped
          copy name u = "<button type=\"button\" class=\"icon copy\" \
                        \title=\"Copy a link to this file\" data-copy=\"[" <> name <>
                        "](file:" <> T.replicate 10 u <> ")\">\
                        \<img src=\"/static/images/copy-link.svg\" alt=\"\"></button>"
      anonymous <- get "/doctor"
      liftIO $ body anonymous `shouldSatisfy` (not . T.isInfixOf section)
      _ <- postForm "/login" "username=bob&password=secret"
      reader <- get "/doctor"
      liftIO $ body reader `shouldSatisfy` (not . T.isInfixOf section)
      _ <- postForm "/logout" ""
      _ <- postForm "/login" "username=ann&password=secret"
      -- the file linked by an archived page only is linked, in both modes
      forM_ ["/doctor", "/doctor?archive=yes"] $ \url -> do
        admin <- get url
        liftIO $ body admin `shouldSatisfy` T.isInfixOf
          (section <> "<section><h3>s</h3><ul class=\"unlinked-files\">\
                      \<li><span class=\"file\">d/b[1]__BBBBBBBBBB.pdf</span>" <> copy "b\\[1\\].pdf" "B" <> "</li>\
                      \<li><span class=\"file\">e__EEEEEEEEEE.txt</span>" <> copy "e.txt" "E" <> "</li></ul></section>")

  it "reloads the pages and the accounts out of a request, as the reload signal does" $
    withKiwiState [] [("p.md", "uid: p\ntitle: Page\n\nHello")] $ \cd ss app -> do
      let run = flip WT.runSession app
      missing <- run $ get "/page/s/new"
      WT.simpleStatus missing `shouldBe` Net.status404
      writeFile (cd FP.</> "s" FP.</> "new.md") "uid: new\ntitle: New\n\nHello"
      reloadKiwi ss
      found <- run $ get "/page/s/new"
      WT.simpleStatus found `shouldBe` Net.status200

  it "shows the search results one by line, with an excerpt" $
    withKiwiApp [ ("a.md", "uid: a\ntitle: Alpha\nlang: english\n\nThe horses run in the field")
                , ("b.md", "uid: b\ntitle: Beta\n\nA horse sleeps") ] $ WT.runSession $ do
      resp <- get "/search?query=horse"
      liftIO $ WT.simpleStatus resp `shouldBe` Net.status200
      liftIO $ body resp `shouldSatisfy` T.isInfixOf "<h1>2 results</h1>"
      liftIO $ body resp `shouldSatisfy` T.isInfixOf
        "<li><a href=\"/page/s/a\"><span class=\"title\">Alpha</span>\
        \<p class=\"excerpt\" lang=\"en\">The <strong>horses</strong> run in the field</p>\
        \</a></li>"
      liftIO $ body resp `shouldNotSatisfy` T.isInfixOf "search-more"

  it "shows the page of a uid, its file, and the pages linking it" $
    withKiwiApp [ ("q.md", "uid: Q\ntitle: Target\n\nText")
                , ("a.md", "uid: a\ntitle: Beta\n\n[t](page:Q)")
                , ("b.md", "uid: b\ntitle: alpha\n\n![i](file:QQQQQQQQQQ) [t](<page: Q>)")
                , ("old.md", "uid: old\ntitle: Old\narchive: yes\n\n[t](page:Q)")
                , ("priv.md", "uid: priv\ntitle: Private\naccess: family\n\n[t](page:Q)")
                , ("d/pic__QQQQQQQQQQ.png", "PNG") ] $ WT.runSession $ do
      let result url label source = T.concat
            [ "<li><a href=\"", url, "\"><span class=\"title\">", label
            , "</span><p class=\"excerpt\">", source, "</p></a></li>" ]
      page' <- get "/search?query=uid:%20Q"
      liftIO $ WT.simpleStatus page' `shouldBe` Net.status200
      liftIO $ body page' `shouldSatisfy` T.isInfixOf
        ("<h2>Page</h2><ul class=\"results\">" <> result "/page/s/Q" "Target" "s" <> "</ul>")
      -- by title, without case; not the archived nor the private page
      liftIO $ body page' `shouldSatisfy` T.isInfixOf
        ("<h2>Pages linking this uid</h2><ul class=\"results\">" <>
         result "/page/s/b" "alpha" "s" <> result "/page/s/a" "Beta" "s" <> "</ul>")
      liftIO $ body page' `shouldNotSatisfy` T.isInfixOf "<h2>File</h2>"
      -- the archived page in the archive mode
      archived <- get "/search?query=uid:Q&archive=yes"
      liftIO $ body archived `shouldSatisfy` T.isInfixOf
        ("<ul class=\"results\">" <> result "/page/s/old?archive=yes" "Old" "s" <> "</ul>")
      file <- get "/search?query=uid:QQQQQQQQQQ"
      liftIO $ body file `shouldSatisfy` T.isInfixOf
        "<h2>File</h2><ul class=\"results\"><li><a href=\"/file/s/QQQQQQQQQQ\" \
        \target=\"_blank\"><span class=\"title\">pic.png</span><p class=\"excerpt\">s</p>\
        \</a></li></ul>"
      liftIO $ body file `shouldSatisfy` T.isInfixOf
        ("<h2>Pages linking this uid</h2><ul class=\"results\">" <>
         result "/page/s/b" "alpha" "s" <> "</ul>")
      none <- get "/search?query=uid:nope"
      liftIO $ body none `shouldSatisfy` T.isInfixOf
        "<h2>Pages linking this uid</h2><p>No page for this uid</p>"
      liftIO $ body none `shouldNotSatisfy` T.isInfixOf "<h2>Page</h2>"

  it "tells when other pages match but are not shown" $
    withKiwiApp [ (show n ++ ".md", "title: P\n\nA horse") | n <- [1 .. 13 :: Int] ] $
      WT.runSession $ do
        resp <- get "/search?query=horse"
        liftIO $ body resp `shouldSatisfy` T.isInfixOf "<h1>12 results</h1>"
        liftIO $ body resp `shouldSatisfy` T.isInfixOf
          "<p class=\"search-more\">Other pages match too: refine the search to find them.</p>"

  it "refuses to browse the pages having an unknown metadata" $
    withKiwiApp [] $ WT.runSession $ do
      resp <- get "/browse?has=nope"
      liftIO $ WT.simpleStatus resp `shouldBe` Net.status400

  it "links the name of a custom metadata to the pages having it" $
    withKiwiApp [("a.md", "uid: a\ntitle: A\nauthor: Smith\n\nText")] $ WT.runSession $ do
      page' <- get "/page/s/a"
      liftIO $ body page' `shouldSatisfy` T.isInfixOf
        "<a class=\"field\" title=\"The pages with this metadata\" href=\"/browse?has=author\">author</a>"
      meta <- get "/metadata"
      liftIO $ body meta `shouldSatisfy` T.isInfixOf
        "<legend><a href=\"/browse?source=s&amp;has=author\" class=\"key\" \
        \title=\"The pages with this metadata\">author</a></legend>"
      -- not the other fields
      liftIO $ body meta `shouldSatisfy` T.isInfixOf "<legend>Tags</legend>"

  it "lists the tags, the fields of kiwi, then the custom fields by name" $
    withKiwiAccounts [("ann", "secret", ["admin"])]
                     [("a.md", "uid: a\ntitle: A\ntags: t\nzone: Nord\nauthor: Smith\n\nText")] $
    WT.runSession $ do
      _ <- postForm "/login" "username=ann&password=secret"
      meta <- body <$> get "/metadata"
      let position t = T.length $ fst $ T.breakOn t meta
          legends = [ "Tags</legend>", "Language</legend>", "Access</legend>"
                    , "author</a></legend>", "zone</a></legend>" ]
      liftIO $ map (`T.isInfixOf` meta) legends `shouldBe` map (const True) legends
      liftIO $ map position legends `shouldSatisfy` (\ps -> and $ zipWith (<) ps (drop 1 ps))
      liftIO $ meta `shouldSatisfy` T.isInfixOf "<fieldset class=\"flex internal-meta\"><legend>Language"
