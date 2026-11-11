module Kiwi.ViewSpec (viewSpec) where

import qualified Data.Text as T
import           Control.Monad.IO.Class (liftIO)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import           Lucid (renderText)
import qualified Network.HTTP.Types as Net
import qualified Network.Wai as Wai
import qualified Network.Wai.Test as WT
import           Test.Hspec
import qualified Text.Pandoc.Definition as PD
import qualified Text.Pandoc.Walk as PW

import qualified Kiwi.Locales as Loc
import           Kiwi.Server (securityHeaders)
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
