module Kiwi.View
  ( renderPage
  , renderBrowseAll
  , renderSearchResults
  , renderLoginForm
  , renderNotFound
  , renderBadRequest
  , renderForbidden
  ) where

import           Control.Monad (forM_, unless, when)
import           Data.List (intersperse, sortOn)
import           Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import           Lucid

import qualified Kiwi.Locales as Loc
import qualified Kiwi.Types as K
import           Kiwi.Types (ActM)
import           Kiwi.Utils (prettyTag, asksK, fst3)
import qualified Kiwi.Utils as U
import           Kiwi.HtmlWriter (writeHtml5)


-- | What the layout needs to know about the current request
data ViewCtx = ViewCtx { ctxLocales :: K.Locales
                       , ctxLogin   :: Maybe T.Text   -- ^ user id
                       , ctxIsAdmin :: Bool
                       , ctxToc     :: K.TocSetting
                       }

getViewCtx :: ActM ViewCtx
getViewCtx = ViewCtx <$> getLocales
                     <*> (fmap fst3 <$> asksK K.login)
                     <*> U.isAdmin
                     <*> asksK K.tocSetting

getLocales :: ActM K.Locales
getLocales = Loc.locales <$> asksK K.uiLang

-- | Renders a complete HTML document: title, page displayed (if any, for the
-- admin menu) and content
renderView :: T.Text -> Maybe K.Page -> Html () -> ActM TL.Text
renderView title pageM content = do
  ctx <- getViewCtx
  return $ renderText $ layout ctx title pageM content


renderPage :: K.Page -> [K.Page] -> ActM TL.Text
renderPage p backlinks =
  renderView (K.pageTitle p) (Just p) (pageView p backlinks)

-- | Links (label, target) to narrow the request, by field
type BrowseMenu = [(K.MetaField, [(T.Text, T.Text)])]

-- | Keys of the request: field, label, and link to remove the key
type BrowseFilters = [(K.MetaField, T.Text, T.Text)]

renderBrowseAll :: BrowseMenu -> BrowseFilters -> [K.Page] -> ActM TL.Text
renderBrowseAll menu filters pages = do
  loc <- getLocales
  renderView (K.loc_Tags loc) Nothing $ browseView loc menu filters pages

renderSearchResults :: [K.Page] -> ActM TL.Text
renderSearchResults pages = do
  loc <- getLocales
  renderView (K.loc_Search loc) Nothing $ searchView loc pages

-- | Login form, with an error message if previous attempt failed
renderLoginForm :: Bool -> ActM TL.Text
renderLoginForm failed = do
  loc <- getLocales
  renderView "Log-in" Nothing $ loginView loc failed

renderNotFound :: ActM TL.Text
renderNotFound = do
  loc <- getLocales
  renderView (K.loc_NotFound loc) Nothing $ h1_ $ toHtml $ K.loc_NotFound loc

-- | Invalid request, with the reason
renderBadRequest :: T.Text -> ActM TL.Text
renderBadRequest err = do
  loc <- getLocales
  renderView (K.loc_BadRequest loc) Nothing $ do
    h1_ $ toHtml $ K.loc_BadRequest loc
    p_ $ toHtml err

renderForbidden :: ActM TL.Text
renderForbidden = do
  loc <- getLocales
  renderView (K.loc_Forbidden loc) Nothing $ h1_ $ toHtml $ K.loc_Forbidden loc



-- * Layout

layout :: ViewCtx -> T.Text -> Maybe K.Page -> Html () -> Html ()
layout ctx title pageM content = doctypehtml_ $ do
  head_ $ do
    meta_ [charset_ "utf-8"]
    meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1"]
    meta_ [name_ "description", content_ "Kiwi is a powerful personal wiki system"]
    title_ $ toHtml title
    link_ [rel_ "icon", href_ "/static/favicon.ico"]
    link_ [rel_ "stylesheet", href_ "/static/css/app.css"]
  body_ $ do
    div_ [class_ "left-column"] $
      div_ [class_ "js-toc", data_ "setting" (K.tocSettingValue $ ctxToc ctx)] mempty
    div_ [class_ "main-column"] $ do
      navigation ctx pageM
      div_ [class_ "page-content"] content
    div_ [class_ "right-column"] $ loginBox ctx
    forM_ ["jquery-3.5.0.min.js", "lity.js", "tocbot.min.js", "app.js"] $ \js ->
      script_ [src_ ("/static/js/" <> js)] T.empty


navigation :: ViewCtx -> Maybe K.Page -> Html ()
navigation ctx pageM = nav_ [class_ "top-nav"] $ div_ [class_ "nav-content"] $ do
  ul_ [class_ "nav-items nav-left"] $
    navItem $ a_ [href_ "/browse", class_ "nav-img"] $ icon "tags"
  ul_ [class_ "nav-items nav-right"] $ do
    when (ctxIsAdmin ctx) $ do
      forM_ pageM $ \p -> do
        let (source, pId) = K.pageUID p
        navItem $ a_ [ class_ "nav-img edit-page"
                     , data_ "page-source" source, data_ "page-id" pId ] $
          icon "edit"
        navItem $ a_ [class_ "nav-img copy-link", data_ "page-link" (cmLink p)] $
          icon "copy-link"
      navItem $ a_ [class_ "nav-img reload"] $ icon "reload"
    li_ $ searchForm (ctxLocales ctx) False
  where
    navItem = li_ [class_ "nav-item"]
    -- commonmark link to the page, to paste in another page
    cmLink p = T.concat ["[", K.pageTitle p, "](<page:", U.pageUIDLink p, ">)"]


loginBox :: ViewCtx -> Html ()
loginBox ctx = div_ [class_ "layout-login"] $ case ctxLogin ctx of
  Just user -> do
    span_ [class_ "username"] $ toHtml user
    a_ [class_ "nav-img logout"] $ icon "logout"
  Nothing -> a_ [href_ "/login", class_ "login"] "login"


searchForm :: K.Locales -> Bool -> Html ()
searchForm loc focus =
  form_ [action_ "/search", method_ "get", class_ "search pure-form"] $
    input_ [ name_ "query", type_ "search", class_ "search"
           , placeholder_ (K.loc_Search loc)
           , if focus then autofocus_ else mempty ]



-- * Views

pageView :: K.Page -> [K.Page] -> Html ()
pageView p backlinks = div_ [class_ "page-view"] $ do
  linksList "tags-list" [ (prettyTag t, browseLink K.FieldTag (K.KeyText t))
                        | t <- K.pageTags p ]
  unless (null $ K.pageCustomMeta p) $
    ul_ [class_ "custom-meta-list"] $ forM_ (K.pageCustomMeta p) $ \cmd -> li_ $ do
      span_ [class_ "field"] $ toHtml $ K.cmdName cmd
      " – "
      sequence_ $ intersperse " "
        [ link (U.showKeyLabel k, browseLink (K.FieldCustom $ K.cmdName cmd) k)
        | k <- K.cmdKeys cmd ]
  h1_ [class_ "page-title"] $ toHtml $ K.pageTitle p
  div_ [class_ "document"] $ toHtmlRaw $ writeHtml5 (K.pageDoc p)
  linksList "pages-list backlinks" $ map pageLink backlinks
  where
    browseLink f k = U.browseUrl $ U.createReq (f, k)


browseView :: K.Locales -> BrowseMenu -> BrowseFilters -> [K.Page] -> Html ()
browseView loc menu filters pages = div_ [class_ "tags-page"] $ do
  unless (null filters) $
    div_ [class_ "filters-selection"] $
      ul_ [class_ "flex-hz filters-list"] $ forM_ filters $ \(f, label, target) ->
        li_ $ a_ [href_ target, title_ (K.loc_RemoveFilter loc)] $ do
          forM_ (customName f) $ \n -> toHtml n <> " : "
          toHtml label <> " ×"
  when (length sources > 1) $
    div_ [class_ "sources-selection"] $ linksList "sources-list" sources
  unless (null tags) $
    div_ [class_ "tags-selection"] $
      fieldset (K.loc_TagsList loc) $ linksList "tags-list" $ sortOn fst tags
  unless (null customs) $
    div_ [class_ "custom-meta-selection"] $ forM_ customs $ \(field, keys) ->
      fieldset field $ linksList "custom-meta-list" keys
  unless (null pages) $
    div_ [class_ "results"] $
      fieldset ("→ " <> K.loc_PagesList loc) $ linksList "pages-list" $
        map pageLink pages
  where
    menuOf f = fromMaybe [] $ lookup f menu
    sources = menuOf K.FieldSource
    tags = menuOf K.FieldTag
    customs = [ (n, keys) | (K.FieldCustom n, keys) <- menu, not (null keys) ]
    -- the field name is only displayed for custom fields
    customName (K.FieldCustom n) = Just n
    customName _ = Nothing
    fieldset :: T.Text -> Html () -> Html ()
    fieldset legend content = fieldset_ [class_ "flex"] $ do
      legend_ $ toHtml legend
      content


searchView :: K.Locales -> [K.Page] -> Html ()
searchView loc pages = div_ [class_ "search-results-page"] $ case pages of
  [] -> do
    h1_ $ toHtml $ K.loc_SearchNothing loc
    searchForm loc True
  _ -> do
    h1_ $ toHtml $ K.loc_SearchResults loc
    linksList "results" $ map pageLink pages


loginView :: K.Locales -> Bool -> Html ()
loginView loc failed =
  div_ [id_ "login-box"] $ div_ [class_ "login-form"] $
    form_ [action_ "/login", method_ "post", class_ "login pure-form"] $ do
      when failed $ p_ [class_ "login-error"] $ toHtml $ K.loc_LoginFailed loc
      ul_ [class_ "flex-vz"] $ do
        li_ $ input_ [ name_ "username", type_ "text", placeholder_ "Username"
                     , class_ "login-id", required_ "" ]
        li_ $ input_ [ name_ "password", type_ "password", placeholder_ "Password"
                     , class_ "login-password", required_ "" ]
        li_ $ button_ [type_ "submit", class_ "pure-button pure-button-primary"]
          "Log in"



-- * Helpers

-- | Horizontal list of links, nothing at all when there is no link
linksList :: T.Text -> [(T.Text, T.Text)] -> Html ()
linksList cls links = unless (null links) $
  ul_ [class_ "flex-hz", class_ cls] $ mapM_ (li_ . link) links

-- | Link from (label, target)
link :: (T.Text, T.Text) -> Html ()
link (label, target) = a_ [href_ target] $ toHtml label

pageLink :: K.Page -> (T.Text, T.Text)
pageLink page = (K.pageTitle page, U.pageUrl page)

icon :: T.Text -> Html ()
icon name = img_ [src_ ("/static/images/" <> name <> ".svg"), alt_ name]
