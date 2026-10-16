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
import           Lucid.Base (makeAttributes)
import qualified Text.Pandoc.Definition as PD
import qualified Text.Pandoc.Walk as PW

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
  renderView (K.loc_Login loc) Nothing $ loginView loc failed

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

-- | The theme is app.css and app.js, they load the other files they need
layout :: ViewCtx -> T.Text -> Maybe K.Page -> Html () -> Html ()
layout ctx title pageM content = do
  doctype_
  html_ [lang_ (K.loc_LangCode loc)] $ do
    head_ $ do
      meta_ [charset_ "utf-8"]
      meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1"]
      meta_ [name_ "description", content_ "Kiwi is a powerful personal wiki system"]
      title_ $ toHtml title
      link_ [rel_ "icon", href_ "/static/favicon.ico"]
      link_ [rel_ "stylesheet", href_ "/static/css/app.css"]
      script_ [type_ "module", src_ "/static/js/app.js"] T.empty
    body_ $ do
      loginBox ctx
      forM_ pageM $ toc loc (ctxToc ctx) . K.pageDoc
      div_ [class_ "layout"] $ do
        navigation ctx pageM
        main_ [class_ "page-content"] content
  where
    loc = ctxLocales ctx


navigation :: ViewCtx -> Maybe K.Page -> Html ()
navigation ctx pageM = nav_ [class_ "top-nav"] $ do
  a_ [href_ "/browse", class_ "icon", title_ (K.loc_Tags loc)] $ icon "tags"
  div_ [class_ "nav-actions"] $ do
    when (ctxIsAdmin ctx) $ do
      forM_ pageM $ \p -> do
        let (source, pId) = K.pageUID p
        iconButton "edit-page" (K.loc_EditPage loc)
          [data_ "page-source" source, data_ "page-id" pId] "edit"
        iconButton "copy-link" (K.loc_CopyLink loc)
          [data_ "page-link" (cmLink p)] "copy-link"
      iconButton "reload" (K.loc_Reload loc) [] "reload"
    searchForm loc False
  where
    loc = ctxLocales ctx
    -- commonmark link to the page, to paste in another page
    cmLink p = T.concat ["[", K.pageTitle p, "](<page:", U.pageUIDLink p, ">)"]
    -- the actions are handled by app.js
    iconButton :: T.Text -> T.Text -> [Attributes] -> T.Text -> Html ()
    iconButton cls label attrs name =
      button_ ([type_ "button", class_ ("icon " <> cls), title_ label] <> attrs) $
        icon name


loginBox :: ViewCtx -> Html ()
loginBox ctx = div_ [class_ "layout-login"] $ case ctxLogin ctx of
  Just user -> do
    span_ [class_ "username"] $ toHtml user
    form_ [action_ "/logout", method_ "post"] $
      button_ [type_ "submit", class_ "icon logout", title_ (K.loc_Logout loc)] $
        icon "logout"
  Nothing -> a_ [href_ "/login", class_ "login"] $ toHtml $ K.loc_Login loc
  where
    loc = ctxLocales ctx


searchForm :: K.Locales -> Bool -> Html ()
searchForm loc focus =
  form_ [action_ "/search", method_ "get", class_ "search"] $
    input_ [ name_ "query", type_ "search"
           , placeholder_ (K.loc_Search loc)
           , if focus then autofocus_ else mempty ]


-- | Table of contents of the document headings (levels 1&2), with the
-- setting "auto", only for documents with many headings
toc :: K.Locales -> K.TocSetting -> PD.Pandoc -> Html ()
toc loc setting doc = when visible $
  nav_ [class_ "toc", makeAttributes "aria-label" (K.loc_Contents loc)] $
    tocList headings
  where
    headings = PW.query heading doc
    heading (PD.Header lvl (ident, _, _) ils)
      | lvl <= 2, not (T.null ident) = [(lvl, ident, plainText ils)]
    heading _ = []
    visible = case setting of
      K.TocOn   -> not (null headings)
      K.TocAuto -> length headings > 5
      K.TocOff  -> False

-- | Nested lists : a heading contains the following deeper headings
tocList :: [(Int, T.Text, T.Text)] -> Html ()
tocList hs = unless (null hs) $ ul_ $ go hs
  where
    go :: [(Int, T.Text, T.Text)] -> Html ()
    go [] = mempty
    go ((lvl, ident, label) : rest) =
      let (sub, next) = span (\(l, _, _) -> l > lvl) rest
      in li_ (a_ [href_ ("#" <> ident)] (toHtml label) <> tocList sub) <> go next

-- | Text of inlines, without formatting
plainText :: [PD.Inline] -> T.Text
plainText = PW.query txt
  where
    txt (PD.Str t)    = t
    txt (PD.Code _ t) = t
    txt (PD.Math _ t) = t
    txt PD.Space      = " "
    txt PD.SoftBreak  = " "
    txt PD.LineBreak  = " "
    txt _             = ""



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
  form_ [action_ "/login", method_ "post", class_ "login"] $ do
    when failed $ p_ [class_ "login-error"] $ toHtml $ K.loc_LoginFailed loc
    input_ [ name_ "username", type_ "text", placeholder_ (K.loc_Username loc)
           , autofocus_, required_ "" ]
    input_ [ name_ "password", type_ "password", placeholder_ (K.loc_Password loc)
           , required_ "" ]
    button_ [type_ "submit"] $ toHtml $ K.loc_Login loc



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
