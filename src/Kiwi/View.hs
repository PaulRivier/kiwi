module Kiwi.View
  ( renderPage
  , renderBrowseAll
  , Toggle (..)
  , PagesListing (..)
  , renderSearchResults
  , renderDoc
  , MetaEntry (..)
  , renderMetadata
  , renderLoginForm
  , renderNotFound
  , renderBadRequest
  , renderForbidden
  ) where

import           Control.Monad (forM_, unless, when)
import           Data.List (intersperse, sortOn)
import           Data.Maybe (fromMaybe, isJust)
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import           Data.Tree (Forest, Tree (..))
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
import           Kiwi.Pandoc (linksInContext)


-- | What the layout needs to know about the current request
data ViewCtx = ViewCtx { ctxLocales :: K.Locales
                       , ctxLogin   :: Maybe T.Text   -- ^ user id
                       , ctxIsAdmin :: Bool
                       , ctxToc     :: K.TocSetting
                       , ctxContext :: K.Context      -- ^ navigation context
                       }

getViewCtx :: ActM ViewCtx
getViewCtx = ViewCtx <$> getLocales
                     <*> (fmap fst3 <$> asksK K.login)
                     <*> U.isAdmin
                     <*> asksK K.tocSetting
                     <*> asksK K.context

getLocales :: ActM K.Locales
getLocales = Loc.locales <$> asksK K.uiLang

-- | Admin actions of the navigation bar
data Actions = PageActions K.Page  -- ^ edit and copy a link to the page, reload
             | ReloadAction
             | NoActions           -- ^ documentation views

-- | Renders a complete HTML document: title, admin actions, document of the
-- ToC (if any) and content
renderView :: T.Text -> Actions -> Maybe PD.Pandoc -> Html () -> ActM TL.Text
renderView title actions tocDoc content = do
  ctx <- getViewCtx
  return $ renderText $ layout ctx title actions tocDoc content


-- | A page, its document and the pages linking to it
renderPage :: K.Page -> PD.Pandoc -> [K.Page] -> ActM TL.Text
renderPage p doc backlinks = do
  loc <- getLocales
  ctx <- asksK K.context
  renderView (K.pageTitle p) (PageActions p) (Just doc) (pageView loc ctx p doc backlinks)

-- | A document embedded in kiwi (README...), with its title as heading if
-- given
renderDoc :: T.Text -> Maybe T.Text -> PD.Pandoc -> ActM TL.Text
renderDoc title headingM doc =
  renderView title NoActions (Just doc) $ div_ [class_ "page-view"] $ do
    forM_ headingM $ h1_ [class_ "page-title"] . toHtml
    div_ [class_ "document"] $ writeHtml5 doc

-- | Key of an union field, with a link to add it to the request, or to
-- remove it when it is selected
data Toggle = Toggle { toggleLabel    :: T.Text
                     , toggleTarget   :: T.Text
                     , toggleSelected :: Bool
                     }

-- | Keys of the union fields, by field
type BrowseToggles = [(K.MetaField, [Toggle])]

-- | Links (label, target) to narrow the request, by field
type BrowseMenu = [(K.MetaField, [(T.Text, T.Text)])]

-- | Keys of the request: field, label, and link to remove the key
type BrowseFilters = [(K.MetaField, T.Text, T.Text)]

-- | Pages listed on the browse page
data PagesListing = RecentPages  -- ^ only the latest modified ones
                  | AllPages     -- ^ all the pages matching the request

-- | The browse page, with a link to switch between the archives and the
-- other pages, if any
renderBrowseAll :: BrowseToggles -> Maybe T.Text -> BrowseMenu -> BrowseFilters ->
                   PagesListing -> [K.Page] -> ActM TL.Text
renderBrowseAll toggles switch menu filters listing pages = do
  loc <- getLocales
  ctx <- asksK K.context
  renderView (K.loc_Tags loc) ReloadAction Nothing $
    browseView loc ctx toggles switch menu filters listing pages

renderSearchResults :: [K.Page] -> ActM TL.Text
renderSearchResults pages = do
  loc <- getLocales
  ctx <- asksK K.context
  renderView (K.loc_Search loc) ReloadAction Nothing $ searchView loc ctx pages

-- | A key of a field on the metadata page
data MetaEntry = MetaEntry { metaLabel :: T.Text  -- ^ as displayed
                           , metaCopy  :: T.Text  -- ^ the copied text
                           , metaLink  :: T.Text  -- ^ to browse its pages
                           , metaCount :: Int     -- ^ number of pages
                           }

-- | Metadata of the pages, by source: the keys of each field, with their
-- sub-keys (sub-tags)
renderMetadata :: [(T.Text, [(K.MetaField, Forest MetaEntry)])] -> ActM TL.Text
renderMetadata sources = do
  loc <- getLocales
  renderView (K.loc_Metadata loc) NoActions Nothing $ metadataView loc sources

-- | Login form, with an error message if previous attempt failed
renderLoginForm :: Bool -> ActM TL.Text
renderLoginForm failed = do
  loc <- getLocales
  renderView (K.loc_Login loc) ReloadAction Nothing $ loginView loc failed

renderNotFound :: ActM TL.Text
renderNotFound = do
  loc <- getLocales
  renderView (K.loc_NotFound loc) ReloadAction Nothing $ h1_ $ toHtml $ K.loc_NotFound loc

-- | Invalid request, with the reason
renderBadRequest :: T.Text -> ActM TL.Text
renderBadRequest err = do
  loc <- getLocales
  renderView (K.loc_BadRequest loc) ReloadAction Nothing $ do
    h1_ $ toHtml $ K.loc_BadRequest loc
    p_ $ toHtml err

renderForbidden :: ActM TL.Text
renderForbidden = do
  loc <- getLocales
  renderView (K.loc_Forbidden loc) ReloadAction Nothing $ h1_ $ toHtml $ K.loc_Forbidden loc



-- * Layout

-- | The theme is app.css and app.js, they load the other files they need
layout :: ViewCtx -> T.Text -> Actions -> Maybe PD.Pandoc -> Html () -> Html ()
layout ctx title actions tocDoc content = do
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
      forM_ tocDoc $ toc loc (ctxToc ctx)
      div_ [class_ "layout"] $ do
        navigation ctx actions
        main_ [class_ "page-content"] content
      footer loc (ctxContext ctx)
  where
    loc = ctxLocales ctx


navigation :: ViewCtx -> Actions -> Html ()
navigation ctx actions = nav_ [class_ "top-nav"] $ do
  a_ [ href_ (U.inContext (ctxContext ctx) "/browse"), class_ "icon"
     , title_ (K.loc_Tags loc) ] $ icon "tags"
  div_ [class_ "nav-actions"] $ do
    when (ctxIsAdmin ctx) $ case actions of
      PageActions p -> do
        let (source, pId) = K.pageUID p
        iconButton "edit-page" (K.loc_EditPage loc)
          [data_ "page-source" source, data_ "page-id" pId] "edit"
        copyButton (K.loc_CopyLink loc) (cmLink p)
        reload
      ReloadAction -> reload
      NoActions -> mempty
    searchForm loc (ctxContext ctx) False
  where
    loc = ctxLocales ctx
    reload = iconButton "reload" (K.loc_Reload loc) [] "reload"
    -- commonmark link to the page, to paste in another page
    cmLink p = T.concat ["[", K.pageTitle p, "](<page:", U.pageUIDLink p, ">)"]


-- | Links to the documentation, in new tabs, out of the navigation context,
-- except the archive mode of the metadata
footer :: K.Locales -> K.Context -> Html ()
footer loc ctx = footer_ [class_ "site-footer"] $
  forM_ [ (K.loc_Metadata loc, U.inContext metaCtx "/metadata"), ("README", "/readme")
        , ("CommonMark", "/commonmark") ] $ \(label, target) ->
    a_ [href_ target, target_ "_blank", rel_ "noopener"] $ toHtml label
  where metaCtx = ctx { K.contextSources = S.empty }


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


-- | The search is done in the navigation context
searchForm :: K.Locales -> K.Context -> Bool -> Html ()
searchForm loc ctx focus =
  form_ [action_ "/search", method_ "get", class_ "search"] $ do
    input_ [ name_ "query", type_ "search"
           , placeholder_ (K.loc_Search loc)
           , if focus then autofocus_ else mempty ]
    forM_ (U.contextParams ctx) $ \(n, v) -> input_ [type_ "hidden", name_ n, value_ v]


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

pageView :: K.Locales -> K.Context -> K.Page -> PD.Pandoc -> [K.Page] -> Html ()
pageView loc ctx p doc backlinks = div_ [class_ "page-view"] $ do
  contextBar loc [fst $ K.pageUID p]
    (linksList "tags-list" [ (prettyTag t, browseLink K.FieldTag (K.KeyText t))
                           | t <- K.pageTags p ])
    (K.contextArchive ctx)
  unless (null $ K.pageCustomMeta p) $
    ul_ [class_ "custom-meta-list"] $ forM_ (K.pageCustomMeta p) $ \cmd -> li_ $ do
      span_ [class_ "field"] $ toHtml $ K.cmdName cmd
      " – "
      sequence_ $ intersperse " "
        [ link (U.showKeyLabel k, browseLink (K.FieldCustom $ K.cmdName cmd) k)
        | k <- K.cmdKeys cmd ]
  h1_ [class_ "page-title"] $ toHtml $ K.pageTitle p
  div_ [class_ "document"] $ writeHtml5 $ linksInContext ctx doc
  linksList "pages-list backlinks" $ map (pageLink ctx) backlinks
  where
    browseLink f k = U.browseUrl ctx $ U.addToReq (U.sourcesReq $ K.contextSources ctx) (f, k)


browseView :: K.Locales -> K.Context -> BrowseToggles -> Maybe T.Text -> BrowseMenu ->
              BrowseFilters -> PagesListing -> [K.Page] -> Html ()
browseView loc ctx toggles switch menu filters listing pages = div_ [class_ "tags-page"] $ do
  -- toggling the only source would change nothing, but it is shown with the
  -- archive switch
  when (length sourceToggles > 1 || isJust switch) $
    div_ [class_ "sources-selection"] $ do
      unless (null sourceToggles) $
        ul_ [class_ "flex-hz sources-list"] $ mapM_ toggleItem sourceToggles
      forM_ switch $ \target ->
        a_ [ href_ target
           , class_ $ if K.contextArchive ctx then "archive-switch active"
                      else "archive-switch" ] $
          toHtml $ K.loc_Archives loc
  unless (null filters) $
    div_ [class_ "filters-selection"] $ fieldset (K.loc_FiltersList loc) $
      ul_ [class_ "flex-hz filters-list"] $ forM_ filters $ \(f, label, target) ->
        li_ $ a_ [href_ target, title_ (K.loc_RemoveFilter loc)] $ do
          unless (f == K.FieldTag) $ toHtml (fieldName loc f) <> " : "
          toHtml label <> " ×"
  unless (null tags) $
    div_ [class_ "tags-selection"] $
      fieldset (K.loc_TagsList loc) $ linksList "tags-list" $ sortOn fst tags
  unless (null customs) $
    div_ [class_ "custom-meta-selection"] $ forM_ customs $ \(field, keys) ->
      fieldset field $ linksList "custom-meta-list" keys
  unless (null pages) $
    div_ [class_ "results"] $
      fieldset ("→ " <> pagesLegend) $ linksList "pages-list" $
        map (pageLink ctx) pages
  where
    pagesLegend = case listing of
      RecentPages -> K.loc_RecentPages loc
      AllPages    -> K.loc_PagesList loc
    sourceToggles = fromMaybe [] $ lookup K.FieldSource toggles
    toggleItem :: Toggle -> Html ()
    toggleItem t
      | toggleSelected t = li_ [class_ "selected"] $
          a_ [href_ (toggleTarget t), title_ (K.loc_RemoveFilter loc)] $
            toHtml (toggleLabel t)
      | otherwise = li_ $ link (toggleLabel t, toggleTarget t)
    menuOf f = fromMaybe [] $ lookup f menu
    tags = menuOf K.FieldTag
    customs = [ (n, keys) | (K.FieldCustom n, keys) <- menu, not (null keys) ]
    fieldset :: T.Text -> Html () -> Html ()
    fieldset legend content = fieldset_ [class_ "flex"] $ do
      legend_ $ toHtml legend
      content


searchView :: K.Locales -> K.Context -> [K.Page] -> Html ()
searchView loc ctx pages = div_ [class_ "search-results-page"] $ do
  contextBar loc (S.toList $ K.contextSources ctx) mempty (K.contextArchive ctx)
  case pages of
    [] -> do
      h1_ $ toHtml $ K.loc_SearchNothing loc
      searchForm loc ctx True
    _ -> do
      h1_ $ toHtml $ K.loc_SearchResults loc
      linksList "results" $ map (pageLink ctx) pages


-- | Bar of the navigation context, at the top of a page or of search results:
-- the sources, as selected on the browse page but without link, the archive
-- mode, and some content (the tags of a page)
contextBar :: K.Locales -> [T.Text] -> Html () -> Bool -> Html ()
contextBar loc sources content archive = div_ [class_ "sources-selection"] $ do
  unless (null sources) $
    ul_ [class_ "flex-hz sources-list"] $
      forM_ sources $ li_ [class_ "selected"] . span_ . toHtml
  when archive $
    span_ [class_ "archive-flag"] $ toHtml $ "[" <> K.loc_Archives loc <> "]"
  content


-- | One section by source, one frame by field, one key by line, the sub-keys
-- in a nested list
metadataView :: K.Locales -> [(T.Text, [(K.MetaField, Forest MetaEntry)])] -> Html ()
metadataView loc sources = div_ [class_ "metadata-page"] $ do
  h1_ [class_ "page-title"] $ toHtml $ K.loc_Metadata loc
  forM_ sources $ \(source, fields) -> section_ $ do
    h2_ $ toHtml source
    forM_ fields $ \(field, entries) ->
      fieldset_ [class_ "flex", class_ (fieldClass field)] $ do
        legend_ $ toHtml $ fieldName loc field
        ul_ [class_ "meta-list"] $ mapM_ entryTree entries
  where
    entryTree :: Tree MetaEntry -> Html ()
    entryTree (Node e children) = li_ $ do
      div_ [class_ "meta-entry"] $ do
        a_ [href_ (metaLink e), class_ "key"] $ toHtml $ metaLabel e
        span_ [class_ "count"] $ toHtml $ "(" <> T.pack (show $ metaCount e) <> ")"
        copyTextButton (K.loc_Copy loc) (metaCopy e)
      unless (null children) $ ul_ $ mapM_ entryTree children
    fieldClass K.FieldTag = "tags-meta"
    fieldClass _          = "other-meta"


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

-- | Name of a field, as displayed
fieldName :: K.Locales -> K.MetaField -> T.Text
fieldName loc f = case f of
  K.FieldTag      -> K.loc_Tags loc
  K.FieldLang     -> K.loc_Lang loc
  K.FieldAccess   -> K.loc_Access loc
  K.FieldCustom n -> n
  _               -> T.pack $ show f

-- | Link from (label, target)
link :: (T.Text, T.Text) -> Html ()
link (label, target) = a_ [href_ target] $ toHtml label

-- | Link to a page, in the navigation context
pageLink :: K.Context -> K.Page -> (T.Text, T.Text)
pageLink ctx page = (K.pageTitle page, U.inContext ctx $ U.pageUrl page)

-- | Button handled by app.js
iconButton :: T.Text -> T.Text -> [Attributes] -> T.Text -> Html ()
iconButton cls label attrs name =
  button_ ([type_ "button", class_ ("icon " <> cls), title_ label] <> attrs) $
    icon name

-- | Button copying the text to the clipboard, handled by app.js
copyButton :: T.Text -> T.Text -> Html ()
copyButton label txt = iconButton "copy" label [data_ "copy" txt] "copy-link"

-- | Copy button with a text glyph instead of the link icon, for a text that is
-- not a link
copyTextButton :: T.Text -> T.Text -> Html ()
copyTextButton label txt =
  button_ [type_ "button", class_ "icon copy", title_ label, data_ "copy" txt] $
    span_ [class_ "glyph"] "\x29C9"

icon :: T.Text -> Html ()
icon name = img_ [src_ ("/static/images/" <> name <> ".svg"), alt_ name]
