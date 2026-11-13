module Kiwi.View
  ( renderPage
  , renderBrowseAll
  , Toggle (..)
  , BrowseMenu
  , BrowseFilters
  , PagesListing (..)
  , renderSearchResults
  , renderDoc
  , renderEditor
  , renderPreview
  , MetaEntry (..)
  , renderMetadata
  , renderDoctor
  , renderLoginForm
  , renderNotFound
  , renderBadRequest
  , renderForbidden
  , renderServerError
  , cmLink     -- exported for tests
  , pageView   -- exported for tests
  ) where

import           Control.Monad (forM_, unless, when)
import           Data.List (intersperse)
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
import qualified Kiwi.Auth as Auth
import qualified Kiwi.Browse as Browse
import           Kiwi.Tags (prettyTag)
import qualified Kiwi.Url as Url
import           Kiwi.Utils (asksK, inlinesText)
import           Kiwi.HtmlWriter (writeHtml5)
import           Kiwi.Pandoc (linksInContext)


-- | What the layout needs to know about the current request
data ViewCtx = ViewCtx { ctxLocales :: K.Locales
                       , ctxLogin   :: Maybe T.Text   -- ^ user id
                       , ctxIsAdmin :: Bool
                       , ctxCanEdit :: Bool           -- ^ an editor is set
                       , ctxToc     :: K.TocSetting
                       , ctxContext :: K.Context      -- ^ navigation context
                       , ctxQuery   :: T.Text         -- ^ in the search field
                       , ctxEditor  :: Bool           -- ^ the page has an editor
                       }

getViewCtx :: ActM ViewCtx
getViewCtx = ViewCtx <$> getLocales
                     <*> (fmap K.loginUser <$> asksK (K.login . K.requestState))
                     <*> Auth.isAdmin
                     <*> (isJust <$> asksK K.editorCommand)
                     <*> asksK K.tocSetting
                     <*> asksK (K.context . K.requestState)
                     <*> pure T.empty
                     <*> pure False

getLocales :: ActM K.Locales
getLocales = Loc.locales <$> asksK K.uiLang

-- | Actions of the navigation bar
data Actions = PageActions K.Page Bool  -- ^ edit the page in the browser (with
                                        -- the write right) or with the
                                        -- editor (admins), copy a link to
                                        -- it, reload
             | ReloadAction
             | NoActions           -- ^ documentation views

-- | Renders a complete HTML document: title, admin actions, document of the
-- ToC (if any) and content
renderView :: T.Text -> Actions -> Maybe PD.Pandoc -> Html () -> ActM TL.Text
renderView title actions tocDoc content = do
  ctx <- getViewCtx
  return $ renderText $ layout ctx title actions tocDoc content


-- | A page, its document and the pages linking to it. The links of the
-- document to the pages that do not exist (according to the predicate) are
-- marked as broken.
renderPage :: (K.PageUID -> Bool) -> K.Page -> Bool -> PD.Pandoc -> [K.Page] -> ActM TL.Text
renderPage pageExists p canWrite doc backlinks = do
  loc <- getLocales
  ctx <- asksK (K.context . K.requestState)
  renderView (K.pageTitle p) (PageActions p canWrite) (Just doc)
             (pageView loc ctx pageExists p canWrite doc backlinks)

-- | Editor of a page: the text of its file, its hash, and the line where
-- the editor starts (from 1)
renderEditor :: K.Page -> T.Text -> T.Text -> Int -> ActM TL.Text
renderEditor p text hash line = do
  vctx <- getViewCtx
  let loc = ctxLocales vctx
  return $ renderText $
    layout vctx { ctxEditor = True } (K.loc_Editing loc <> " : " <> K.pageTitle p)
           NoActions Nothing $
      editorView loc p text hash line

-- | HTML of a document previewed in the editor, with the links to the pages
-- that do not exist (according to the predicate) marked as broken
renderPreview :: (K.PageUID -> Bool) -> PD.Pandoc -> TL.Text
renderPreview pageExists doc = renderText $ writeHtml5 pageExists doc

-- | A document embedded in kiwi (README...), with its title as heading if
-- given
renderDoc :: T.Text -> Maybe T.Text -> PD.Pandoc -> ActM TL.Text
renderDoc title headingM doc =
  renderView title NoActions (Just doc) $ div_ [class_ "page-view"] $ do
    forM_ headingM $ h1_ [class_ "page-title"] . toHtml
    -- links of embedded documents are not relative to any source
    div_ [class_ "document"] $ writeHtml5 (const True) doc

-- | Key of an union field, with a link to add it to the request, or to
-- remove it when it is selected
data Toggle = Toggle { toggleLabel    :: T.Text
                     , toggleTarget   :: T.Text
                     , toggleSelected :: Bool
                     }
  deriving (Eq, Show)

-- | Keys of the union fields, by field
type BrowseToggles = [(K.MetaField, [Toggle])]

-- | Links (label, target) to narrow the request, by field, with a link to
-- the pages having the field, if any
type BrowseMenu = [(K.MetaField, Maybe T.Text, [(T.Text, T.Text)])]

-- | Keys of the request: field, label of the key (none for the pages having
-- the field), and link to remove it
type BrowseFilters = [(K.MetaField, Maybe T.Text, T.Text)]

-- | Pages listed on the browse page
data PagesListing = RecentPages  -- ^ only the latest modified ones
                  | AllPages     -- ^ all the pages matching the request
  deriving (Eq, Show)

-- | The browse page, with a link to switch between the archives and the
-- other pages, if any
renderBrowseAll :: BrowseToggles -> Maybe T.Text -> BrowseMenu -> BrowseFilters ->
                   PagesListing -> [K.Page] -> ActM TL.Text
renderBrowseAll toggles switch menu filters listing pages = do
  loc <- getLocales
  ctx <- asksK (K.context . K.requestState)
  renderView (K.loc_Tags loc) ReloadAction Nothing $
    browseView loc ctx toggles switch menu filters listing pages

-- | Results of the query, also in the search field, with the lang it was
-- processed in: the pages, each one with an excerpt of its content (see
-- 'Kiwi.Search.excerpt'), and True when other pages match but are not shown
renderSearchResults :: K.Lang -> T.Text -> [(K.Page, [(T.Text, Bool)])] -> Bool ->
                       ActM TL.Text
renderSearchResults lang query pages more = do
  vctx <- getViewCtx
  let loc = ctxLocales vctx
  return $ renderText $
    layout vctx { ctxQuery = query } (K.loc_Search loc) ReloadAction Nothing $
      searchView loc (ctxContext vctx) lang query pages more

-- | The pages with links to pages that the user can not read, with these
-- links (page ids), source by source
renderDoctor :: [(T.Text, [(FilePath, T.Text)])] -> [(T.Text, [(K.Page, [T.Text])])] ->
                ActM TL.Text
renderDoctor errors broken = do
  loc <- getLocales
  ctx <- asksK (K.context . K.requestState)
  renderView (K.loc_Doctor loc) NoActions Nothing $ doctorView loc ctx errors broken

-- | A key of a field on the metadata page
data MetaEntry = MetaEntry { metaLabel :: T.Text  -- ^ as displayed
                           , metaCopy  :: T.Text  -- ^ the copied text
                           , metaLink  :: T.Text  -- ^ to browse its pages
                           , metaCount :: Int     -- ^ number of pages
                           }

-- | Metadata of the pages, by source: the fields, with a link to the pages
-- having them, if any, and their keys with their sub-keys (sub-tags)
renderMetadata :: [(T.Text, [(K.MetaField, Maybe T.Text, Forest MetaEntry)])] -> ActM TL.Text
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

-- | An error of the server: its cause is in the logs, not on the page
renderServerError :: ActM TL.Text
renderServerError = do
  loc <- getLocales
  renderView (K.loc_ServerError loc) ReloadAction Nothing $
    h1_ $ toHtml $ K.loc_ServerError loc



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
      -- the markdown editor (EasyMDE) runs before app.js, which uses it; its
      -- styles come first, the theme overrides them
      when (ctxEditor ctx) $ do
        link_ [rel_ "stylesheet", href_ "/static/vendor/easymde.min.css"]
        script_ [src_ "/static/vendor/easymde.min.js", defer_ ""] T.empty
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
  a_ [ href_ (Url.inContext (ctxContext ctx) "/browse"), class_ "icon"
     , title_ (K.loc_Tags loc) ] $ icon "tags"
  div_ [class_ "nav-actions"] $ do
    case actions of
      PageActions p canWrite -> do
        let (source, pId) = K.pageUID p
        when canWrite $
          a_ [ href_ (Url.editUrl $ K.pageUID p), class_ "icon edit-page"
             , title_ (K.loc_EditPage loc) ] $ icon "edit"
        -- without editor in the config, the local edition is disabled
        when (admin && ctxCanEdit ctx) $
          iconButton "edit-locally" (K.loc_EditLocally loc)
            ([data_ "page-source" source, data_ "page-id" pId] <> alerts) "edit-local"
        when (admin || canWrite) $ copyButton (K.loc_CopyLink loc) (cmLink p)
        when admin reload
      ReloadAction -> when admin reload
      NoActions -> mempty
    searchForm loc (ctxContext ctx) (ctxQuery ctx) False
  where
    loc = ctxLocales ctx
    admin = ctxIsAdmin ctx
    reload = iconButton "reload" (K.loc_Reload loc) alerts "reload"
    -- texts of the alerts shown by app.js when the action fails
    alerts = [ data_ "error-session" (K.loc_ActionSession loc)
             , data_ "error-forbidden" (K.loc_ActionForbidden loc)
             , data_ "error-failed" (K.loc_ActionFailed loc) ]

-- | CommonMark link to the page, to paste in another page. The brackets of the
-- title are escaped (and the backslashes, which would escape them).
cmLink :: K.Page -> T.Text
cmLink p = T.concat ["[", escape (K.pageTitle p), "](<page:", Url.pageUIDLink p, ">)"]
  where
    escape = T.concatMap $ \c -> if c `elem` ['[', ']', '\\'] then T.pack ['\\', c]
                                 else T.singleton c


-- | Links to the documentation, in new tabs, out of the navigation context,
-- except the archive mode of the metadata and of the doctor
footer :: K.Locales -> K.Context -> Html ()
footer loc ctx = footer_ [class_ "site-footer"] $
  forM_ [ (K.loc_Metadata loc, Url.inContext metaCtx "/metadata")
        , (K.loc_Doctor loc, Url.inContext metaCtx "/doctor")
        , ("README", "/readme"), ("CommonMark", "/commonmark") ] $ \(label, target) ->
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


-- | The search is done in the navigation context. The field has the given
-- query.
searchForm :: K.Locales -> K.Context -> T.Text -> Bool -> Html ()
searchForm loc ctx query focus =
  form_ [action_ "/search", method_ "get", class_ "search"] $ do
    input_ [ name_ "query", type_ "search"
           , if T.null query then mempty else value_ query
           , placeholder_ (K.loc_Search loc)
           , if focus then autofocus_ else mempty ]
    forM_ (Url.contextParams ctx) $ \(n, v) -> input_ [type_ "hidden", name_ n, value_ v]


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
plainText = inlinesText



-- * Views

-- | A page. With the write right, its document has the URL of its editor,
-- to edit it from one of its blocks (see 'Kiwi.HtmlWriter.lineKey').
pageView :: K.Locales -> K.Context -> (K.PageUID -> Bool) -> K.Page -> Bool -> PD.Pandoc ->
            [K.Page] -> Html ()
pageView loc ctx pageExists p canWrite doc backlinks = div_ [class_ "page-view"] $ do
  contextBar loc [fst $ K.pageUID p]
    (linksList "tags-list" [ (prettyTag t, browseLink K.FieldTag (K.KeyText t))
                           | t <- K.pageTags p ])
    (K.contextArchive ctx)
  h1_ [class_ "page-title", pageLang] $ toHtml $ K.pageTitle p
  -- custom metadata as a two-column table: the names of the fields (links to
  -- the pages having them), then their values
  unless (null $ K.pageCustomMeta p) $
    fieldset_ [class_ "flex page-meta"] $
      dl_ [class_ "page-meta-list"] $ forM_ (K.pageCustomMeta p) $ \cmd -> do
        dt_ $ a_ [ class_ "field", title_ (K.loc_PagesWithField loc)
                 , href_ $ Browse.browseUrl ctx $ Browse.addFieldToReq contextReq
                                                    (K.FieldCustom $ K.cmdName cmd) ] $
          toHtml $ K.cmdName cmd
        dd_ $ sequence_ $ intersperse " "
          [ link (Browse.showKeyLabel k, browseLink (K.FieldCustom $ K.cmdName cmd) k)
          | k <- K.cmdKeys cmd ]
  div_ ([class_ "document", pageLang] ++
        if canWrite then [ data_ "edit-url" (Url.editUrl $ K.pageUID p)
                         , data_ "edit-title" (K.loc_EditBlock loc) ] else []) $
    writeHtml5 pageExists $ linksInContext ctx doc
  unless (null backlinks) $
    fieldset_ [class_ "flex backlinks"] $ do
      legend_ $ toHtml $ K.loc_Backlinks loc
      linksList "pages-list backlinks" $ map (pageLink ctx) backlinks
  where
    -- the title and the document are in the lang of the page, the rest in
    -- the lang of the interface
    pageLang = lang_ $ K.langCode $ K.pageLang p
    contextReq = Browse.sourcesReq $ K.contextSources ctx
    browseLink f k = Browse.browseUrl ctx $ Browse.addToReq contextReq (f, k)


-- | The editor of a page (see 'renderEditor'): a form, turned into a
-- markdown editor by app.js, with the texts of its buttons and messages.
editorView :: K.Locales -> K.Page -> T.Text -> T.Text -> Int -> Html ()
editorView loc p text hash line =
  form_ ([ class_ "page-editor", method_ "post", action_ "/save-page"
         , data_ "page-url" (Url.pageUrl p), data_ "line" (T.pack $ show line)
         , data_ "error-session" (K.loc_ActionSession loc)
         , data_ "error-forbidden" (K.loc_ActionForbidden loc)
         , data_ "error-failed" (K.loc_ActionFailed loc)
         , data_ "error-conflict" (K.loc_SaveConflict loc)
         , data_ "error-invalid" (K.loc_SaveInvalid loc)
         , data_ "warning-ignored" (K.loc_SaveIgnored loc) ] ++
         [ data_ ("button-" <> name) label | (name, label) <- K.loc_EditorButtons loc ]) $ do
    h1_ [class_ "page-title"] $ do
      span_ [class_ "editing"] $ toHtml (K.loc_Editing loc) <> " : "
      toHtml (K.pageTitle p)
    input_ [type_ "hidden", name_ "source", value_ source]
    input_ [type_ "hidden", name_ "id", value_ pId]
    input_ [type_ "hidden", name_ "hash", value_ hash]
    -- the browser drops a newline at the start of a textarea
    textarea_ [name_ "text", class_ "editor-text", rows_ "30"] $ toHtml ("\n" <> text)
    div_ [class_ "editor-actions"] $ do
      button_ [type_ "submit", class_ "save"] $ toHtml $ K.loc_Save loc
      a_ [href_ (Url.pageUrl p), class_ "cancel"] $ toHtml $ K.loc_Cancel loc
  where (source, pId) = K.pageUID p

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
        li_ $ a_ [href_ target, title_ (K.loc_RemoveFilter loc)] $ case label of
          Nothing -> toHtml (fieldName loc f) <> " ×"
          Just l  -> do
            unless (f == K.FieldTag) $ toHtml (fieldName loc f) <> " : "
            toHtml l <> " ×"
  unless (null tags) $
    div_ [class_ "tags-selection"] $
      fieldset (K.loc_TagsList loc) $ linksList "tags-list" tags
  -- the name of a field is a link to the pages having it
  unless (null customs) $
    div_ [class_ "custom-meta-selection"] $ forM_ customs $ \(field, fieldLink, keys) ->
      fieldsetWith (maybe (toHtml field)
                          (\l -> a_ [href_ l, title_ (K.loc_PagesWithField loc)] $ toHtml field)
                          fieldLink) $
        linksList "custom-meta-list" keys
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
    tags = concat [ keys | (K.FieldTag, _, keys) <- menu ]
    customs = [ (n, l, keys) | (K.FieldCustom n, l, keys) <- menu, not (null keys) ]
    fieldset :: T.Text -> Html () -> Html ()
    fieldset = fieldsetWith . toHtml
    fieldsetWith :: Html () -> Html () -> Html ()
    fieldsetWith legend content = fieldset_ [class_ "flex"] $ do
      legend_ legend
      content


-- | The results of the query, with their number (a single result is
-- displayed directly, see 'Kiwi.Controller.serveSearch'), and a line after
-- them when other pages match but are not shown
searchView :: K.Locales -> K.Context -> K.Lang -> T.Text -> [(K.Page, [(T.Text, Bool)])] ->
              Bool -> Html ()
searchView loc ctx lang query pages more = div_ [class_ "search-results-page"] $ do
  contextBar loc (S.toList $ K.contextSources ctx) mempty (K.contextArchive ctx)
  case pages of
    [] -> do
      h1_ $ toHtml $ K.loc_SearchNothing loc
      searched
      searchForm loc ctx query True
    _ -> do
      h1_ $ toHtml $ T.concat [T.pack (show $ length pages), " ", K.loc_SearchResults loc]
      searched
      -- one result by line, a single link: the title, then the excerpt, in
      -- the lang of the page, with the words of the query in bold
      ul_ [class_ "results"] $ forM_ pages $ \(pg, fragments) ->
        li_ $ a_ [href_ $ Url.inContext ctx $ Url.pageUrl pg] $ do
          span_ [class_ "title"] $ toHtml $ K.pageTitle pg
          unless (null fragments) $
            p_ [class_ "excerpt", lang_ (K.langCode $ K.pageLang pg)] $
              sequence_ $ intersperse " "
                [ if hit then strong_ (toHtml t) else toHtml t | (t, hit) <- fragments ]
      when more $ p_ [class_ "search-more"] $ toHtml $ K.loc_SearchMore loc
  where
    searched = unless (T.null $ T.strip query) $
      p_ [class_ "search-query"] $ do
        toHtml $ T.concat [ K.loc_SearchQuery loc, " [", K.langName lang, "]"
                          , K.loc_Colon loc, " " ]
        strong_ $ toHtml query


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
-- | The pages with broken links, each one with the ids of its broken links,
-- in a section by source
doctorView :: K.Locales -> K.Context -> [(T.Text, [(FilePath, T.Text)])] ->
              [(T.Text, [(K.Page, [T.Text])])] -> Html ()
doctorView loc ctx errors broken = div_ [class_ "doctor-page"] $ do
  h1_ [class_ "page-title"] $ toHtml $ K.loc_Doctor loc
  -- the files that are not loaded (path in the pages folder, and reason),
  -- in a section by source: nothing at all without error
  unless (null errors) $ do
    h2_ $ toHtml $ K.loc_LoadErrors loc
    forM_ errors $ \(source, files) -> section_ $ do
      h3_ $ toHtml source
      ul_ [class_ "load-errors"] $ forM_ files $ \(path, reason) -> li_ $ do
        span_ [class_ "file"] $ toHtml path
        ul_ $ li_ $ toHtml reason
  h2_ $ toHtml $ K.loc_BrokenLinks loc
  if null broken
    then p_ $ toHtml $ K.loc_NoBrokenLink loc
    else forM_ broken $ \(source, pages) -> section_ $ do
      h3_ $ toHtml source
      ul_ [class_ "broken-links"] $ forM_ pages $ \(pg, targets) -> li_ $ do
        link $ pageLink ctx pg
        ul_ $ forM_ targets $ li_ . toHtml

metadataView :: K.Locales -> [(T.Text, [(K.MetaField, Maybe T.Text, Forest MetaEntry)])] ->
                Html ()
metadataView loc sources = div_ [class_ "metadata-page"] $ do
  h1_ [class_ "page-title"] $ toHtml $ K.loc_Metadata loc
  forM_ sources $ \(source, fields) -> section_ $ do
    h2_ $ toHtml source
    -- the name of a custom field is a link to the pages having it
    forM_ fields $ \(field, fieldLink, entries) ->
      fieldset_ [class_ "flex", class_ (fieldClass field)] $ do
        legend_ $ case fieldLink of
          Nothing -> toHtml $ fieldName loc field
          Just l  -> a_ [href_ l, class_ "key", title_ (K.loc_PagesWithField loc)] $
                       toHtml $ fieldName loc field
        ul_ [class_ "meta-list"] $ mapM_ entryTree entries
  where
    entryTree :: Tree MetaEntry -> Html ()
    entryTree (Node e children) = li_ $ do
      div_ [class_ "meta-entry"] $ do
        a_ [href_ (metaLink e), class_ "key"] $ toHtml $ metaLabel e
        span_ [class_ "count"] $ toHtml $ "(" <> T.pack (show $ metaCount e) <> ")"
        copyTextButton (K.loc_Copy loc) (metaCopy e)
      unless (null children) $ ul_ $ mapM_ entryTree children
    fieldClass K.FieldTag    = "tags-meta"
    fieldClass K.FieldLang   = "internal-meta"
    fieldClass K.FieldAccess = "internal-meta"
    fieldClass _             = "other-meta"


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
pageLink ctx page = (K.pageTitle page, Url.inContext ctx $ Url.pageUrl page)

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

-- | Image of an icon link or button, described by the title of its parent
icon :: T.Text -> Html ()
icon name = img_ [src_ ("/static/images/" <> name <> ".svg"), alt_ ""]
