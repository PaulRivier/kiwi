module Kiwi.View
  ( renderPage
  , renderBrowseAll
  , Toggle (..)
  , BrowseMenu
  , BrowseFilters
  , PagesListing (..)
  , renderSearchResults
  , renderUidSearch
  , renderDoc
  , renderEditor
  , renderNewPage
  , VocabularyEntry (..)
  , renderNewPageSources
  , DeleteForm (..)
  , renderDeletePage
  , getLocales
  , renderPreview
  , MetaEntry (..)
  , renderMetadata
  , renderMetadataEditor
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
import qualified Data.Aeson as A
import           Data.List (intersperse)
import           Data.Maybe (fromMaybe, isJust, isNothing)
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import           Data.Tree (Forest, Tree (..))
import           Lucid
import           Lucid.Base (makeAttributes)
import qualified Text.Pandoc.Definition as PD
import qualified Text.Pandoc.Walk as PW
import qualified System.FilePath as FP

import qualified Kiwi.Locales as Loc
import qualified Kiwi.Types as K
import           Kiwi.Types (ActM)
import qualified Kiwi.Auth as Auth
import qualified Kiwi.Browse as Browse
import           Kiwi.Tags (prettyTag)
import qualified Kiwi.Url as Url
import           Kiwi.Utils (asksK, getDB, inlinesText)
import qualified Kiwi.Write as Write
import           Kiwi.HtmlWriter (PageLookup, writeHtml5)
import           Kiwi.Pandoc (linksInContext)
import           Kiwi.Files (servedName)


-- | What the layout needs to know about the current request
data ViewCtx = ViewCtx { ctxLocales :: K.Locales
                       , ctxLogin   :: Maybe T.Text   -- ^ user id
                       , ctxIsAdmin :: Bool
                       , ctxCanEdit :: Bool           -- ^ an editor is set
                       , ctxToc     :: K.TocSetting
                       , ctxContext :: K.Context      -- ^ navigation context
                       , ctxQuery   :: T.Text         -- ^ in the search field
                       , ctxEditor  :: Bool           -- ^ the page has an editor
                       -- | the sources where the user can create a page
                       , ctxCreatable :: [T.Text]
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
                     <*> (Write.creatableSources <$> getDB <*> asksK (K.login . K.requestState))

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
renderPage :: PageLookup -> K.Page -> Bool -> PD.Pandoc -> [K.Page] -> [K.Page] -> [T.Text] -> ActM TL.Text
renderPage findPage p canWrite doc backlinks related keywords = do
  loc <- getLocales
  ctx <- asksK (K.context . K.requestState)
  renderView (K.pageTitle p) (PageActions p canWrite) (Just doc)
             (pageView loc ctx findPage p canWrite doc backlinks related keywords)

-- | Editor of a page: its header and its body, each one with its hash, and
-- the line of the body where the editor starts (from 1); with a field for
-- the message of the commit, and the vocabulary of the header
renderEditor :: K.Page -> Bool -> (T.Text, T.Text) -> (T.Text, T.Text) -> Int ->
                [VocabularyEntry] -> ActM TL.Text
renderEditor p canDelete (meta, metaHash) (body, bodyHash) line =
  editorLayout (EditPage p metaHash bodyHash canDelete) meta body line

-- | Form of a new page in this source, with this header and an empty body
-- (see 'renderEditor')
renderNewPage :: T.Text -> T.Text -> [VocabularyEntry] -> ActM TL.Text
renderNewPage source meta = editorLayout (NewPage source) meta "" 1

editorLayout :: EditTarget -> T.Text -> T.Text -> Int -> [VocabularyEntry] -> ActM TL.Text
editorLayout target meta body line vocabulary = do
  vctx <- getViewCtx
  let loc = ctxLocales vctx
      title = case target of
        EditPage p _ _ _ -> K.loc_Editing loc <> " : " <> K.pageTitle p
        NewPage source -> K.loc_NewPageIn loc <> " " <> source
  return $ renderText $
    layout vctx { ctxEditor = True } title NoActions Nothing $
      editorView loc (ctxIsAdmin vctx) target meta body line vocabulary

-- | The sources where the user can create a page, each one a link to the
-- form of a new page there
renderNewPageSources :: [T.Text] -> ActM TL.Text
renderNewPageSources sources = do
  loc <- getLocales
  renderView (K.loc_NewPage loc) ReloadAction Nothing $ div_ [class_ "new-page-sources"] $ do
    h1_ $ toHtml $ K.loc_NewPageSource loc
    ul_ $ forM_ sources $ \s -> li_ $ a_ [href_ (Url.newPageUrl s)] $ toHtml s

-- | HTML of a document previewed in the editor, with the links to the pages
-- that do not exist (according to the predicate) marked as broken
renderPreview :: PageLookup -> PD.Pandoc -> TL.Text
renderPreview findPage doc = renderText $ writeHtml5 findPage doc

-- | A document embedded in kiwi (README...), with its title as heading if
-- given
renderDoc :: T.Text -> Maybe T.Text -> PD.Pandoc -> ActM TL.Text
renderDoc title headingM doc =
  renderView title NoActions (Just doc) $ div_ [class_ "page-view"] $ do
    forM_ headingM $ h1_ [class_ "page-title"] . toHtml
    -- links of embedded documents are not relative to any source
    div_ [class_ "document"] $ writeHtml5 (const $ Just Nothing) doc

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

-- | Results of the search of a uid (see 'Kiwi.Controller.serveUidSearch'):
-- the pages with this uid, the files with this uid (source, name), and the
-- pages linking it
renderUidSearch :: T.Text -> T.Text -> [K.Page] -> [(T.Text, FilePath)] -> [K.Page] ->
                   ActM TL.Text
renderUidSearch query uid pages files linking = do
  vctx <- getViewCtx
  let loc = ctxLocales vctx
  return $ renderText $
    layout vctx { ctxQuery = query } (K.loc_Search loc) ReloadAction Nothing $
      uidSearchView loc (ctxContext vctx) uid pages files linking

-- | The doctor page, source by source: the files not loaded (path and
-- reason), the pages with broken links (with these links), and the files
-- that no page links (path and uid)
renderDoctor :: [(T.Text, [(FilePath, T.Text)])] -> [(T.Text, [(K.Page, [T.Text])])] ->
                [(T.Text, [(FilePath, T.Text)])] -> ActM TL.Text
renderDoctor errors broken unlinked = do
  loc <- getLocales
  ctx <- asksK (K.context . K.requestState)
  renderView (K.loc_Doctor loc) ReloadAction Nothing $ doctorView loc ctx errors broken unlinked

-- | A key of a field on the metadata page
data MetaEntry = MetaEntry { metaLabel :: T.Text  -- ^ as displayed
                           , metaCopy  :: T.Text  -- ^ the copied text
                           , metaLink  :: T.Text  -- ^ to browse its pages
                           , metaCount :: Int     -- ^ number of pages
                           }

-- | Metadata of the pages, by source: the fields, with a link to the pages
-- having them and a description, if any, and their keys with their sub-keys (sub-tags); for
-- the admins, a link to the metadata editor
renderMetadata :: Bool -> [(T.Text, [(K.MetaField, Maybe T.Text, Maybe T.Text, Forest MetaEntry)])] ->
                  ActM TL.Text
renderMetadata admin sources = do
  loc <- getLocales
  renderView (K.loc_Metadata loc) ReloadAction Nothing $ metadataView loc admin sources

-- | The metadata editor (admins): the script, the result of its last run or
-- check, if any (an error, or what it did), then the syntax of the scripts
renderMetadataEditor :: T.Text -> Maybe (Either T.Text [T.Text]) -> Either String PD.Pandoc ->
                        ActM TL.Text
renderMetadataEditor script result guide = do
  loc <- getLocales
  renderView (K.loc_MetadataEditor loc) NoActions Nothing $ div_ [class_ "metadata-editor"] $ do
    h1_ [class_ "page-title"] $ toHtml $ K.loc_MetadataEditor loc
    forM_ result $ \r -> case r of
      Left err -> pre_ [class_ "script-result script-error"] $ toHtml err
      Right ls -> pre_ [class_ "script-result"] $ toHtml $ T.unlines ls
    form_ [action_ "/metadata-editor", method_ "post", class_ "metadata-script"] $ do
      p_ [class_ "help"] $ toHtml $ K.loc_MetaScriptHelp loc
      -- the browser drops a newline at the start of a textarea
      textarea_ [name_ "script", rows_ "14", spellcheck_ "false"] $ toHtml ("\n" <> script)
      div_ [class_ "buttons"] $ do
        button_ [type_ "submit", name_ "action", value_ "check"] $ toHtml $ K.loc_Check loc
        button_ [type_ "submit", name_ "action", value_ "apply"] $ toHtml $ K.loc_Apply loc
    -- links of embedded documents are not relative to any source
    case guide of
      Right doc -> div_ [class_ "document"] $ writeHtml5 (const $ Just Nothing) doc
      Left err -> pre_ [class_ "script-error"] $ toHtml err

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
    case actions of
      PageActions p _ -> newPage (Just $ fst $ K.pageUID p)
      ReloadAction -> newPage Nothing
      NoActions -> mempty
    searchForm loc (ctxContext ctx) (ctxQuery ctx) False
  where
    loc = ctxLocales ctx
    admin = ctxIsAdmin ctx
    -- a new page in the source of the page, else in the only source of the
    -- context (or the only one where the user can create), else the choice
    -- of the source
    creatable = ctxCreatable ctx
    newPage sourceM = unless (null creatable) $
      a_ [ href_ (newPageTarget sourceM), class_ "icon new-page"
         , title_ (K.loc_NewPage loc) ] $ icon "new"
    newPageTarget sourceM = case sourceM of
      Just s | s `elem` creatable -> Url.newPageUrl s
      _ -> let inCtx = K.contextSources (ctxContext ctx)
               candidates = if S.null inCtx then creatable else filter (`S.member` inCtx) creatable
           in case candidates of
                [s] -> Url.newPageUrl s
                _ -> "/new"
    reload = iconButton "reload" (K.loc_Reload loc) alerts "reload"
    -- texts of the alerts shown by app.js when the action fails
    alerts = [ data_ "error-session" (K.loc_ActionSession loc)
             , data_ "error-forbidden" (K.loc_ActionForbidden loc)
             , data_ "error-failed" (K.loc_ActionFailed loc) ]

-- | CommonMark link to the page, by its uid, to paste in another page of its
-- source (see 'cmLinkTo': a valid uid needs no angle brackets)
cmLink :: K.Page -> T.Text
cmLink p = cmLinkTo (K.pageTitle p) ("page:" <> snd (K.pageUID p))

-- | CommonMark link to a file of the source, by its uid, with its name
-- without uid as text ("[cake.jpg](file:K3M9QW2TZ1)")
cmFileLink :: FilePath -> T.Text -> T.Text
cmFileLink path uid = cmLinkTo (T.pack $ servedName $ FP.takeFileName path) ("file:" <> uid)

-- | CommonMark link with this text and target (that needs no angle
-- brackets). The brackets of the text are escaped (and the backslashes,
-- which would escape them).
cmLinkTo :: T.Text -> T.Text -> T.Text
cmLinkTo text target = T.concat ["[", escape text, "](", target, ")"]
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
pageView :: K.Locales -> K.Context -> PageLookup -> K.Page -> Bool -> PD.Pandoc ->
            [K.Page] -> [K.Page] -> [T.Text] -> Html ()
pageView loc ctx findPage p canWrite doc backlinks related keywords = div_ [class_ "page-view"] $ do
  contextBar loc [fst $ K.pageUID p]
    (linksList "tags-list" [ (prettyTag t, browseLink K.FieldTag (K.KeyText t))
                           | t <- K.pageTags p ])
    (K.contextArchive ctx)
  h1_ [class_ "page-title", pageLang] $ toHtml $ K.pageTitle p
  -- metadata: the summary (plain text, in the lang of the page), then the
  -- custom metadata as a two-column table: the names of the fields (links to
  -- the pages having them), then their values
  unless (isNothing (K.pageSummary p) && null (K.pageCustomMeta p)) $
    fieldset_ [class_ "flex page-meta"] $ do
      forM_ (K.pageSummary p) $ \s -> p_ [class_ "page-summary", pageLang] $ toHtml s
      unless (null $ K.pageCustomMeta p) $
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
    writeHtml5 findPage $ linksInContext ctx doc
  -- the backlinks, each one marked by an icon, then the related pages (see
  -- "Kiwi.Related"), in one list
  unless (null backlinks && null related) $
    fieldset_ [class_ "flex related"] $ do
      legend_ $ toHtml $ K.loc_RelatedPages loc
      ul_ [class_ "flex-hz pages-list related"] $ do
        forM_ backlinks $ \pg -> li_ [class_ "backlink"] $ pageItem pg $
          img_ [ src_ "/static/images/backlink.svg", class_ "backlink"
               , alt_ (K.loc_Backlink loc), title_ (K.loc_Backlink loc) ]
        forM_ related $ \pg -> li_ $ pageItem pg mempty
  -- the keywords (see "Kiwi.Keywords"), the best first, each one a search of
  -- the pages having it, in the lang of the page
  unless (null keywords) $
    fieldset_ [class_ "flex keywords"] $ do
      legend_ $ toHtml $ K.loc_Keywords loc
      ul_ [class_ "flex-hz keywords", pageLang] $ forM_ keywords $ \k ->
        li_ $ a_ [href_ $ Url.searchUrl ctx (K.langCode (K.pageLang p) <> ":" <> k)] $ toHtml k
  where
    -- the title and the document are in the lang of the page, the rest in
    -- the lang of the interface
    pageLang = lang_ $ K.langCode $ K.pageLang p
    pageItem pg mark = a_ ([href_ $ Url.inContext ctx $ Url.pageUrl pg] ++ summaryTitle pg) $
      mark >> toHtml (K.pageTitle pg)
    contextReq = Browse.sourcesReq $ K.contextSources ctx
    browseLink f k = Browse.browseUrl ctx $ Browse.addToReq contextReq (f, k)


-- | What the editor writes: a page (with the hashes of its header and its
-- body, and whether the user can delete it), or a new page of a source
data EditTarget = EditPage K.Page T.Text T.Text Bool
                | NewPage T.Text

-- | The editor of a page, or the form of a new page (see 'renderEditor',
-- 'renderNewPage'): a form with the header in a text area, and the body in
-- another one, turned into a markdown editor by app.js, with the texts of
-- its buttons and messages, the field of the message of the commit, and,
-- for a new page and an admin, the field of its path. The id and the hashes
-- are empty for a new page. The link to the deletion of the page needs the
-- admin right on it.
editorView :: K.Locales -> Bool -> EditTarget -> T.Text -> T.Text -> Int -> [VocabularyEntry] ->
              Html ()
editorView loc admin target meta body line vocabulary =
  form_ ([ class_ formClass, method_ "post", action_ action
         , data_ "page-url" back, data_ "line" (T.pack $ show line)
         , data_ "error-session" (K.loc_ActionSession loc)
         , data_ "error-forbidden" (K.loc_ActionForbidden loc)
         , data_ "error-failed" (K.loc_ActionFailed loc)
         , data_ "error-conflict" (K.loc_SaveConflict loc)
         , data_ "error-invalid" (K.loc_SaveInvalid loc)
         , data_ "warning-ignored" (K.loc_SaveIgnored loc)
         , data_ "new-value" (K.loc_NewMetaValue loc) ] ++
         [ data_ ("button-" <> name) label | (name, label) <- K.loc_EditorButtons loc ]) $ do
    h1_ [class_ "page-title"] $ case target of
      EditPage p _ _ _ -> do
        span_ [class_ "editing"] $ toHtml (K.loc_Editing loc) <> " : "
        toHtml (K.pageTitle p)
      NewPage _ -> do
        span_ [class_ "editing"] $ toHtml (K.loc_NewPageIn loc) <> " "
        toHtml source
    input_ [type_ "hidden", name_ "source", value_ source]
    input_ [type_ "hidden", name_ "id", value_ pId]
    input_ [type_ "hidden", name_ "meta-hash", value_ metaHash]
    input_ [type_ "hidden", name_ "body-hash", value_ bodyHash]
    when (admin && isNew) $
      input_ [ type_ "text", name_ "path", class_ "page-path", maxlength_ "500"
             , placeholder_ (K.loc_PagePath loc), title_ (K.loc_PagePath loc) ]
    -- the browser drops a newline at the start of a textarea
    label_ [class_ "editor-label", for_ "editor-meta"] $ toHtml (K.loc_EditorMeta loc)
    textarea_ [ id_ "editor-meta", name_ "meta", class_ "editor-meta", spellcheck_ "false"
              , rows_ (T.pack $ show $ max 3 (length (T.lines meta) + 1)) ] $ toHtml ("\n" <> meta)
    script_ [type_ "application/json", id_ "meta-vocabulary"] $ vocabularyJson vocabulary
    label_ [class_ "editor-label", for_ "editor-body"] $ toHtml (K.loc_EditorBody loc)
    textarea_ [id_ "editor-body", name_ "body", class_ "editor-text", rows_ "30"] $ toHtml ("\n" <> body)
    div_ [class_ "editor-actions"] $ do
      input_ [ type_ "text", name_ "message", class_ "commit-message", maxlength_ "2000"
             , placeholder_ (K.loc_CommitMessage loc), title_ (K.loc_CommitMessage loc) ]
      button_ [type_ "submit", class_ "save"] $ toHtml $ K.loc_Save loc
      a_ [href_ back, class_ "cancel"] $ toHtml $ K.loc_Cancel loc
      case target of
        EditPage p _ _ True ->
          a_ [href_ (Url.deleteUrl $ K.pageUID p), class_ "delete-page"] $ toHtml $ K.loc_DeletePage loc
        _ -> mempty
  where
    (isNew, (source, pId), (metaHash, bodyHash), back, action) = case target of
      EditPage p mh bh _ -> (False, K.pageUID p, (mh, bh), Url.pageUrl p, "/save-page")
      NewPage s -> (True, (s, ""), ("", ""), "/", "/create-page")
    formClass = if isNew then "page-editor new-page" else "page-editor"

-- | What the editor of a header proposes for a key (see 'editorView'): its
-- values, the rights that can follow a value after a ":" (access), and its
-- description
data VocabularyEntry = VocabularyEntry
  { vocKey :: T.Text
  , vocValues :: [T.Text]
  , vocRights :: [T.Text]
  , vocDescription :: Maybe T.Text
  }

-- | The vocabulary as JSON, for app.js, in a script element (a block of
-- data, never run): no "<", so that no value can end the element
vocabularyJson :: [VocabularyEntry] -> T.Text
vocabularyJson entries = T.replace "<" "\\u003c" $ TL.toStrict $ TLE.decodeUtf8 $ A.encode $
  [ A.object $ [ "key" A..= vocKey e, "values" A..= vocValues e ]
               ++ [ "rights" A..= vocRights e | not (null $ vocRights e) ]
               ++ [ "description" A..= d | Just d <- [vocDescription e] ]
  | e <- entries ]

-- | What the confirmation of the deletion of a page shows
data DeleteForm = DeleteForm
  { dfPage :: K.Page
  , dfHash :: T.Text                 -- ^ of the file of the page, as read
  , dfPath :: Maybe FP.FilePath      -- ^ of the file, from the top of the source (admins)
  , dfBacklinks :: [K.Page]          -- ^ the pages linking to it, that the user can read
  , dfFiles :: [(T.Text, T.Text)]    -- ^ the files that only this page links: uid, name
  , dfError :: Maybe T.Text          -- ^ failure of the deletion asked
  , dfMessage :: T.Text              -- ^ of the commit
  , dfDeleteFiles :: Bool            -- ^ delete the files too
  }

-- | Confirmation of the deletion of a page (see 'DeleteForm'): a form that
-- needs no script, sent to /delete-page
renderDeletePage :: DeleteForm -> ActM TL.Text
renderDeletePage df = do
  loc <- getLocales
  let p = dfPage df
      (source, pId) = K.pageUID p
  renderView (K.loc_Deleting loc <> " : " <> K.pageTitle p) NoActions Nothing $
    form_ [class_ "delete-page", method_ "post", action_ "/delete-page"] $ do
      h1_ [class_ "page-title"] $ do
        span_ [class_ "editing"] $ toHtml (K.loc_Deleting loc) <> " : "
        a_ [href_ (Url.pageUrl p)] $ toHtml (K.pageTitle p)
      forM_ (dfError df) $ \e -> p_ [class_ "delete-error"] $ toHtml e
      forM_ (dfPath df) $ \path -> p_ [class_ "delete-path"] $ code_ $ toHtml $ source <> "/" <> T.pack path
      p_ [class_ "help"] $ toHtml $ K.loc_DeleteHelp loc
      unless (null $ dfBacklinks df) $ do
        p_ $ toHtml $ K.loc_DeleteBacklinks loc
        ul_ $ forM_ (dfBacklinks df) $ \b -> li_ $ a_ [href_ (Url.pageUrl b)] $ toHtml (K.pageTitle b)
      unless (null $ dfFiles df) $ do
        p_ $ toHtml $ K.loc_DeleteFiles loc
        ul_ $ forM_ (dfFiles df) $ \(_, name) -> li_ $ code_ $ toHtml name
        forM_ (dfFiles df) $ \(u, _) -> input_ [type_ "hidden", name_ "file", value_ u]
        p_ $ label_ $ do
          input_ $ [type_ "checkbox", name_ "delete-files", value_ "yes"] ++
                   [checked_ | dfDeleteFiles df]
          " " <> toHtml (K.loc_DeleteFilesToo loc)
      input_ [type_ "hidden", name_ "source", value_ source]
      input_ [type_ "hidden", name_ "id", value_ pId]
      input_ [type_ "hidden", name_ "hash", value_ (dfHash df)]
      div_ [class_ "editor-actions"] $ do
        input_ [ type_ "text", name_ "message", class_ "commit-message", maxlength_ "2000"
               , value_ (dfMessage df)
               , placeholder_ (K.loc_CommitMessage loc), title_ (K.loc_CommitMessage loc) ]
        button_ [type_ "submit", class_ "delete"] $ toHtml $ K.loc_Delete loc
        a_ [href_ (Url.editUrl $ K.pageUID p), class_ "cancel"] $ toHtml $ K.loc_Cancel loc

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
      fieldset ("→ " <> pagesLegend) $ pagesList "pages-list" ctx pages
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
      -- one result by line, a single link: the title, then the summary of
      -- the page and the excerpt, in the lang of the page, with the words of
      -- the query in bold in the excerpt
      ul_ [class_ "results"] $ forM_ pages $ \(pg, fragments) ->
        li_ $ a_ [href_ $ Url.inContext ctx $ Url.pageUrl pg] $ do
          span_ [class_ "title"] $ toHtml $ K.pageTitle pg
          forM_ (K.pageSummary pg) $ \s ->
            p_ [class_ "summary", lang_ (K.langCode $ K.pageLang pg)] $ toHtml s
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


-- | The pages with the uid, the files with the uid, then the pages linking
-- it, each one with its source, in the style of the search results
uidSearchView :: K.Locales -> K.Context -> T.Text -> [K.Page] -> [(T.Text, FilePath)] ->
                 [K.Page] -> Html ()
uidSearchView loc ctx uid pages files linking = div_ [class_ "search-results-page"] $ do
  contextBar loc (S.toList $ K.contextSources ctx) mempty (K.contextArchive ctx)
  h1_ $ toHtml $ "uid:" <> uid
  unless (null pages) $ do
    h2_ $ toHtml $ K.loc_UidPage loc
    results [ (K.pageTitle p, Url.inContext ctx $ Url.pageUrl p, fst $ K.pageUID p, summaryTitle p)
            | p <- pages ]
  unless (null files) $ do
    h2_ $ toHtml $ K.loc_UidFile loc
    -- in a new tab, as the links of the pages to the files that are not images
    results [ (T.pack name, T.concat ["/file/", Url.urlEncodeText s, "/", Url.urlEncodeText uid]
              , s, [target_ "_blank"])
            | (s, name) <- files ]
  h2_ $ toHtml $ K.loc_UidLinking loc
  if null linking
    then p_ $ toHtml $ K.loc_UidNothing loc
    else results [ (K.pageTitle p, Url.inContext ctx $ Url.pageUrl p, fst $ K.pageUID p, summaryTitle p)
                 | p <- linking ]
  where
    results :: [(T.Text, T.Text, T.Text, [Attributes])] -> Html ()
    results items = ul_ [class_ "results"] $ forM_ items $ \(label, url, source, attrs) ->
      li_ $ a_ ([href_ url] <> attrs) $ do
        span_ [class_ "title"] $ toHtml label
        p_ [class_ "excerpt"] $ toHtml source


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
              [(T.Text, [(K.Page, [T.Text])])] -> [(T.Text, [(FilePath, T.Text)])] -> Html ()
doctorView loc ctx errors broken unlinked = div_ [class_ "doctor-page"] $ do
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
  -- the files that no page links (path from the top of the source), each
  -- one with a button copying a link to it: nothing at all without such a file
  unless (null unlinked) $ do
    h2_ $ toHtml $ K.loc_UnlinkedFiles loc
    forM_ unlinked $ \(source, files) -> section_ $ do
      h3_ $ toHtml source
      ul_ [class_ "unlinked-files"] $ forM_ files $ \(path, uid) -> li_ $ do
        span_ [class_ "file"] $ toHtml path
        copyButton (K.loc_CopyFileLink loc) (cmFileLink path uid)

metadataView :: K.Locales -> Bool -> [(T.Text, [(K.MetaField, Maybe T.Text, Maybe T.Text, Forest MetaEntry)])] ->
                Html ()
metadataView loc admin sources = div_ [class_ "metadata-page"] $ do
  h1_ [class_ "page-title"] $ toHtml $ K.loc_Metadata loc
  when admin $ p_ [class_ "metadata-editor-link"] $
    a_ [href_ "/metadata-editor"] $ toHtml $ K.loc_OpenMetadataEditor loc
  forM_ sources $ \(source, fields) -> section_ $ do
    h2_ $ toHtml source
    -- the name of a custom field is a link to the pages having it, its
    -- description as title
    forM_ fields $ \(field, fieldLink, descriptionM, entries) ->
      fieldset_ [class_ "flex", class_ (fieldClass field)] $ do
        legend_ (maybe [] (\d -> [title_ d]) descriptionM) $ case fieldLink of
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

-- | List of links to pages, in the navigation context, with their summary as
-- title
pagesList :: T.Text -> K.Context -> [K.Page] -> Html ()
pagesList cls ctx pages = unless (null pages) $
  ul_ [class_ "flex-hz", class_ cls] $ forM_ pages $ \pg ->
    li_ $ a_ ([href_ $ Url.inContext ctx $ Url.pageUrl pg] ++ summaryTitle pg) $
      toHtml $ K.pageTitle pg

-- | The summary of a page as title of its links, if any
summaryTitle :: K.Page -> [Attributes]
summaryTitle = maybe [] (\s -> [title_ s]) . K.pageSummary

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
