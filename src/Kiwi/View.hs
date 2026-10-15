module Kiwi.View
  ( renderPage
  , renderBrowseAll
  , renderSearchResults
  , renderLoginForm
  , renderNotFound
  , renderBadRequest
  , renderForbidden
  ) where

import           Data.Aeson (ToJSON(..), (.=), toJSON, object)
import qualified Data.Aeson as J
import           Data.List (sortOn)
import           Data.Maybe (fromMaybe)
import qualified Data.Text as T
-- import qualified Data.Time as Time
import qualified Data.Text.Lazy as TL
import qualified Text.Mustache as X
-- import qualified Text.Pandoc as P
-- import           Web.Scotty.Trans (liftAndCatchIO)

import qualified Kiwi.Locales as Loc
import qualified Kiwi.Types as K
import           Kiwi.Types (ActM)
import           Kiwi.Utils (prettyTag, asksK, fst3)
import qualified Kiwi.Utils as U
import           Kiwi.HtmlWriter (writeHtml5)


linkJSON :: (T.Text, T.Text) -> J.Value
linkJSON (label, target) = object [ ("label", toJSON label)
                                  , ("target", toJSON target) ]


pageLink :: K.Page -> (T.Text, T.Text)
pageLink page = (K.pageTitle page, U.pageUrl page)

renderLayout :: T.Text -> TL.Text -> Maybe J.Value -> ActM TL.Text
renderLayout t c pgi = do
  loc <- getLocalesJSON
  tpl <- getTemplate
  userM <- fmap fst3 <$> asksK K.login
  isAdmin <- U.isAdmin
  toc <- asksK K.tocSetting
  let ld = object [ "title" .= t
                  , "toc-setting" .= K.tocSettingValue toc
                  , "content" .= c
                  , "locales" .= loc
                  , "page-info" .= pgi
                  , "login" .= userM
                  , "is-admin" .= isAdmin ]
  -- liftAndCatchIO $ putStrLn $ show ld
  return $ X.renderMustache (K.layoutTemplate tpl) ld


renderPage :: K.Page -> [K.Page] -> ActM TL.Text
renderPage p blp =
  let doc = writeHtml5 (K.pageDoc p) in
  do
    tpl <- getTemplate
    loc <- getLocalesJSON
    let pvd = object [ "title" .= K.pageTitle p
                     , "page-source" .= fst (K.pageUID p)
                     , "page-id" .= snd (K.pageUID p)
                     , "tags" .= tagsViewData
                     , "custom-meta" .= customMeta
                     , "lang" .= (show $ K.pageLang p)
                     , "backlinks" .= map (linkJSON . pageLink) blp
                     , "url" .= U.pageUrl p
                     , "document" .= doc
                     , "locales" .= loc ]
        tagsViewData = map (\t -> linkJSON (prettyTag t, browseTag t)) $
                       K.pageTags p
        layoutContent = X.renderMustache (K.pageTemplate tpl) (toJSON pvd)
        pgi = Just $ object [ "page-source" .= fst (K.pageUID p)
                            , "page-id" .= snd (K.pageUID p)
                            , "cm-link" .= T.concat [ "[", K.pageTitle p, "](<page:",
                                                      U.pageUIDLink p, ">)" ]
                            ]
    renderLayout (K.pageTitle p) layoutContent pgi
  where 
    browseTag t = browseLink K.FieldTag (K.KeyText t)
    browseLink f k = U.browseUrl $ U.createReq (f,k)
    customMeta = map customMeta1 $ K.pageCustomMeta p
      where customMeta1 cmd =
              object [ "name" .= K.cmdName cmd
                     , "links" .= map (mkLink $ K.cmdName cmd) (K.cmdKeys cmd) ]
            mkLink f k = linkJSON ( U.showKeyLabel k,
                                    browseLink (K.FieldCustom f) k )



-- | Links (label, target) to narrow the request, by field
type BrowseMenu = [(K.MetaField, [(T.Text, T.Text)])]

-- | Keys of the request: field, label, and link to remove the key
type BrowseFilters = [(K.MetaField, T.Text, T.Text)]

renderBrowseAll :: BrowseMenu -> BrowseFilters -> [K.Page] -> ActM TL.Text
renderBrowseAll menu filters pages = do
  loc <- getLocales
  locJ <- getLocalesJSON
  tpl <- getTemplate
  let c = object [ ("filters", mkFilters)
                 , ("sources", mkSources)
                 , ("tags", mkTags)
                 , ("custom-meta", mkCustomMeta)
                 , ("pages", mkPages)
                 , ("locales", locJ) ]
      mkFilters = toJSONListHack $ map filterJSON filters
      mkTags = toJSONListHack $ map linkJSON $ sortOn fst $
               fromMaybe [] $ lookup K.FieldTag menu
      mkCustomMeta = toJSONListHack $ map customMetaJSON $
                     [(n, vs) | (K.FieldCustom n, vs)  <- menu ]
      mkSources = let sourcesLink = fromMaybe [] $ lookup K.FieldSource menu
        in if length sourcesLink > 1 then toJSONListHack $ map linkJSON sourcesLink
           else J.Null
      mkPages = toJSONListHack $ map (linkJSON . pageLink) pages
      lc = X.renderMustache (K.browseTemplate tpl) c
  renderLayout (K.loc_Tags loc) lc Nothing
  where
    customMetaJSON :: (T.Text, [(T.Text, T.Text)]) -> J.Value
    customMetaJSON (field, keys) = object [ ("field", toJSON field)
                                          , ("keys", toJSONListHack $ map linkJSON keys) ]

    -- the field name is only given for custom fields
    filterJSON :: (K.MetaField, T.Text, T.Text) -> J.Value
    filterJSON (f, label, target) = object [ "field" .= customName f
                                           , "label" .= label
                                           , "target" .= target ]
    customName (K.FieldCustom n) = Just n
    customName _ = Nothing
  
      
renderSearchResults :: [K.Page] -> ActM TL.Text
renderSearchResults pages = do
  loc <- getLocales
  locJ <- getLocalesJSON
  tpl <- getTemplate
  let c = object [ "results" .= (toJSONListHack $ map (linkJSON . pageLink) pages)
                 , "locales" .= locJ ]
  let lc = X.renderMustache (K.searchTemplate tpl) c
  renderLayout (K.loc_Search loc) lc Nothing


-- | Login form, with an error message if previous attempt failed
renderLoginForm :: Bool -> ActM TL.Text
renderLoginForm failed = do
  loc <- getLocales
  tpl <- getTemplate
  let c = object [ "login-error" .= if failed then Just (K.loc_LoginFailed loc)
                                    else Nothing ]
  let lc = X.renderMustache (K.loginTemplate tpl) c
  renderLayout "Log-in" lc Nothing


renderNotFound :: ActM TL.Text
renderNotFound = do
  loc <- getLocales
  locJ <- getLocalesJSON
  tpl <- getTemplate
  let layoutContent = X.renderMustache (K.notFoundTemplate tpl) locJ
  renderLayout (K.loc_NotFound loc) layoutContent Nothing


-- | Invalid request, with the reason
renderBadRequest :: T.Text -> ActM TL.Text
renderBadRequest err = do
  loc <- getLocales
  locJ <- getLocalesJSON
  tpl <- getTemplate
  let c = object [ "error" .= err, "locales" .= locJ ]
      layoutContent = X.renderMustache (K.badRequestTemplate tpl) c
  renderLayout (K.loc_BadRequest loc) layoutContent Nothing


renderForbidden :: ActM TL.Text
renderForbidden = do
  loc <- getLocales
  locJ <- getLocalesJSON
  tpl <- getTemplate
  let layoutContent = X.renderMustache (K.forbiddenTemplate tpl) locJ
  renderLayout (K.loc_Forbidden loc) layoutContent Nothing


getLocalesJSON :: ActM J.Value
getLocalesJSON = do
  l <- Loc.locales <$> asksK K.uiLang
  return $ object [ "home" .= K.loc_Home l
                  , "tags" .= K.loc_Tags l
                  , "tags-list" .= K.loc_TagsList l
                  , "pages-list" .= K.loc_PagesList l
                  , "search" .= K.loc_Search l
                  , "search-results" .= K.loc_SearchResults l
                  , "search-nothing" .= K.loc_SearchNothing l
                  , "agenda" .= K.loc_Agenda l
                  , "today" .= K.loc_Today l
                  , "to-come" .= K.loc_ToCome l
                  , "not-found" .= K.loc_NotFound l
                  , "bad-request" .= K.loc_BadRequest l
                  , "forbidden" .= K.loc_Forbidden l
                  , "remove-filter" .= K.loc_RemoveFilter l ]



getLocales :: ActM K.Locales
getLocales = do
  l <- asksK K.uiLang
  return $ Loc.locales l

getTemplate :: ActM K.KiwiTemplate
getTemplate = asksK K.template


toJSONListHack :: ToJSON a => [a] -> J.Value
toJSONListHack [] = J.Null
toJSONListHack l = object [("items", toJSON l)]
