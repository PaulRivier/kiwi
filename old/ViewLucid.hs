{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules #-}

module Kiwi.ViewLucid where

-- import           Control.Monad (when)
-- import           Data.List (sort, sortOn)
-- import qualified Data.Text as T
-- import qualified Data.Text.Lazy as TL
-- import           Lucid
-- import           Lucid.Base (makeAttribute)
-- import qualified Text.Pandoc as P

-- import qualified Kiwi.Types as K
-- import           Kiwi.Utils (segmentsToTag)

-- renderHtml :: Html () -> TL.Text
-- renderHtml = renderText


-- pageView :: K.Locales -> K.Page -> Html ()
-- pageView loc p = let content = case (P.runPure $
--                                      P.writeHtml5String P.def (K.pageDoc p)) of
--                         Right r -> r
--                         Left e -> T.pack $ show e in
--   layout loc (K.pageTitle p) (Just p) $ do
--     div_ [ class_ "page-view" ] $ do
--       ul_ [ class_ "flex-hz tags-list" ] $ mconcat $ map showTag (sort $ K.pageTags p)
--       h1_ [class_ "page-title"] $ toHtml $ K.pageTitle p
--       div_ [ class_ "document" ] $ toHtmlRaw content
--   where
--     showTag :: T.Text -> Html ()
--     showTag t = li_ $ 
--       a_ [href_ (T.concat ["/tagged/", t])] $ toHtml t


-- notFound :: K.Locales -> Html ()
-- notFound loc = layout loc (K.loc_NotFound loc) Nothing $
--                toHtml (K.loc_NotFound loc)

-- tagged :: K.Locales -> [K.TagId] -> [K.TagSegments] -> [K.Page] -> Html ()
-- tagged loc tagsSel nextTags pages =
--   layout loc "Tags" Nothing $ do
--     div_ [ class_ "tags-page" ] $ do
--       when (not $ null nextTags) $
--         div_ [ class_ "tags-selection" ] $ do
--           h1_ $ toHtml (K.loc_TagsList loc)
--           ul_ [class_ "flex-hz tags-list"] $ mconcat $ map tagLinkHtml $ sortOn fst $
--                                              map prepareTagLink nextTags
--       div_ [ class_ "results" ] $ do
--         h1_ $ toHtml (K.loc_PagesList loc)
--         ul_ [class_ "flex-hz tagged-pages"] $ mconcat $ map makePageLink pages
--   where
--     tagLinkHtml :: (T.Text, T.Text) -> Html ()
--     tagLinkHtml (label, tagsChain) = li_ $
--       a_ [href_ (T.concat ["/tagged/", tagsChain])] $ toHtml $ label
--     prepareTagLink :: K.TagSegments -> (T.Text, T.Text)
--     prepareTagLink tagSeg = case tagSeg of
--       (t:[]) -> (t, makeTagsChain tagSeg)
--       _      -> (T.concat ["> ", last tagSeg], makeTagsChain tagSeg)
--     makePageLink :: K.Page -> Html ()
--     makePageLink page = li_ $
--       a_ [href_ (K.pageUrl page)] $ toHtml $ K.pageTitle page
--     makeTagsChain :: K.TagSegments -> T.Text
--     makeTagsChain t = let tid = segmentsToTag t in T.intercalate "," (sort (tid:tagsSel))
      

-- searchResults :: K.Locales -> [K.Page] -> Html ()
-- searchResults loc pages =
--   layout loc (K.loc_Search loc) Nothing $ do
--     case pages of
--       [] -> nothingToShow
--       _  -> div_ [ class_ "search-results-page" ] $ do
--         h1_ $ toHtml (K.loc_SearchResults loc)
--         ul_ [class_ "flex-hz results"] $ mconcat $ map makePageLink pages
--   where
--     makePageLink :: K.Page -> Html ()
--     makePageLink page = li_ $
--       a_ [href_ (K.pageUrl page)] $ toHtml $ K.pageTitle page
--     nothingToShow :: Html ()
--     nothingToShow = do
--       h1_ $ toHtml (K.loc_SearchNothing loc)
--       searchForm loc True
  

-- layout :: K.Locales -> T.Text -> Maybe K.Page -> Html ()  -> Html ()
-- layout loc title pageM content = do
--   doctype_
--   html_ [ class_ "no-js" ] $ do
--     head_ $ do
--       meta_ [ httpEquiv_ "content-type", content_ "text/html; charset=UTF-8" ]
--       title_ $ toHtml title
--       meta_ [ charset_ "utf-8" ]
--       meta_ [ name_ "viewport", content_ "width=device-width, initial-scale=1"]
--       meta_ [ name_ "description", content_ "Kiwi is a powerful personal wiki system" ]
--       link_ [ rel_ "shortcut icon", type_ "image/x-icon", href_ "/static/favicon.ico" ]
--       link_ [ href_ "/static/css/pure-min.css", rel_ "stylesheet", media_ "screen" ]
--       link_ [ href_ "/static/css/app.css", rel_ "stylesheet", media_ "screen" ]
--     body_ [style_ "background-color: #2b2b2b;"] $ do
--       div_ [ class_ "left-column" ] $ div_ [ class_ "js-toc" ] $ void
--       div_ [ class_ "main-column" ] $ do
--         navbar loc pageM
--         div_ [ class_ "page-content" ] $ content
--         input_ [ class_ "copy" ] -- for clipboard copy
--       div_ [ class_ "right-column" ] $ void
--       script_ [src_ "/static/js/jquery-3.5.0.min.js"] $ void
--       script_ [src_ "/static/js/lity.js"] $ void
--       script_ [src_ "/static/js/tocbot.min.js"] $ void
--       script_ [src_ "/static/js/app.js"] $ void
        


-- navbar :: K.Locales -> Maybe K.Page -> Html ()
-- navbar loc pageM = 
--   nav_ [ class_ "top-nav" ] $  do
--     div_ [ class_ "nav-content" ] $ do 
--       ul_ [ class_ "nav-items nav-left" ] $ do
--         li_ [ class_ "nav-item" ] $ a_ [ class_ "nav-link", href_ "/" ] $
--           img_ [ src_ "/static/images/kiwi-home.png" ] 
--         li_ [ class_ "nav-item" ] $ a_ [ class_ "nav-link", href_ "/tagged/*" ] $ "Tags"
--       ul_ [ class_ "nav-items nav-right" ] $ do
--         case pageM of
--           Nothing  -> void
--           Just p -> do
--             li_ $ a_ [ class_ "edit-page", data_ "page-id" (K.pageId p) ] $
--               img_ [ src_ "/static/images/edit.svg" ] 
--             li_ $ a_ [ class_ "copy-link", data_ "page-link" (makePageLink p) ] $
--               img_ [ src_ "/static/images/copy-link.svg" ]
--         li_ $ a_ [ class_ "reload" ] $ img_ [ src_ "/static/images/reload.svg" ] 
--         li_ $ searchForm loc False
--   where
--     makePageLink p = T.concat [ "[", K.pageTitle p, "](<page:", K.pageId p, ">)" ]


-- searchForm :: K.Locales -> Bool -> Html ()
-- searchForm loc focus =
--   let focusAttrs = if focus then [ makeAttribute "autofocus" "" ]
--                    else [] in
--     form_ [ class_ "search pure-form", action_ "/search", method_ "get" ] $ do
--       input_ $ [ name_ "query", class_ "search", 
--                 type_ "search", placeholder_ (K.loc_Search loc) ] ++ focusAttrs


-- void :: Html ()
-- void = mempty

