-- renderTagged :: [K.TagId] -> [K.TagSegments] -> [K.Page] -> ActM TL.Text
-- renderTagged tagsSel nextTags pages = do
--   loc <- getLocales
--   locJ <- getLocalesJSON
--   tpl <- getTemplate
--   let c = object [ ("tags", mkTags), ("pages", mkPages), ("locales", locJ) ]
--       mkTags = toJSONListHack $ map tagJSON $ sortOn fst $ map prepareTagLink nextTags
--       mkPages = toJSONListHack $ map (linkJSON . pageLink) pages
--       lc = X.renderMustache (K.taggedTemplate tpl) c
--   renderLayout (K.loc_Tags loc) lc Nothing
--   where
--     tagJSON :: (T.Text, T.Text) -> J.Value
--     tagJSON (label, tagsChain) = linkJSON (label, T.concat ["/browse/tags/", tagsChain])
--     prepareTagLink :: K.TagSegments -> (T.Text, T.Text)
--     prepareTagLink tagSeg = case tagSeg of
--       (t:[]) -> (t, makeTagsChain tagSeg)
--       _      -> (T.concat ["> ", last tagSeg], makeTagsChain tagSeg)
--     makeTagsChain :: K.TagSegments -> T.Text
--     makeTagsChain t = let
--       tagParents = map segmentsToTag $ init $ tail $ inits t
--       newTagsSel = tagsSel \\ tagParents
--       tid = segmentsToTag t
--       in T.intercalate "," (sort (tid:newTagsSel))




  
-- serveTagged :: T.Text -> ActM ()
-- serveTagged tags' = do
--   db <- getRestrictedDB
--   r <- if tags' == "*" then taggedIndex db
--        else taggedWith db (U.splitTextEsc ',' tags')
--   html r
--   where
--     taggedIndex db =
--       let rootTags = filterRootTags $ tagsListSubSegments $ nodup $
--                      concat (map K.pageTags $ DI.documentsList (K.pagesIndex db))
--       in V.renderTagged [] rootTags $ DI.documentsList (K.pagesIndex db)
--     taggedWith db query =
--       let idx = K.pagesIndex db
--           subset = DI.narrow idx $ U.qAll K.FieldTag query
--           matchingPages = DI.documentsList subset
--           matchesAllTagsSegs = tagsListSubSegments $ K.fromKeys $
--                                DI.getFieldKeys subset K.FieldTag
--           querySegs = tagsListSubSegments query
--           nextTags = filterRootTags $ fastListDiff matchesAllTagsSegs querySegs
--       in V.renderTagged query nextTags matchingPages



