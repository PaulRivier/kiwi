{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Kiwi.Pandoc
  ( loadPageIO
  , pageDoc
  , embeddedDoc
  , pagePath
  , linksInContext
  , unknownMetaKeys
  , invalidMetaValues
  , loadPage         -- exported for tests
  , parseMetaData    -- exported for tests
  , parseCustomMeta  -- exported for tests
  , sourceMeta       -- exported for tests
  ) where

import           Control.Applicative ((<|>))
import           Control.Exception (IOException, try)
import qualified Commonmark as Cm
import qualified Commonmark.Extensions as CmEx
import qualified Commonmark.Pandoc as CmP

import qualified Data.ByteString as BS
import           Data.Functor ((<&>))
import qualified Data.Map as M
import           Data.Maybe (fromMaybe, isNothing, mapMaybe)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import qualified Data.Time as Time
import qualified System.Directory as D
import qualified System.FilePath as FP
import           System.IO.Error (ioeGetErrorString)
-- import qualified Text.Pandoc as P
import qualified Text.Pandoc.Builder as PB
import qualified Text.Pandoc.Definition as PD
import qualified Text.Pandoc.Walk as PW
-- import qualified Text.Pandoc.Shared as PS
import           Text.Read (readMaybe)

import           Kiwi.LangDetect (resolveLang)
import           Kiwi.Types
import           Kiwi.Utils (splitOnFirst, splitMeta, splitTags, pathToPageId,
                             parseLangSetting)
import qualified Kiwi.Utils as U



loadPage :: FP.FilePath -> T.Text -> T.Text -> MetaData -> [CustomMetaConfig] ->
              FP.FilePath -> Either String PandocPage
loadPage path c source md cmc dir =
    let (docMeta, content) = splitPage c
        meta = md { metaId    = T.concat <$> M.lookup "id" docMeta
                  , metaTitle = findWith (metaTitle md) (T.intercalate " ")
                                         "title" docMeta
                  , metaTags = findWith (metaTags md) (splitTags . T.intercalate ",")
                                        "tags" docMeta
                  , metaAccess = findWith (metaAccess md) (splitMeta . T.intercalate ",")
                                          "access" docMeta
                  , metaLang = fromMaybe (metaLang md)
                                         ( M.lookup "lang" docMeta >>=
                                           either (const Nothing) Just .
                                           parseLangSetting . last )
                  , metaArchive = fromMaybe False $
                                  M.lookup "archive" docMeta >>= parseBool . last
                  , metaCustom = parseCustomMeta docMeta cmc
                  }
    in loadDoc path source dir docMeta content <&>
       \(doc, colLinks) -> PandocPage doc meta colLinks
  where
    -- mdConf = P.def { P.readerExtensions = P.pandocExtensions }
    findWith def join' field meta = fromMaybe def $
                                    fmap join' $
                                    M.lookup field meta


-- | Keys of the metadata header of a page text that kiwi does not use: neither
-- built-in (read by 'loadPage' and 'loadDoc') nor declared custom metadata
unknownMetaKeys :: [CustomMetaConfig] -> T.Text -> [T.Text]
unknownMetaKeys cmc c = filter (`notElem` known) $ M.keys $ fst $ splitPage c
  where
    known = [ "id", "title", "tags", "access", "lang", "archive", "images-dir"
            , "files-dir" ] ++ map cmcName cmc


-- | Built-in metadata of the header of a page text whose value can not be
-- read, with this value: the page has the default value instead. Only
-- "archive" is checked.
invalidMetaValues :: T.Text -> [(T.Text, T.Text)]
invalidMetaValues c = case M.lookup "archive" (fst $ splitPage c) of
  Just vs | v <- last vs, isNothing (parseBool v) -> [("archive", v)]
  _ -> []


-- | yes, true, 1 or no, false, 0, ignoring case
parseBool :: T.Text -> Maybe Bool
parseBool b = case T.toLower (T.strip b) of
  v | v `elem` ["yes", "true", "1"] -> Just True
    | v `elem` ["no", "false", "0"] -> Just False
    | otherwise -> Nothing


-- | Metadata and content of a page. Windows line endings are normalized,
-- and a file without blank line is considered as metadata only, with empty
-- content
splitPage :: T.Text -> (M.Map T.Text [T.Text], T.Text)
splitPage c =
  let normC = T.replace "\r\n" "\n" c
      (metaC, content) = fromMaybe (normC, T.empty) $ splitOnFirst "\n\n" normC
  in (parseMetaData metaC, content)


-- | Document of a page, with its links fixed, and the links to other pages
loadDoc :: FP.FilePath -> T.Text -> FP.FilePath -> M.Map T.Text [T.Text] -> T.Text ->
           Either String (PD.Pandoc, [PageUID])
loadDoc path source dir docMeta content =
    let dirT = T.pack $ dir
        imagesDir = maybe dirT T.concat $ M.lookup "images-dir" docMeta
        filesDir  = maybe dirT T.concat $ M.lookup "files-dir" docMeta
        syntaxSpec = Cm.defaultSyntaxSpec <>
                     CmEx.strikethroughSpec <> CmEx.pipeTableSpec <> CmEx.autolinkSpec <>
                     CmEx.taskListSpec <> CmEx.footnoteSpec <>
                     CmEx.autoIdentifiersSpec


    in case (Cm.commonmarkWith syntaxSpec path content
             :: Maybe (Either Cm.ParseError (CmP.Cm () PB.Blocks))) of
      Nothing -> Left $ "Unknown error"
      Just x -> case x of
         Left e -> Left $ show e
         Right cmBlocks->
           let doc' = PB.doc (CmP.unCm cmBlocks)
               (doc, collected) = walkDoc source dirT imagesDir filesDir doc'
               colLinks = [ (source, l) | CollectedPageLink l <- collected ]
           in Right (doc, colLinks)


-- | Document of a loaded page, parsed again from its text to be displayed
pageDoc :: FP.FilePath -> Page -> Either String PD.Pandoc
pageDoc contentDir' p =
  let path = pageAbsoluteFSPath p
      source = fst $ pageUID p
      relPath = snd $ pagePath contentDir' path
      (docMeta, content) = splitPage $ pageText p
  in fst <$> loadDoc path source (pageSubDir relPath) docMeta content


-- | Document of a file embedded in kiwi (README, CommonMark cheatsheet), with
-- or without a metadata header, and the title of its header. Its links are
-- not relative to any source.
embeddedDoc :: FP.FilePath -> Bool -> T.Text -> Either String (Maybe T.Text, PD.Pandoc)
embeddedDoc name withHeader c =
  let (docMeta, content) = if withHeader then splitPage c
                           else (M.empty, T.replace "\r\n" "\n" c)
      title = T.intercalate " " <$> M.lookup "title" docMeta
  in (\(doc, _) -> (title, doc)) <$> loadDoc name "" "" docMeta content


-- | Source of a page file, and its path in the pages folder, without
-- extension
pagePath :: FP.FilePath -> FP.FilePath -> (T.Text, FP.FilePath)
pagePath contentDir' fullPath =
  let splitPath = FP.splitDirectories $ FP.makeRelative contentDir' fullPath
  in ( T.pack $ concat $ take 1 splitPath
     , FP.dropExtension $ FP.joinPath $ drop 2 $ splitPath )

-- | Subfolder of a page in the pages folder, from its path there
pageSubDir :: FP.FilePath -> FP.FilePath
pageSubDir relPath = case FP.takeDirectory relPath of
  "." -> ""
  x   -> x




-- | Class of the links to other pages, set by 'walkDoc'
pageLinkClass :: T.Text
pageLinkClass = "kiwi-link-page"

-- | The links to other pages keep the navigation context
linksInContext :: Context -> PD.Pandoc -> PD.Pandoc
linksInContext ctx = PW.walk inContext
  where
    inContext (PD.Link attr@(_, classes, _) txt (url, lName))
      | pageLinkClass `elem` classes = PD.Link attr txt (U.inContext ctx url, lName)
    inContext x = x


data LinkType = PageLink T.Text
              | AnchorLink T.Text
              | ImageLink T.Text
              | FileLink T.Text
              | OtherLink T.Text

-- | Fix links and images paths, and collect links to other pages.
-- Links are collected from the original document, before paths are fixed.
walkDoc :: T.Text -> T.Text -> T.Text -> T.Text ->
           PD.Pandoc -> (PD.Pandoc, [CollectedFromDoc])
walkDoc source pageDir imagesDir filesDir doc =
  (PW.walk fixInlines doc, PW.query collect doc)
    where
      -- chemin des images
      fixInlines (PD.Image attr desc (rawLink, lName)) =
        PD.Image attr desc ((fixImage imagesDir rawLink), lName)
      -- chemin des pages, fichiers, liens externes
      fixInlines (PD.Link attr txt (rawLink, lName)) =
        case parseLink rawLink of
          PageLink l   -> PD.Link (addClass pageLinkClass attr) txt
                                  (pathToPage pageDir l, lName)
          AnchorLink l -> PD.Link (addClass "kiwi-link-anchor" attr) txt
                                  (l, lName)
          ImageLink l  -> PD.Link (addClass "kiwi-link-image" attr) txt
                                  (pathToImage imagesDir l, lName)
          FileLink l   -> PD.Link (newTab $ addClass "kiwi-link-file" attr) txt
                                  (pathToFile filesDir l, lName)
          OtherLink l  -> PD.Link (newTab $ addClass "kiwi-link-external" attr) txt
                                  (l, lName)
      -- reste
      fixInlines x = x

      collect :: PD.Inline -> [CollectedFromDoc]
      collect (PD.Link _ _ (rawLink, _)) = case parseLink rawLink of
        PageLink l -> [CollectedPageLink $ absolutePageId pageDir l]
        _          -> []
      collect _ = []

      parseLink :: T.Text -> LinkType
      parseLink l = let ltM = (PageLink  <$> T.stripPrefix ("page:") l) <|>
                              (ImageLink <$> T.stripPrefix ("image:") l) <|>
                              (FileLink  <$> T.stripPrefix ("file:") l) <|>
                              (if T.isPrefixOf "#" l then Just (AnchorLink l) else Nothing)
                    in fromMaybe (OtherLink l) ltM

      addClass :: T.Text -> PD.Attr -> PD.Attr
      addClass c (idAttr, classAttr, kvs) = (idAttr, c:classAttr, kvs)

      -- opens the link in a new tab
      newTab :: PD.Attr -> PD.Attr
      newTab (idAttr, classAttr, kvs) = (idAttr, classAttr, ("target", "_blank"):kvs)

      -- setId :: T.Text -> PD.Attr -> PD.Attr
      -- setId i (_, classAttr, kvs) = (i, classAttr, kvs)

      fixImage :: T.Text -> T.Text -> T.Text
      fixImage ipd l = let nl = pathToImage ipd <$> T.stripPrefix "image:" l
                       in fromMaybe l nl

      pathToImage d r = pathToR "/image" d r
      pathToFile d r = pathToR "/file" d r
      pathToR n d r =
          case T.uncons r of
            -- absolute link
            Just ('/', l) -> U.joinPathT [n, source, l]
            -- relative link
            _       -> U.joinPathT [n, source, d, r]
      pathToPage d r = U.joinPathT ["/page", source, absolutePageId d r]

      absolutePageId d r =
          case T.uncons r of
            -- absolute link
            Just ('/', l) -> U.normalizePageId l
            -- relative link
            _             -> U.normalizePageId $ U.joinPathT [d, r]



-- | Default metadata of the pages of a source: its config overrides the global
-- defaults, and the page metadata override both
sourceMeta :: Maybe SourceConfig -> MetaData -> MetaData
sourceMeta scM md = md { metaAccess = fromMaybe (metaAccess md) (scM >>= scAccess)
                        , metaLang = fromMaybe (metaLang md) (scM >>= scLang) }


loadPageIO :: FP.FilePath -> FP.FilePath -> T.Text ->
              MetaData -> [CustomMetaConfig] -> Maybe SourceConfig ->
              IO (Either String (Page, T.Text))
loadPageIO fullPath relPath source md cmc scM = do
  -- file may have been removed since directory listing
  fileE <- try $ (,) <$> BS.readFile fullPath <*> D.getModificationTime fullPath
  let rootTag = fromMaybe [] (scRootTag <$> scM)
  return $ case fileE of
    Left (e :: IOException) -> Left $ "unable to read file : " ++ ioeGetErrorString e
    Right (bytes, mt) -> case TE.decodeUtf8' bytes of
     Left e -> Left $ "not a valid UTF-8 file : " ++ show e
     Right c -> case (loadPage fullPath c source (sourceMeta scM md) cmc (pageSubDir relPath)) of
      Left e -> Left $ "unable to decode markdown : " ++ e
      -- the page and the plain text of its content, for indexing, are
      -- evaluated here, so that the document can be freed
      Right p -> let meta = pandocMeta p
                     pId = fromMaybe (pathToPageId relPath) $ metaId meta
                     !content = U.pageContent $ pandocDoc p
                     links = pandocPageLinks p
                     !_ = length links
                     !page = Page { pageUID = (source, pId)
                                  , pageAbsoluteFSPath = fullPath
                                  , pageMTime = mt
                                  , pageText = c
                                  , pageTitle = metaTitle meta
                                  , pageTags =  map (U.prefixNormalizeTag rootTag) $
                                                metaTags meta
                                  , pageAccess =  metaAccess meta
                                  , pageLang = resolveLang (metaLang meta) (metaTitle meta)
                                                           content
                                  , pageLinks = links
                                  , pageArchive = metaArchive meta
                                  , pageCustomMeta = metaCustom meta
                                  }
        in Right (page, content)


parseCustomMeta :: M.Map T.Text [T.Text] -> [CustomMetaConfig] -> [CustomMetaData]
parseCustomMeta m = mapMaybe $
  \cmc -> case M.lookup (cmcName cmc) m of
    Nothing -> Nothing
    Just v  -> let cmdKs = doRead (cmcType cmc) $ concatMap (splitMeta) v
               in Just $ CustomMetaData (cmcName cmc) cmdKs
  where
    doRead CmtText = map KeyText
    doRead CmtInt  = map KeyInt . mapMaybe (readMaybe . T.unpack)
    doRead CmtDate = map KeyDay . mapMaybe readDateMaybe
    doRead CmtBool = map KeyBool . map readBool

    readDateMaybe :: T.Text -> Maybe Time.Day
    readDateMaybe d = Time.parseTimeM True Time.defaultTimeLocale "%Y-%-m-%-d" (T.unpack d)

    readBool :: T.Text -> Bool
    readBool b = if elem (T.toLower b) ["yes", "true", "1"] then True else False
          
  


parseMetaData :: T.Text -> M.Map T.Text [T.Text]
parseMetaData t = let metaLines = T.lines t
  in M.fromListWith (flip (++)) $ 
     mapMaybe parseMetaLine metaLines
  where
    parseMetaLine l = splitOnFirst ":" l <&>
                      \(k,v) -> (T.strip k, [T.strip v])

                        

        
                         
                         




