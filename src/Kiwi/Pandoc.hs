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
  , Loaded (..)
  , loadPage         -- exported for tests
  , splitPage        -- exported for tests
  , parseMetaData    -- exported for tests
  , parseCustomMeta  -- exported for tests
  , sourceMeta       -- exported for tests
  ) where

import           Control.Applicative ((<|>))
import           Control.Exception (IOException, evaluate, try)
import           Control.Monad (mfilter)
import qualified Commonmark as Cm
import qualified Commonmark.Extensions as CmEx
import qualified Commonmark.Pandoc as CmP

import qualified Data.ByteString as BS
import           Data.Functor.Identity (Identity, runIdentity)
import qualified Data.Map as M
import           Data.Char (isAlphaNum, isSpace)
import           Data.Maybe (fromMaybe, isNothing, mapMaybe)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import qualified System.Directory as D
import qualified System.FilePath as FP
import           System.IO.Error (ioeGetErrorString)
-- import qualified Text.Pandoc as P
import qualified Text.Pandoc.Builder as PB
import qualified Text.Pandoc.Definition as PD
import qualified Text.Pandoc.Walk as PW
-- import qualified Text.Pandoc.Shared as PS

import           Kiwi.HtmlWriter (pageTargetAttrs)
import           Kiwi.LangDetect (resolveLang)
import           Kiwi.Types
import           Kiwi.Tags (splitMeta, splitTags, prefixNormalizeTag)
import qualified Kiwi.Url as Url
import           Kiwi.Utils (splitOnFirst, pathToPageId, parseLangSetting, parseBool,
                             readCustomValue)
import qualified Kiwi.Utils as U



-- | A page from the text of its file: its header is read once, here. Fails
-- when the header is invalid (see 'splitPage'), or the markdown unreadable.
loadPage :: FP.FilePath -> T.Text -> T.Text -> MetaData -> [CustomMetaConfig] ->
              FP.FilePath -> Either String PandocPage
loadPage path c source md cmc dir = do
    (docMeta, content) <- splitPage (map cmcName cmc) c
        -- an empty id is ignored: the page gets the id of its path
    let meta = md { metaId    = mfilter (not . T.null) $
                                T.concat <$> M.lookup "id" docMeta
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
        dirT = T.pack dir
        imagesDir = maybe dirT T.concat $ M.lookup "images-dir" docMeta
        filesDir  = maybe dirT T.concat $ M.lookup "files-dir" docMeta
    (doc, colLinks) <- loadDoc path source dirT imagesDir filesDir content
    return $ PandocPage doc meta colLinks docMeta content imagesDir filesDir
  where
    -- mdConf = P.def { P.readerExtensions = P.pandocExtensions }
    findWith def join' field meta = fromMaybe def $
                                    fmap join' $
                                    M.lookup field meta


-- | Keys of the metadata header of a page that kiwi does not use: neither
-- built-in (read by 'loadPage') nor declared custom metadata
unknownMetaKeys :: [CustomMetaConfig] -> M.Map T.Text [T.Text] -> [T.Text]
unknownMetaKeys cmc docMeta = filter (`notElem` known) $ M.keys docMeta
  where
    known = [ "id", "title", "tags", "access", "lang", "archive", "images-dir"
            , "files-dir" ] ++ map cmcName cmc


-- | Metadata of the header of a page whose value can not be read, with this
-- value. For "archive" and "lang" (built-in), the page has the default
-- value instead; for the custom metadata of type int, date or bool, the
-- value is ignored.
invalidMetaValues :: [CustomMetaConfig] -> M.Map T.Text [T.Text] -> [(T.Text, T.Text)]
invalidMetaValues cmc docMeta =
  let lastValue k = last <$> M.lookup k docMeta
  in [ ("archive", v) | Just v <- [lastValue "archive"], isNothing (parseBool v) ]
     ++ [ ("lang", v) | Just v <- [lastValue "lang"]
                      , either (const True) (const False) (parseLangSetting v) ]
     ++ [ (cmcName cfg, v) | cfg <- cmc
                           , v <- maybe [] (concatMap splitMeta) $
                                  M.lookup (cmcName cfg) docMeta
                           , isNothing (readCustomValue (cmcType cfg) v) ]


-- | Metadata and content of a page (see 'normalizeNewlines'): the header is the
-- first block of the file, until its first blank line, and the content is
-- what follows. A file without blank line is a header only, with an empty
-- content. Fails when the header is invalid (see 'parseMetaData'); the given
-- keys (custom metadata names) are allowed even with several words.
splitPage :: [T.Text] -> T.Text -> Either String (M.Map T.Text [T.Text], T.Text)
splitPage keys c =
  let normC = normalizeNewlines c
      (metaC, content) = fromMaybe (normC, T.empty) $ splitOnFirst "\n\n" normC
  in (, content) <$> parseMetaData keys metaC

-- | The text of a file without its initial byte order mark (U+FEFF, added by
-- some Windows editors) if any, and with Unix line endings
normalizeNewlines :: T.Text -> T.Text
normalizeNewlines c = T.replace "\r\n" "\n" $ fromMaybe c $ T.stripPrefix "\xFEFF" c


-- | Document of a page from its markdown content, with its links fixed, and
-- the links to other pages. Links are relative to the folder of the page,
-- or to its images and files folders.
loadDoc :: FP.FilePath -> T.Text -> T.Text -> T.Text -> T.Text -> T.Text ->
           Either String (PD.Pandoc, [PageUID])
loadDoc path source dirT imagesDir filesDir content =
    let syntaxSpec = Cm.defaultSyntaxSpec <>
                     CmEx.strikethroughSpec <> CmEx.pipeTableSpec <> CmEx.autolinkSpec <>
                     CmEx.taskListSpec <> CmEx.footnoteSpec <>
                     CmEx.autoIdentifiersSpec
    -- the last row of a table is not read as a row without a newline after
    -- it. An extra newline only changes an unclosed fenced code block at the
    -- end, which gets one more empty line.
    in case runIdentity (Cm.commonmarkWith syntaxSpec path (content <> "\n")
                         :: Identity (Either Cm.ParseError (CmP.Cm () PB.Blocks))) of
         Left e -> Left $ show e
         Right cmBlocks->
           let doc' = PB.doc (CmP.unCm cmBlocks)
               (doc, collected) = walkDoc source dirT imagesDir filesDir doc'
               colLinks = [ (source, l) | CollectedPageLink l <- collected ]
           in Right (doc, colLinks)


-- | Document of a loaded page, parsed again from its content to be
-- displayed. The header is not read again: the page keeps what it gave.
pageDoc :: FP.FilePath -> Page -> Either String PD.Pandoc
pageDoc contentDir' p =
  let path = pageAbsoluteFSPath p
      source = fst $ pageUID p
      relPath = snd $ pagePath contentDir' path
  in fst <$> loadDoc path source (T.pack $ pageSubDir relPath) (pageImagesDir p)
                     (pageFilesDir p) (pageBody p)


-- | Document of a file embedded in kiwi (README, CommonMark cheatsheet), with
-- or without a metadata header, and the title of its header. Its links are
-- not relative to any source.
embeddedDoc :: FP.FilePath -> Bool -> T.Text -> Either String (Maybe T.Text, PD.Pandoc)
embeddedDoc name withHeader c = do
  (docMeta, content) <- if withHeader then splitPage [] c
                        else Right (M.empty, normalizeNewlines c)
  let title = T.intercalate " " <$> M.lookup "title" docMeta
  (doc, _) <- loadDoc name "" "" "" "" content
  return (title, doc)


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
      | pageLinkClass `elem` classes = PD.Link attr txt (Url.inContext ctx url, lName)
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
          PageLink l   -> let (pId, fragment) = pageTarget pageDir l
                              uid = (source, pId)
                          in PD.Link (addKVs (pageTargetAttrs uid) $
                                      addClass pageLinkClass attr) txt
                                     (Url.pageUIDUrl uid <> fragment, lName)
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
        PageLink l -> [CollectedPageLink $ fst $ pageTarget pageDir l]
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

      addKVs :: [(T.Text, T.Text)] -> PD.Attr -> PD.Attr
      addKVs kvs' (idAttr, classAttr, kvs) = (idAttr, classAttr, kvs ++ kvs')

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
      -- the target may be percent-encoded, and have a fragment
      -- ("doc.pdf#page=2"); each segment is encoded, with the source
      pathToR n d r =
        let (path, fragment) = T.breakOn "#" r
            segments = absoluteSegments d $ Url.urlDecodeText path
        in T.intercalate "/" (n : map Url.urlEncodeText (source : segments))
           <> fragment



-- | Id of the page targeted by a "page:" link written in a page of the
-- given folder, and the fragment of the link ("#anchor"), if any. The target
-- may be percent-encoded, and may end with the extension of the page file
-- (.md or .markdown), which is removed. See 'absoluteSegments' for the
-- resolution of relative targets.
pageTarget :: T.Text -> T.Text -> (T.Text, T.Text)
pageTarget pageDir l =
  let (path, fragment) = T.breakOn "#" l
      noExt p = fromMaybe p $ T.stripSuffix ".md" p <|> T.stripSuffix ".markdown" p
      pId = T.intercalate ":" $ absoluteSegments pageDir $ noExt $ Url.urlDecodeText path
  in (pId, fragment)

-- | Segments of a path written in a page of the given folder: an absolute
-- path ("/a/b") starts at the top of the folder of the source, a relative
-- one from the folder. The "." and ".." segments of the path are resolved,
-- and its empty segments removed. A ".." at the top of the source folder is
-- ignored (as by browsers at the root of a site), so that a link never
-- leaves its source.
absoluteSegments :: T.Text -> T.Text -> [T.Text]
absoluteSegments dir path =
  let full = case T.uncons path of
        Just ('/', l) -> l
        _             -> T.concat [dir, "/", path]
  in reverse $ foldl resolve [] $ T.splitOn "/" full
  where
    resolve acc ""   = acc
    resolve acc "."  = acc
    resolve acc ".." = drop 1 acc
    resolve acc s    = s : acc



-- | Default metadata of the pages of a source: its config overrides the global
-- defaults, and the page metadata override both
sourceMeta :: Maybe SourceConfig -> MetaData -> MetaData
sourceMeta scM md = md { metaAccess = fromMaybe (metaAccess md) (scM >>= scAccess)
                        , metaLang = fromMaybe (metaLang md) (scM >>= scLang) }


-- | A loaded page, and what its header has that kiwi can not use (see
-- 'unknownMetaKeys' and 'invalidMetaValues')
data Loaded = Loaded { loadedPage :: !Page
                     , loadedUnknownKeys :: ![T.Text]
                     , loadedInvalidValues :: ![(T.Text, T.Text)]
                     }

loadPageIO :: FP.FilePath -> FP.FilePath -> T.Text ->
              MetaData -> [CustomMetaConfig] -> Maybe SourceConfig ->
              IO (Either String Loaded)
loadPageIO fullPath relPath source md cmc scM = do
  -- file may have been removed since directory listing. The time is read
  -- first: a file modified while it is read then gets another time, and is
  -- loaded again at the next reload.
  fileE <- try $ flip (,) <$> D.getModificationTime fullPath <*> BS.readFile fullPath
  let rootTag = fromMaybe [] (scRootTag <$> scM)
  -- evaluated by the caller thread, so that pages are parsed concurrently
  -- (see 'Kiwi.PagesDB.updatePagesDB')
  evaluate $ case fileE of
    Left (e :: IOException) -> Left $ "unable to read file : " ++ ioeGetErrorString e
    Right (bytes, mt) -> case TE.decodeUtf8' bytes of
     Left e -> Left $ "not a valid UTF-8 file : " ++ show e
     Right c -> case (loadPage fullPath c source (sourceMeta scM md) cmc (pageSubDir relPath)) of
      Left e -> Left $ "unable to load the page : " ++ e
      -- the page, the plain text of its content and the issues of its
      -- header are evaluated here, so that the document can be freed
      Right p -> let meta = pandocMeta p
                     pId = fromMaybe (pathToPageId relPath) $ metaId meta
                     !content = U.pageContent $ pandocDoc p
                     links = pandocPageLinks p
                     !_ = length links
                     !header = pandocHeader p
                     unknown = unknownMetaKeys cmc header
                     invalid = invalidMetaValues cmc header
                     !_ = length unknown + length invalid
                     !page = Page { pageUID = (source, pId)
                                  , pageAbsoluteFSPath = fullPath
                                  , pageMTime = mt
                                  , pageBody = pandocBody p
                                  , pageImagesDir = pandocImagesDir p
                                  , pageFilesDir = pandocFilesDir p
                                  , pagePlainText = content
                                  , pageTitle = metaTitle meta
                                  , pageTags =  map (prefixNormalizeTag rootTag) $
                                                metaTags meta
                                  , pageAccess =  metaAccess meta
                                  , pageLang = resolveLang (metaLang meta) (metaTitle meta)
                                                           content
                                  , pageLinks = links
                                  , pageArchive = metaArchive meta
                                  , pageCustomMeta = metaCustom meta
                                  }
        in Right $ Loaded page unknown invalid


-- | Custom metadata of a page header. The values that can not be read for
-- their type are ignored (see 'invalidMetaValues').
parseCustomMeta :: M.Map T.Text [T.Text] -> [CustomMetaConfig] -> [CustomMetaData]
parseCustomMeta m = mapMaybe $
  \cmc -> case M.lookup (cmcName cmc) m of
    Nothing -> Nothing
    Just v  -> let cmdKs = mapMaybe (readCustomValue (cmcType cmc)) $
                           concatMap (splitMeta) v
               in Just $ CustomMetaData (cmcName cmc) cmdKs

-- | Metadata of the header of a page. Each line is a field ("key: value",
-- with a key in one word of letters, digits, '-' and '_', or one of the
-- given keys), a separator made of '-' or '.' ("---", "..."), or blank.
-- Anything else (a heading, some text) is an error: the first block of a
-- page must be its metadata.
parseMetaData :: [T.Text] -> T.Text -> Either String (M.Map T.Text [T.Text])
parseMetaData keys t =
  M.fromListWith (flip (++)) . concat <$>
  traverse parseMetaLine (zip [1 :: Int ..] $ T.lines t)
  where
    parseMetaLine (n, l)
      | T.all isSpace l = Right []
      | T.all (`elem` ['-', '.']) (T.strip l) = Right []
      | Just (k, v) <- splitOnFirst ":" l, validKey (T.strip k) =
          Right [(T.strip k, [T.strip v])]
      | otherwise = Left $ "invalid metadata header, line " ++ show n
    validKey k = k `elem` keys ||
                 (not (T.null k) && T.all (\ch -> isAlphaNum ch || ch `elem` ['-', '_']) k)
