{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Kiwi.Pandoc
  ( loadPageIO
  , pageDoc
  , embeddedDoc
  , pagePath
  , linksInContext
  , Loaded (..)
  , loadPage         -- exported for tests
  ) where

import           Control.Applicative ((<|>))
import           Control.Exception (IOException, evaluate, try)
import qualified Commonmark as Cm
import qualified Commonmark.Extensions as CmEx
import qualified Commonmark.Pandoc as CmP

import qualified Data.ByteString as BS
import           Data.Functor.Identity (runIdentity)
import           Data.Typeable (Typeable)
import           Data.Maybe (fromMaybe)
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

import           Kiwi.HtmlWriter (pageTargetAttrs, lineKey)
import           Kiwi.LangDetect (resolveLang)
import           Kiwi.Meta (emptyMeta, mergeMeta, sourceMeta, splitPage, normalizeNewlines,
                            unknownMetaKeys, invalidMetaValues)
import           Kiwi.Types
import           Kiwi.Tags (prefixNormalizeTag)
import qualified Kiwi.Url as Url
import           Kiwi.Utils (pathToPageId)
import qualified Kiwi.Utils as U



-- | A page from the text of its file: its header is read once, here, and
-- merged over the given defaults. Fails when the header is invalid (see
-- 'Kiwi.Meta.splitPage'), or the markdown unreadable.
loadPage :: FP.FilePath -> T.Text -> T.Text -> MetaData -> [CustomMetaConfig] ->
              FP.FilePath -> Either String PandocPage
loadPage path c source md cmc dir = do
    (header, content) <- splitPage cmc c
    let meta = mergeMeta md header
        dirT = T.pack dir
        imagesDir = fromMaybe dirT $ metaImagesDir meta
        filesDir  = fromMaybe dirT $ metaFilesDir meta
    (doc, colLinks) <- loadDoc False path source dirT imagesDir filesDir content
    return $ PandocPage doc meta colLinks content imagesDir filesDir


-- | Document of a page from its markdown content, with its links fixed, and
-- the links to other pages. Links are relative to the folder of the page,
-- or to its images and files folders. With lines, each top-level block is
-- wrapped in a div holding its first line in the content (see 'lineKey').
loadDoc :: Bool -> FP.FilePath -> T.Text -> T.Text -> T.Text -> T.Text -> T.Text ->
           Either String (PD.Pandoc, [PageUID])
loadDoc withLines path source dirT imagesDir filesDir content =
    -- the last row of a table is not read as a row without a newline after
    -- it. An extra newline only changes an unclosed fenced code block at the
    -- end, which gets one more empty line.
    let parsed = if withLines
                 then topLevelLines . CmP.unCm <$> parseCm @BlockLines path (content <> "\n")
                 else PB.toList . CmP.unCm <$> parseCm @() path (content <> "\n")
    in case parsed of
         Left e -> Left $ show e
         Right blocks ->
           let doc' = PD.Pandoc PD.nullMeta blocks
               (doc, collected) = walkDoc source dirT imagesDir filesDir doc'
               colLinks = [ (source, l) | CollectedPageLink l <- collected ]
           in Right (doc, colLinks)

-- | Blocks of a markdown content, with the source ranges given by the type
-- of the 'CmP.Cm' (none for '()', see 'BlockLines')
parseCm :: forall r. (Typeable r, Cm.Rangeable (CmP.Cm r PB.Inlines),
                      Cm.Rangeable (CmP.Cm r PB.Blocks)) =>
           FP.FilePath -> T.Text -> Either Cm.ParseError (CmP.Cm r PB.Blocks)
parseCm path content = runIdentity $ Cm.commonmarkWith syntaxSpec path content
  where
    syntaxSpec = Cm.defaultSyntaxSpec <>
                 CmEx.strikethroughSpec <> CmEx.pipeTableSpec <> CmEx.autolinkSpec <>
                 CmEx.taskListSpec <> CmEx.footnoteSpec <>
                 CmEx.autoIdentifiersSpec

-- | Source ranges of a parse: blocks only, each one with its first line
-- (see 'withLine'; inlines have none)
data BlockLines

instance Cm.Rangeable (CmP.Cm BlockLines PB.Inlines) where
  ranged _ x = x

instance Cm.Rangeable (CmP.Cm BlockLines PB.Blocks) where
  ranged r (CmP.Cm bs) = case Cm.unSourceRange r of
    (start, _) : _ -> CmP.Cm $ withLine (Cm.sourceLine start) <$> bs
    []             -> CmP.Cm bs

-- | The block with its first line (see 'lineKey'): in its attributes when it
-- has some, or in a div around it. A block may get several ranges: the
-- first one is kept. The range of a pipe table starts at its delimiter row,
-- after its header row.
withLine :: Int -> PD.Block -> PD.Block
withLine l b
  | isLineDiv b = b
  | otherwise = case blockAttr b of
      Just ((i, cs, kvs), set)
        | lineKey `elem` map fst kvs -> b
        | otherwise -> set (i, cs, (lineKey, line) : kvs)
      Nothing -> PD.Div ("", [], [(lineKey, line)]) [b]
  where
    line = T.pack $ show $ case b of
      PD.Table {} -> l - 1
      _           -> l

-- | Whether the block is a div that only holds the line of its block
isLineDiv :: PD.Block -> Bool
isLineDiv (PD.Div ("", [], kvs@(_:_)) [_]) = all ((== lineKey) . fst) kvs
isLineDiv _ = False

-- | The attributes of a block that has some, and the block with other ones
blockAttr :: PD.Block -> Maybe (PD.Attr, PD.Attr -> PD.Block)
blockAttr b = case b of
  PD.Header n attr ils         -> Just (attr, \a -> PD.Header n a ils)
  PD.CodeBlock attr t          -> Just (attr, \a -> PD.CodeBlock a t)
  PD.Table attr c s h bs f     -> Just (attr, \a -> PD.Table a c s h bs f)
  PD.Figure attr c bs          -> Just (attr, \a -> PD.Figure a c bs)
  PD.Div attr bs               -> Just (attr, \a -> PD.Div a bs)
  _                            -> Nothing

-- | The blocks, with their lines kept at the top level only
topLevelLines :: PB.Blocks -> [PD.Block]
topLevelLines = map top . PB.toList
  where
    top b = case b of
      PD.Div (_, _, (_, l) : _) [inner] | isLineDiv b -> setLine l (clean inner)
      _ -> case blockAttr b of
        Just ((_, _, kvs), _) | Just l <- lookup lineKey kvs -> setLine l (clean b)
        _ -> clean b
    clean :: PD.Block -> PD.Block
    clean = PW.walk dropLine . PW.walk (concatMap unwrap)
    unwrap :: PD.Block -> [PD.Block]
    unwrap b@(PD.Div _ bs) | isLineDiv b = bs
    unwrap b = [b]
    dropLine b = case blockAttr b of
      Just ((i, cs, kvs), set) -> set (i, cs, filter ((/= lineKey) . fst) kvs)
      Nothing -> b
    -- the line of the original block (that of a table is already fixed)
    setLine l b = case blockAttr b of
      Just ((i, cs, kvs), set) -> set (i, cs, (lineKey, l) : kvs)
      Nothing -> PD.Div ("", [], [(lineKey, l)]) [b]


-- | Document of a loaded page, parsed again from its content to be
-- displayed. The header is not read again: the page keeps what it gave.
pageDoc :: FP.FilePath -> Page -> Either String PD.Pandoc
pageDoc contentDir' p =
  let path = pageAbsoluteFSPath p
      source = fst $ pageUID p
      relPath = snd $ pagePath contentDir' path
  in fst <$> loadDoc True path source (T.pack $ pageSubDir relPath) (pageImagesDir p)
                     (pageFilesDir p) (pageBody p)


-- | Document of a file embedded in kiwi (README, CommonMark cheatsheet), with
-- or without a metadata header, and the title of its header. Its links are
-- not relative to any source.
embeddedDoc :: FP.FilePath -> Bool -> T.Text -> Either String (Maybe T.Text, PD.Pandoc)
embeddedDoc name withHeader c = do
  (header, content) <- if withHeader then splitPage [] c
                       else Right (emptyMeta, normalizeNewlines c)
  let title = metaTitle header
  (doc, _) <- loadDoc False name "" "" "" "" content
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
                     -- the defaults of the config have every field
                     title = fromMaybe "" $ metaTitle meta
                     !content = U.pageContent $ pandocDoc p
                     links = pandocPageLinks p
                     !_ = length links
                     unknown = unknownMetaKeys cmc meta
                     invalid = invalidMetaValues cmc meta
                     !_ = length unknown + length invalid
                     !page = Page { pageUID = (source, pId)
                                  , pageAbsoluteFSPath = fullPath
                                  , pageMTime = mt
                                  , pageBody = pandocBody p
                                  , pageImagesDir = pandocImagesDir p
                                  , pageFilesDir = pandocFilesDir p
                                  , pagePlainText = content
                                  , pageTitle = title
                                  , pageTags =  map (prefixNormalizeTag rootTag) $
                                                fromMaybe [] $ metaTags meta
                                  -- without access, only the admins can read
                                  , pageAccess = fromMaybe [] $ metaAccess meta
                                  , pageLang = resolveLang
                                                 (fromMaybe defaultLangSetting $ metaLang meta)
                                                 title content
                                  , pageLinks = links
                                  , pageArchive = fromMaybe False $ metaArchive meta
                                  , pageCustomMeta = metaCustom meta
                                  }
        in Right $ Loaded page unknown invalid
