{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Kiwi.Pandoc
  ( loadPageIO
  , linksInContext
  , loadPage         -- exported for tests
  , parseMetaData    -- exported for tests
  , parseCustomMeta  -- exported for tests
  ) where

import           Control.Applicative ((<|>))
import           Control.Exception (IOException, try)
import qualified Commonmark as Cm
import qualified Commonmark.Extensions as CmEx
import qualified Commonmark.Pandoc as CmP

import qualified Data.ByteString as BS
import           Data.Functor ((<&>))
import qualified Data.Map as M
import           Data.Maybe (fromMaybe, mapMaybe)
import qualified Data.Set as S
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

import           Kiwi.Types
import           Kiwi.Utils (splitOnFirst, splitMeta, pathToPageId)
import qualified Kiwi.Utils as U



loadPage :: FP.FilePath -> T.Text -> T.Text -> MetaData -> [CustomMetaConfig] ->
              FP.FilePath -> Either String PandocPage
loadPage path c source md cmc dir =
    -- windows line endings are normalized, and a file without blank line
    -- is considered as metadata only, with empty content
    let (metaC, content) = fromMaybe (normC, T.empty) $ splitOnFirst "\n\n" normC
        docMeta = parseMetaData metaC
        meta = md { metaId    = T.concat <$> M.lookup "id" docMeta
                  , metaTitle = findWith (metaTitle md) (T.intercalate " ")
                                         "title" docMeta
                  , metaTags = findWith (metaTags md) (splitMeta . T.intercalate ",")
                                        "tags" docMeta
                  , metaAccess = findWith (metaAccess md) (splitMeta . T.intercalate ",")
                                          "access" docMeta
                  , metaLang = fromMaybe (metaLang md)
                                         ( M.lookup "lang" docMeta >>=
                                           readMaybe . T.unpack . last )
                  , metaCustom = parseCustomMeta docMeta cmc
                  }
        dirT = T.pack $ dir
        imagesDir = findWith dirT T.concat "images-dir" docMeta
        filesDir  = findWith dirT T.concat "files-dir" docMeta
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
           in Right (PandocPage doc meta colLinks)
  where
    normC = T.replace "\r\n" "\n" c
    -- mdConf = P.def { P.readerExtensions = P.pandocExtensions }
    findWith def join' field meta = fromMaybe def $
                                    fmap join' $
                                    M.lookup field meta




-- | Class of the links to other pages, set by 'walkDoc'
pageLinkClass :: T.Text
pageLinkClass = "kiwi-link-page"

-- | The links to other pages keep the sources context
linksInContext :: S.Set T.Text -> PD.Pandoc -> PD.Pandoc
linksInContext sources = PW.walk inContext
  where
    inContext (PD.Link attr@(_, classes, _) txt (url, lName))
      | pageLinkClass `elem` classes = PD.Link attr txt (U.inContext sources url, lName)
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



loadPageIO :: FP.FilePath -> FP.FilePath -> T.Text ->
              MetaData -> [CustomMetaConfig] -> Maybe SourceConfig ->
              IO (Either String Page)
loadPageIO fullPath relPath source md cmc scM = do
  -- file may have been removed since directory listing
  fileE <- try $ (,) <$> BS.readFile fullPath <*> D.getModificationTime fullPath
  let pageDir = case FP.takeDirectory relPath of
        "." -> ""
        x   -> x
  let rootTag = fromMaybe [] (scRootTag <$> scM)
  return $ case fileE of
    Left (e :: IOException) -> Left $ "unable to read file : " ++ ioeGetErrorString e
    Right (bytes, mt) -> case TE.decodeUtf8' bytes of
     Left e -> Left $ "not a valid UTF-8 file : " ++ show e
     Right c -> case (loadPage fullPath c source md cmc pageDir) of
      Left e -> Left $ "unable to decode markdown : " ++ e
      Right p -> let meta = pandocMeta p
                     pId = fromMaybe (pathToPageId relPath) $ metaId meta in
        Right $ Page { pageUID = (source, pId)
                      , pageAbsoluteFSPath = fullPath
                      , pageMTime = mt
                      , pageDoc = pandocDoc p
                      , pageTitle = metaTitle meta
                      , pageTags =  map (U.prefixNormalizeTag rootTag) $
                                    metaTags meta
                      , pageAccess =  metaAccess meta
                      , pageLang = metaLang meta
                      , pageLinks = pandocPageLinks p
                      , pageCustomMeta = metaCustom meta
                      }


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

                        

        
                         
                         




