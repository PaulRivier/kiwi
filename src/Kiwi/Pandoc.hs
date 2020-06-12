{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}

module Kiwi.Pandoc where

import           Control.Applicative ((<|>))
import           Control.Monad.Writer.Lazy (runWriter, tell)
import qualified Data.Map as M
import           Data.Maybe (fromMaybe, catMaybes)
import qualified Data.Text as T
import qualified Data.Time as Time
import qualified System.Directory as D
import qualified System.FilePath as FP
import qualified Text.Pandoc as P
import qualified Text.Pandoc.Walk as PW
-- import qualified Text.Pandoc.Shared as PS
import           Text.Read (readMaybe)

import           Kiwi.Types
import           Kiwi.Utils (getFileContent, splitOnFirst, (>:), for,  splitMeta,
                             pathToPageId)
import qualified Kiwi.Utils as U


loadPage :: T.Text -> T.Text -> MetaData -> [CustomMetaConfig] ->
            FP.FilePath -> Either String PandocPage
loadPage c source md cmc dir = case splitOnFirst "\n\n" c of
  Nothing -> Left "Metadata is not valid"
  Just (metaC, content) -> 
    let docMeta = parseMetaData metaC
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
        imagesDir = T.unpack $ findWith dirT T.concat "images-dir" docMeta
        filesDir  = T.unpack $ findWith dirT T.concat "files-dir" docMeta

    in case (P.runPure $ P.readCommonMark mdConf content) of
         Left e -> Left $ show e
         Right doc'->
           let (doc, collected) = walkDoc (T.unpack source) dir imagesDir filesDir doc'
               colLinks = [ (source, l) | CollectedPageLink l <- collected ]
           in Right (PandocPage doc meta colLinks)
  where
    mdConf = P.def { P.readerExtensions = P.pandocExtensions }
    findWith def join' field meta = fromMaybe def $
                                    fmap join' $
                                    M.lookup field meta



data LinkType = PageLink T.Text
              | AnchorLink T.Text
              | ImageLink T.Text
              | FileLink T.Text
              | OtherLink T.Text

walkDoc :: FP.FilePath -> FP.FilePath -> FP.FilePath -> FP.FilePath ->
           P.Pandoc -> (P.Pandoc, [CollectedFromDoc])
walkDoc source pageDir imagesDir filesDir doc =
  runWriter (PW.walkM (fixInlines pageDir imagesDir filesDir) doc)
    where
      -- chemin des images
      fixInlines _ ipd _ (P.Image attr desc (rawLink, lName)) = do
        return $ P.Image attr desc ((fixImage ipd rawLink), "fig:" <> lName)
      -- chemin des pages, fichiers, liens externes
      fixInlines ppd ipd fpd (P.Link attr txt (rawLink, lName)) = do
        let parsedLink = parseLink rawLink
        case parsedLink of   -- (newAttr, newLink, newTxt)
          PageLink l  -> do
            tell [CollectedPageLink l]
            return $ P.Link (addClass "kiwi-link-page" attr) txt 
                            (pathToPage ppd l, lName)
          AnchorLink l -> return $ P.Link (addClass "kiwi-link-anchor" attr) txt 
                                          (l, lName)
          ImageLink l -> return $ P.Link (addClass "kiwi-link-image" attr) txt 
                                         (pathToImage ipd l, lName)
          FileLink l  -> return $ P.Link (addClass "kiwi-link-file" attr) txt
                                         (pathToFile fpd l, lName)
          OtherLink l -> return $ P.Link (addClass "kiwi-link-external" attr) txt
                                         (l, lName)
      -- reste
      fixInlines _ _ _ x = return $ x

      parseLink :: T.Text -> LinkType
      parseLink l = let ltM = (PageLink  <$> T.stripPrefix ("page:") l) <|>
                              (ImageLink <$> T.stripPrefix ("image:") l) <|>
                              (FileLink  <$> T.stripPrefix ("file:") l) <|>
                              (if T.isPrefixOf "#" l then Just (AnchorLink l) else Nothing)
                    in fromMaybe (OtherLink l) ltM

      addClass :: T.Text -> P.Attr -> P.Attr
      addClass c (idAttr, classAttr, kvs) = (idAttr, c:classAttr, kvs)

      -- setId :: T.Text -> P.Attr -> P.Attr
      -- setId i (_, classAttr, kvs) = (i, classAttr, kvs)

      fixImage :: FP.FilePath -> T.Text -> T.Text
      fixImage ipd l = let nl = pathToImage ipd <$> T.stripPrefix "image:" l
                       in fromMaybe l nl

      pathToImage d r = pathToR "/image" d r
      pathToFile d r = pathToR "/file" d r
      pathToPage d r = pathToR "/page" d r
      pathToR n d r =
          case T.uncons r of
            -- absolute link
            Just ('/', l) -> T.pack $ FP.joinPath [n, source, T.unpack l]
            -- relative link
            _       -> T.pack $ FP.joinPath [n, source, d, T.unpack r]



loadPageIO :: FP.FilePath -> FP.FilePath -> T.Text ->
              MetaData -> [CustomMetaConfig] -> Maybe SourceConfig ->
              IO Page
loadPageIO fullPath relPath source md cmc scM = do
  cM <- getFileContent fullPath
  mt <- D.getModificationTime fullPath
  let pageDir = FP.takeDirectory relPath
  let rootTag = fromMaybe [] (scRootTag <$> scM)
  case cM of
    Nothing -> error ("file not found : " ++ fullPath)
    Just c -> case (loadPage c source md cmc pageDir) of
      Left _ -> error "Unable to read markdown"
      Right p -> let meta = pandocMeta p
                     pId = fromMaybe (pathToPageId relPath) $ metaId meta in
        return $ Page { pageUID = (source, pId)
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
parseCustomMeta m c = catMaybes $ for c $
  \cmc -> case M.lookup (cmcName cmc) m of
    Nothing -> Nothing
    Just v  -> let cmdKs = doRead (cmcType cmc) $ concatMap (splitMeta) v
               in Just $ CustomMetaData (cmcName cmc) cmdKs
  where 
    doRead CmtText = map KeyText
    doRead CmtInt  = map KeyInt . catMaybes . map (readMaybe . T.unpack)
    doRead CmtDate = map KeyDay . catMaybes . map readDateMaybe
    doRead CmtBool = map KeyBool . map readBool

    readDateMaybe :: T.Text -> Maybe Time.Day
    readDateMaybe d = Time.parseTimeM True Time.defaultTimeLocale "%Y-%-m-%-d" (T.unpack d)

    readBool :: T.Text -> Bool
    readBool b = if elem (T.toLower b) ["yes", "true", "1"] then True else False
          
  


parseMetaData :: T.Text -> M.Map T.Text [T.Text]
parseMetaData t = let metaLines = T.lines t
  in M.fromListWith (flip (++)) $ 
     catMaybes $ map parseMetaLine metaLines
  where
    parseMetaLine l = (splitOnFirst ":" l) >:
                      \(k,v) -> (T.strip k, [T.strip v])

                        

        
                         
                         
