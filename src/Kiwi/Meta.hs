-- | The metadata header of the pages: reading, combination ('mergeMeta',
-- 'clearMeta', 'keepOnlyMeta') and checks. The same 'MetaData' type holds
-- the header of a file (absent fields are 'Nothing'), the defaults of the
-- config and of the sources, and the metadata of a page: its header merged
-- over the defaults.
module Kiwi.Meta
  ( emptyMeta
  , mergeMeta
  , clearMeta
  , keepOnlyMeta
  , sourceMeta
  , splitPage
  , normalizeNewlines
  , parseHeader
  , unknownMetaKeys
  , invalidMetaValues
  , parseHeaderFields  -- exported for tests
  ) where

import           Control.Applicative ((<|>))
import           Control.Monad (mfilter)
import           Data.Char (isAlphaNum, isSpace)
import qualified Data.Map.Strict as M
import           Data.Maybe (fromMaybe, mapMaybe)
import qualified Data.Text as T

import           Kiwi.Tags (splitMeta, splitTags)
import           Kiwi.Types
import           Kiwi.Utils (splitOnFirst, parseLangSetting, parseBool, readCustomValue)


-- | Metadata without any field
emptyMeta :: MetaData
emptyMeta = MetaData { metaId = Nothing
                     , metaTitle = Nothing
                     , metaTags = Nothing
                     , metaAccess = Nothing
                     , metaLang = Nothing
                     , metaArchive = Nothing
                     , metaImagesDir = Nothing
                     , metaFilesDir = Nothing
                     , metaCustom = []
                     , metaOther = M.empty
                     }

-- | The fields of the second metadata, and those of the first one that the
-- second does not have (custom and other fields: by name)
mergeMeta :: MetaData -> MetaData -> MetaData
mergeMeta a b =
  MetaData { metaId = metaId b <|> metaId a
           , metaTitle = metaTitle b <|> metaTitle a
           , metaTags = metaTags b <|> metaTags a
           , metaAccess = metaAccess b <|> metaAccess a
           , metaLang = metaLang b <|> metaLang a
           , metaArchive = metaArchive b <|> metaArchive a
           , metaImagesDir = metaImagesDir b <|> metaImagesDir a
           , metaFilesDir = metaFilesDir b <|> metaFilesDir a
           , metaCustom = metaCustom b ++
                          filter ((`notElem` map cmdName (metaCustom b)) . cmdName)
                                 (metaCustom a)
           , metaOther = M.union (metaOther b) (metaOther a)
           }

-- | The metadata without the fields of these keys (as written in a header:
-- "title", "access", names of custom metadata…), and without their invalid
-- values
clearMeta :: [T.Text] -> MetaData -> MetaData
clearMeta keys = filterMeta (`notElem` keys)

-- | The metadata with the fields of these keys only (see 'clearMeta')
keepOnlyMeta :: [T.Text] -> MetaData -> MetaData
keepOnlyMeta keys = filterMeta (`elem` keys)

filterMeta :: (T.Text -> Bool) -> MetaData -> MetaData
filterMeta keep m =
  MetaData { metaId = field "id" metaId
           , metaTitle = field "title" metaTitle
           , metaTags = field "tags" metaTags
           , metaAccess = field "access" metaAccess
           , metaLang = field "lang" metaLang
           , metaArchive = field "archive" metaArchive
           , metaImagesDir = field "images-dir" metaImagesDir
           , metaFilesDir = field "files-dir" metaFilesDir
           , metaCustom = filter (keep . cmdName) (metaCustom m)
           , metaOther = M.filterWithKey (\k _ -> keep k) (metaOther m)
           }
  where
    field :: T.Text -> (MetaData -> Maybe a) -> Maybe a
    field k f = if keep k then f m else Nothing

-- | Default metadata of the pages of a source: its config overrides the global
-- defaults, and the page metadata override both
sourceMeta :: Maybe SourceConfig -> MetaData -> MetaData
sourceMeta scM md = mergeMeta md emptyMeta { metaAccess = scM >>= scAccess
                                           , metaLang = scM >>= scLang }


-- | Header and content of a page (see 'normalizeNewlines'): the header is the
-- first block of the file, until its first blank line, and the content is
-- what follows. A file without blank line is a header only, with an empty
-- content. Fails when the header is invalid (see 'parseHeaderFields').
splitPage :: [CustomMetaConfig] -> T.Text -> Either String (MetaData, T.Text)
splitPage cmc c =
  let normC = normalizeNewlines c
      (metaC, content) = fromMaybe (normC, T.empty) $ splitOnFirst "\n\n" normC
  in (, content) <$> parseHeader cmc metaC

-- | The text of a file without its initial byte order mark (U+FEFF, added by
-- some Windows editors) if any, and with Unix line endings
normalizeNewlines :: T.Text -> T.Text
normalizeNewlines c = T.replace "\r\n" "\n" $ fromMaybe c $ T.stripPrefix "\xFEFF" c


-- | Metadata of a header (see 'parseHeaderFields'). The values that can not
-- be read are kept in 'metaOther', with the unknown keys.
parseHeader :: [CustomMetaConfig] -> T.Text -> Either String MetaData
parseHeader cmc t = fromFields cmc <$> parseHeaderFields (map cmcName cmc) t

-- | Metadata of the fields of a header (key: values of its lines). Repeated
-- fields are joined, except "lang" and "archive", whose last line is used.
fromFields :: [CustomMetaConfig] -> M.Map T.Text [T.Text] -> MetaData
fromFields cmc fields =
  MetaData { -- an empty id is ignored: the page gets the id of its path
             metaId = mfilter (not . T.null) $ T.concat <$> look "id"
           , metaTitle = T.intercalate " " <$> look "title"
           , metaTags = splitTags . T.intercalate "," <$> look "tags"
           , metaAccess = splitMeta . T.intercalate "," <$> look "access"
           , metaLang = fst lang
           , metaArchive = fst archive
           , metaImagesDir = T.concat <$> look "images-dir"
           , metaFilesDir = T.concat <$> look "files-dir"
           , metaCustom = map fst customs
           , metaOther = M.fromList $ unknown ++ snd lang ++ snd archive ++
                                      concatMap snd customs
           }
  where
    look k = M.lookup k fields
    -- the last value, if it can be read, or the field as invalid
    lastValue :: T.Text -> (T.Text -> Maybe a) -> (Maybe a, [(T.Text, [T.Text])])
    lastValue k parse = case last <$> look k of
      Nothing -> (Nothing, [])
      Just v  -> case parse v of
        Just x  -> (Just x, [])
        Nothing -> (Nothing, [(k, [v])])
    lang = lastValue "lang" (either (const Nothing) Just . parseLangSetting)
    archive = lastValue "archive" parseBool
    customs = [ (CustomMetaData (cmcName c) (mapMaybe snd values), invalid)
              | c <- cmc, Just vs <- [look (cmcName c)]
              , let values = [ (v, readCustomValue (cmcType c) v)
                             | v <- concatMap splitMeta vs ]
                    invalid = [ (cmcName c, bad)
                              | let bad = [ v | (v, Nothing) <- values ], not (null bad) ] ]
    unknown = M.toList $ M.filterWithKey (\k _ -> k `notElem` knownKeys cmc) fields

-- | Keys of the header that kiwi reads: built-in, and custom metadata
knownKeys :: [CustomMetaConfig] -> [T.Text]
knownKeys cmc = [ "id", "title", "tags", "access", "lang", "archive", "images-dir"
                , "files-dir" ] ++ map cmcName cmc


-- | Keys of a header that kiwi does not use: neither built-in nor declared
-- custom metadata
unknownMetaKeys :: [CustomMetaConfig] -> MetaData -> [T.Text]
unknownMetaKeys cmc = filter (`notElem` knownKeys cmc) . M.keys . metaOther

-- | Fields of a header whose value can not be read, with this value. For
-- "archive" and "lang" (built-in), the page has the default value instead;
-- for the custom metadata of type int, date or bool, the value is ignored.
invalidMetaValues :: [CustomMetaConfig] -> MetaData -> [(T.Text, T.Text)]
invalidMetaValues cmc m =
  [ (k, v) | k <- ["archive", "lang"] ++ map cmcName cmc
           , v <- fromMaybe [] (M.lookup k $ metaOther m) ]


-- | Fields of a header: key and values of its lines. Each line is a field
-- ("key: value", with a key in one word of letters, digits, '-' and '_', or
-- one of the given keys), a separator made of '-' or '.' ("---", "..."), or
-- blank. Anything else (a heading, some text) is an error: the first block
-- of a page must be its metadata.
parseHeaderFields :: [T.Text] -> T.Text -> Either String (M.Map T.Text [T.Text])
parseHeaderFields keys t =
  M.fromListWith (flip (++)) . concat <$>
  traverse parseLine (zip [1 :: Int ..] $ T.lines t)
  where
    parseLine (n, l)
      | T.all isSpace l = Right []
      | T.all (`elem` ['-', '.']) (T.strip l) = Right []
      | Just (k, v) <- splitOnFirst ":" l, validKey (T.strip k) =
          Right [(T.strip k, [T.strip v])]
      | otherwise = Left $ "invalid metadata header, line " ++ show n
    validKey k = k `elem` keys ||
                 (not (T.null k) && T.all (\ch -> isAlphaNum ch || ch `elem` ['-', '_']) k)
