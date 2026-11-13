-- | The metadata header of the pages: reading, combination ('mergeMeta',
-- 'clearMeta', 'keepOnlyMeta') and checks. The same 'MetaData' type holds
-- the header of a file (absent fields are 'Nothing'), the defaults of the
-- config and of the sources, and the metadata of a page: its header merged
-- over the defaults.
module Kiwi.Meta
  ( emptyMeta
  , mergeMeta
  , clearMeta
  , metaKeys
  , keepOnlyMeta
  , sourceMeta
  , adminKeys
  , parseAccess
  , splitPage
  , normalizeNewlines
  , parseHeader
  , writeHeader
  , writePage
  , unknownMetaKeys
  , invalidMetaValues
  , parseHeaderFields  -- exported for tests
  ) where

import           Control.Applicative ((<|>))
import           Control.Monad (mfilter)
import           Data.Char (isAlphaNum, isSpace)
import qualified Data.List as L
import qualified Data.Map.Strict as M
import           Data.Ord (Down (..))
import           Data.Maybe (fromMaybe, isJust, mapMaybe)
import qualified Data.Text as T

import           Kiwi.Tags (splitMeta, splitTags, tagToSegments)
import           Kiwi.Types
import           Kiwi.Utils (splitOnFirst, parseLangSetting, parseBool, readCustomValue,
                             showCustomValue, showLangSetting, nodup)


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

-- | Keys of the fields of the metadata, valid or not
metaKeys :: MetaData -> [T.Text]
metaKeys m = nodup $
  [ k | (k, True) <- [ ("id", isJust $ metaId m), ("title", isJust $ metaTitle m)
                     , ("tags", isJust $ metaTags m), ("access", isJust $ metaAccess m)
                     , ("lang", isJust $ metaLang m), ("archive", isJust $ metaArchive m)
                     , ("images-dir", isJust $ metaImagesDir m)
                     , ("files-dir", isJust $ metaFilesDir m) ] ]
  ++ map cmdName (metaCustom m) ++ M.keys (metaOther m)

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


-- | Keys of the header that only the users with the admin right on a page
-- can change: those with the write right can not.
adminKeys :: [T.Text]
adminKeys = ["access", "id", "archive", "images-dir", "files-dir"]

-- | Groups of an access list ("family:w, friends, me:a"), with their right,
-- and the items that can not be read. The right of a group is read (r, the
-- default), write (w, or rw) or admin (a); "public" can only read. A group
-- given several times gets its highest right. Groups are sorted by name.
parseAccess :: T.Text -> (Access, [T.Text])
parseAccess t =
  let items = [ (i, parseItem i) | i <- splitMeta t ]
  in ( M.toList $ M.fromListWith max [ x | (_, Just x) <- items ]
     , [ i | (i, Nothing) <- items ] )
  where
    parseItem i = do
      let (g, l) = fromMaybe (i, "r") $ splitOnFirst ":" i
          group = T.strip g
      level <- case T.toLower (T.strip l) of
        "r"  -> Just AccessRead
        "w"  -> Just AccessWrite
        "rw" -> Just AccessWrite
        "a"  -> Just AccessAdmin
        _    -> Nothing
      if T.null group || (group == "public" && level > AccessRead)
        then Nothing else Just (group, level)

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
           , metaAccess = fst <$> access
           , metaLang = fst lang
           , metaArchive = fst archive
           , metaImagesDir = T.concat <$> look "images-dir"
           , metaFilesDir = T.concat <$> look "files-dir"
           , metaCustom = map fst customs
           , metaOther = M.fromList $ unknown ++ snd lang ++ snd archive ++
                                      [ ("access", bad) | Just (_, bad@(_:_)) <- [access] ] ++
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
    access = parseAccess . T.intercalate "," <$> look "access"
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
-- an invalid group of "access" is ignored (see 'parseAccess'); for the
-- custom metadata of type int, date or bool, the value is ignored.
invalidMetaValues :: [CustomMetaConfig] -> MetaData -> [(T.Text, T.Text)]
invalidMetaValues cmc m =
  [ (k, v) | k <- ["archive", "lang", "access"] ++ map cmcName cmc
           , v <- fromMaybe [] (M.lookup k $ metaOther m) ]


-- | Text of a page: its header (see 'writeHeader'), a blank line, and its
-- content, which ends with a newline. A page without metadata starts with a
-- blank line.
writePage :: [CustomMetaConfig] -> MetaData -> T.Text -> T.Text
writePage cmc m content =
  let header = writeHeader cmc m
      body = T.dropWhileEnd (== '\n') content
  in (if T.null header then "\n" else header) <> "\n" <>
     (if T.null body then "" else body <> "\n")

-- | Normalized text of a header, read back by 'parseHeader': one line by
-- key, in this order: title, id, tags, access, lang, archive, images-dir,
-- files-dir, the custom metadata in the order of the config, then the other
-- keys by name (one line by value). The tags are sorted, without
-- duplicates, their segments separated by " > ". The values that can not be read are written as they were.
writeHeader :: [CustomMetaConfig] -> MetaData -> T.Text
writeHeader cmc m = T.unlines $ concat
  [ line "title" (metaTitle m)
  , line "id" (metaId m)
  , line "tags" (T.intercalate ", " . map writeTag . nodup . map writeTag' <$>
                 metaTags m)
  , let invalid = other "access"
    in case metaAccess m of
         Nothing | null invalid -> []
         access -> [field "access" $ T.intercalate ", " $
                      map writeAccess (L.sortOn (\(g, l) -> (Down l, g)) $ fromMaybe [] access)
                      ++ invalid]
  , orOther "lang" (showLangSetting <$> metaLang m)
  , orOther "archive" ((\b -> if b then "yes" else "no") <$> metaArchive m)
  , line "images-dir" (metaImagesDir m)
  , line "files-dir" (metaFilesDir m)
  , concatMap custom cmc
  , [ field k v | (k, vs) <- M.toList (metaOther m), k `notElem` knownKeys cmc, v <- vs ]
  ]
  where
    field k v = if T.null v then k <> ":" else k <> ": " <> v
    line k = maybe [] (\v -> [field k v])
    other k = M.findWithDefault [] k (metaOther m)
    -- a value that can be read, or the one that could not
    orOther k v = case v of
      Just _  -> line k v
      Nothing -> map (field k) (other k)
    custom c =
      let n = cmcName c
          values = [ escape "," (showCustomValue v)
                   | cmd <- metaCustom m, cmdName cmd == n, v <- cmdKeys cmd ]
          invalid = map (escape ",") (other n)
      in if any ((== n) . cmdName) (metaCustom m) || not (null invalid)
         then [field n (T.intercalate ", " (values ++ invalid))]
         else []
    writeAccess (g, l) = g <> case l of
      AccessRead  -> ""
      AccessWrite -> ":w"
      AccessAdmin -> ":a"
    -- a tag, by its segments: "a > b"
    writeTag' = T.intercalate " > " . map (escape ">") . tagToSegments
    writeTag = escape ",{}"
    escape cs = T.concatMap (\ch -> if ch `elem` T.unpack cs then T.pack ['\\', ch]
                                    else T.singleton ch)


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
