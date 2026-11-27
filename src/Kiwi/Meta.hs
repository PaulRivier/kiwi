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
  , kiwiKeys
  , isValidUid
  , uidFromBytes
  , parseAccess
  , splitPage
  , parseHeader
  , writeHeader
  , writePage
  , unknownMetaKeys
  , invalidMetaValues
  , parseHeaderFields  -- exported for tests
  ) where

import           Control.Applicative ((<|>))
import           Control.Monad (mfilter)
import           Data.Bits ((.&.))
import qualified Data.ByteString as BS
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
emptyMeta = MetaData { metaUid = Nothing
                     , metaPreviousUids = Nothing
                     , metaTitle = Nothing
                     , metaSummary = Nothing
                     , metaTags = Nothing
                     , metaAccess = Nothing
                     , metaLang = Nothing
                     , metaArchive = Nothing
                     , metaCustom = []
                     , metaOther = M.empty
                     }

-- | The fields of the second metadata, and those of the first one that the
-- second does not have (custom and other fields: by name)
mergeMeta :: MetaData -> MetaData -> MetaData
mergeMeta a b =
  MetaData { metaUid = metaUid b <|> metaUid a
           , metaPreviousUids = metaPreviousUids b <|> metaPreviousUids a
           , metaTitle = metaTitle b <|> metaTitle a
           , metaSummary = metaSummary b <|> metaSummary a
           , metaTags = metaTags b <|> metaTags a
           , metaAccess = metaAccess b <|> metaAccess a
           , metaLang = metaLang b <|> metaLang a
           , metaArchive = metaArchive b <|> metaArchive a
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
  [ k | (k, True) <- [ ("uid", isJust $ metaUid m)
                     , ("previous-uids", isJust $ metaPreviousUids m)
                     , ("title", isJust $ metaTitle m), ("summary", isJust $ metaSummary m)
                     , ("tags", isJust $ metaTags m), ("access", isJust $ metaAccess m)
                     , ("lang", isJust $ metaLang m), ("archive", isJust $ metaArchive m) ] ]
  ++ map cmdName (metaCustom m) ++ M.keys (metaOther m)

-- | The metadata with the fields of these keys only (see 'clearMeta')
keepOnlyMeta :: [T.Text] -> MetaData -> MetaData
keepOnlyMeta keys = filterMeta (`elem` keys)

filterMeta :: (T.Text -> Bool) -> MetaData -> MetaData
filterMeta keep m =
  MetaData { metaUid = field "uid" metaUid
           , metaPreviousUids = field "previous-uids" metaPreviousUids
           , metaTitle = field "title" metaTitle
           , metaSummary = field "summary" metaSummary
           , metaTags = field "tags" metaTags
           , metaAccess = field "access" metaAccess
           , metaLang = field "lang" metaLang
           , metaArchive = field "archive" metaArchive
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
-- can see and change in an editor: those with the write right can not.
adminKeys :: [T.Text]
adminKeys = ["access"]

-- | Keys of the header that kiwi keeps: no editor can see nor change them,
-- whatever their right
kiwiKeys :: [T.Text]
kiwiKeys = ["uid", "previous-uids"]

-- | Whether a uid can be used: from 1 to 64 letters, digits, '-' or '_'
isValidUid :: T.Text -> Bool
isValidUid u = not (T.null u) && T.length u <= 64 &&
               T.all (\ch -> isAlphaNum ch || ch == '-' || ch == '_') u

-- | A uid made of random bytes: one character of the Crockford base32
-- alphabet by byte (its 5 low bits, so that each character is equally
-- likely). Kiwi draws 10 bytes, so 50 bits.
uidFromBytes :: BS.ByteString -> T.Text
uidFromBytes = T.pack . map (\b -> T.index crockford (fromIntegral (b .&. 31))) . BS.unpack
  where crockford = "0123456789ABCDEFGHJKMNPQRSTVWXYZ"

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

-- | Header and content of a page: the header is the first block of the
-- file, until its first blank line, and the content is what follows. A file
-- without blank line is a header only, with an empty content. Fails when the
-- header is invalid (see 'parseHeaderFields'). Lines end with LF only.
splitPage :: [CustomMetaConfig] -> T.Text -> Either String (MetaData, T.Text)
splitPage cmc c =
  let (metaC, content) = fromMaybe (c, T.empty) $ splitOnFirst "\n\n" c
  in (, content) <$> parseHeader cmc metaC


-- | Metadata of a header (see 'parseHeaderFields'). The values that can not
-- be read are kept in 'metaOther', with the unknown keys.
parseHeader :: [CustomMetaConfig] -> T.Text -> Either String MetaData
parseHeader cmc t = fromFields cmc <$> parseHeaderFields (map cmcName cmc) t

-- | Metadata of the fields of a header (key: values of its lines). Repeated
-- fields are joined, except "lang" and "archive", whose last line is used.
fromFields :: [CustomMetaConfig] -> M.Map T.Text [T.Text] -> MetaData
fromFields cmc fields =
  MetaData { -- an empty uid is ignored: the page gets a new one
             metaUid = mfilter (not . T.null) $ T.concat <$> look "uid"
           , metaPreviousUids = splitMeta . T.intercalate "," <$> look "previous-uids"
           , metaTitle = T.intercalate " " <$> look "title"
           , metaSummary = T.intercalate " " <$> look "summary"
           , metaTags = splitTags . T.intercalate "," <$> look "tags"
           , metaAccess = fst <$> access
           , metaLang = fst lang
           , metaArchive = fst archive
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
knownKeys cmc = [ "uid", "previous-uids", "title", "summary", "tags", "access", "lang"
                , "archive" ]
                ++ map cmcName cmc


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
-- key, in this order: title, summary, uid, previous-uids, tags, access, lang, archive,
-- the custom metadata in the order of the config, then the other
-- keys by name (one line by value). The tags are grouped by their common
-- prefixes (see 'writeTags'). The values that can not be read are written as
-- they were.
writeHeader :: [CustomMetaConfig] -> MetaData -> T.Text
writeHeader cmc m = T.unlines $ concat
  [ line "title" (metaTitle m)
  , line "summary" (metaSummary m)
  , line "uid" (metaUid m)
  , line "previous-uids" (T.intercalate ", " . map (escape ",") <$> metaPreviousUids m)
  , line "tags" (writeTags <$> metaTags m)
  , let invalid = other "access"
    in case metaAccess m of
         Nothing | null invalid -> []
         access -> [field "access" $ T.intercalate ", " $
                      map writeAccess (L.sortOn (\(g, l) -> (Down l, g)) $ fromMaybe [] access)
                      ++ invalid]
  , orOther "lang" (showLangSetting <$> metaLang m)
  , orOther "archive" ((\b -> if b then "yes" else "no") <$> metaArchive m)
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
    escape cs = T.concatMap (\ch -> if ch `elem` T.unpack cs then T.pack ['\\', ch]
                                    else T.singleton ch)


-- | Text of a tags field, read back by 'splitTags': the tags without
-- duplicates, sorted by segments, those with the same first segment grouped
-- between braces, at each level: "a > b > c, a > b > d, e" gives
-- "a > b > {c, d}, e". A tag that is the prefix of others is written before
-- them ("a, a > {b, c}"): the syntax has no empty alternative.
writeTags :: [TagId] -> T.Text
writeTags = T.intercalate ", " . go . nodup . filter (not . null) . map tagToSegments
  where
    -- sorted, distinct, non-empty segments
    go :: [TagSegments] -> [T.Text]
    go = concatMap group . L.groupBy (\a b -> take 1 a == take 1 b)
    group g = case g of
      (s : _) : _ ->
        let self = [ escape s | [_] <- g ]
            rests = [ r | _ : r@(_ : _) <- g ]
        in self ++ case go rests of
                     []  -> []
                     [r] -> [escape s <> " > " <> r]
                     rs  -> [escape s <> " > {" <> T.intercalate ", " rs <> "}"]
      _ -> []
    escape = T.concatMap (\ch -> if ch `elem` [',', '>', '{', '}'] then T.pack ['\\', ch]
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
