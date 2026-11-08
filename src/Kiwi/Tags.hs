-- | Reading of tags and metadata lists: splitting on commas, groups between
-- braces, segments separated by ">"
module Kiwi.Tags
  ( tagSubSegments
  , tagToSegments
  , tagsListSubSegments
  , segmentsToTag
  , prettyTag
  , prefixNormalizeTag
  , filterRootTags
  , splitMeta
  , splitTags
  ) where

import           Data.Char (isSpace)
import qualified Data.List as L
import qualified Data.Set as S
import qualified Data.Text as T

import           Kiwi.Types (TagId, TagSegments)
import           Kiwi.Utils (nodup)


-- loisir > sport > escalade =>
--   [ loisir, loisir > sport, loisir > sport > escalade ]
tagSubSegments :: TagId -> [TagSegments]
tagSubSegments = drop 1 . L.inits . tagToSegments

tagToSegments :: TagId -> TagSegments
tagToSegments = splitTextEsc '>'

tagsListSubSegments :: [TagId] -> [TagSegments]
tagsListSubSegments = nodup . concatMap tagSubSegments

segmentsToTag :: TagSegments -> TagId
segmentsToTag = T.intercalate (T.pack ">") .
                map (T.replace ">" "\\>")

prettyTag :: TagId -> TagId
prettyTag = T.intercalate (T.pack " > ") . tagToSegments

-- normalizeTag :: TagId -> TagId
-- normalizeTag = segmentsToTag . tagToSegments

prefixNormalizeTag :: TagSegments -> TagId -> TagId
prefixNormalizeTag prefix = segmentsToTag . (prefix ++) . tagToSegments

filterRootTags :: [TagSegments] -> [TagSegments]
filterRootTags tss = let set = S.fromList tss in filter (isRoot set) tss
  where isRoot _ (_:[]) = True
        isRoot set ts = not $ S.member (init ts) set

splitMeta :: T.Text -> [TagId]
splitMeta = splitTextEsc ','

-- | Split a tags field on commas, expanding the groups between braces:
-- "a > {b, c > {d, e}}" gives "a > b", "a > c > d" and "a > c > e", and
-- "{a, b} > c" gives "a > c" and "b > c". An empty group gives no tag.
-- Commas and braces can be escaped with a backslash, other escapes are kept
-- for 'tagToSegments'. With unbalanced braces, the field is split as usual,
-- braces included.
splitTags :: T.Text -> [TagId]
splitTags t = case tagsP (T.unpack t) of
  Just (tags, []) -> map T.pack tags
  _               -> splitMeta t
  where
    -- tags until an unmatched closing brace or the end, blank ones dropped
    tagsP :: String -> Maybe ([String], String)
    tagsP s = do
      (alts, rest) <- tagP [""] s
      let tags = filter (not . null) $ map strip alts
      case rest of
        ',' : rest' -> do
          (others, rest'') <- tagsP rest'
          return (tags ++ others, rest'')
        _ -> return (tags, rest)

    -- a tag, as the alternatives built so far: each part (text or group)
    -- multiplies them, until a comma, an unmatched closing brace or the end
    tagP :: [String] -> String -> Maybe ([String], String)
    tagP alts s = case s of
      '{' : rs -> do
        (group, rest) <- tagsP rs
        case rest of
          '}' : rest' -> tagP [ a ++ g | a <- alts, g <- group ] rest'
          _           -> Nothing  -- unclosed group
      '\\' : c : rs | c `elem` [',', '{', '}'] -> tagP (append [c]) rs
                    | otherwise                -> tagP (append ['\\', c]) rs
      c : _ | c `elem` [',', '}'] -> Just (alts, s)
      c : rs -> tagP (append [c]) rs
      [] -> Just (alts, [])
      where append cs = map (++ cs) alts

    strip = L.dropWhileEnd isSpace . dropWhile isSpace

-- | Split on separator, unless it is escaped by a backslash. Other
-- backslashes are kept as is. Parts are stripped, empty parts are dropped.
splitTextEsc :: Char -> T.Text -> [T.Text]
splitTextEsc sep = filter (not . T.null) . map (T.strip . T.pack . reverse) .
                   go [] . T.unpack
  where
    -- acc is the current part, reversed
    go acc ('\\':c:rs) | c == sep = go (c:acc) rs
    go acc (c:rs)      | c == sep = acc : go [] rs
                       | otherwise = go (c:acc) rs
    go acc []          = [acc]
