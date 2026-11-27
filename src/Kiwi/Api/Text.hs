-- | Texts of the answers of the API (see "Kiwi.Api"): compact, made to be
-- read by an agent (a language model) as well as by a person. One record by
-- line, the page first as SOURCE/UID, the fields always in the same order;
-- a first line tells what was found and what was cut.
module Kiwi.Api.Text
  ( pageRef
  , pageFields
  , pagesText
  , facetsText
  , Section (..)
  , sections
  , outlineText
  , numberedLines
  , linesRange
  , folderCounts
  , tagText
  , dayText
  , sizeText
  ) where

import qualified Data.Map.Strict as M
import           Data.Maybe (mapMaybe)
import qualified Data.Text as T
import qualified Data.Time as Time
import qualified System.FilePath as FP
import qualified Text.Pandoc.Definition as PD
import           Text.Read (readMaybe)

import           Kiwi.HtmlWriter (lineKey)
import qualified Kiwi.Tags as Tags
import           Kiwi.Types
import           Kiwi.Utils (inlinesText, showCustomValue)


-- | A page in the answers: SOURCE/UID
pageRef :: Page -> T.Text
pageRef p = let (s, u) = pageUID p in s <> "/" <> u

-- | A tag as written in the headers: "a > b"
tagText :: TagId -> T.Text
tagText = Tags.prettyTag

-- | Day of a time: YYYY-MM-DD
dayText :: Time.UTCTime -> T.Text
dayText = T.pack . show . Time.utctDay

-- | Path of the file of a page from the top of its source
sourcePath :: FP.FilePath -> Page -> T.Text
sourcePath contentDir' p =
  T.pack $ FP.makeRelative (contentDir' FP.</> T.unpack (fst $ pageUID p)) (pageAbsoluteFSPath p)

-- | The size of a content, value of the field content_size: "chars=N
-- lines=M", its characters and lines
sizeText :: Int -> Int -> T.Text
sizeText chars nLines = T.concat [ "chars=", T.pack (show chars), " lines=", T.pack (show nLines) ]

-- | The fields of a page that the lists can show, besides its custom
-- fields (see 'pagesText')
pageFields :: [T.Text]
pageFields = [ "title", "tags", "summary", "keywords", "lang", "modified", "content_size"
             , "links", "backlinks", "related" ]

-- | List of pages, one by line (SOURCE/UID), each one followed by the given
-- fields that it has, one by line, written as in a header ("  tags: a > b,
-- c"; "modified": the day of the last change; "content_size": the
-- characters and lines of its content, without the header (see
-- 'sizeText'); "links", "backlinks", "keywords" and "related", given by the function, see
-- 'Kiwi.Api.extraField'; a custom field by its name), and a blank line
-- when fields are asked.
-- The first line gives the number of pages found and of pages shown, and
-- how to see the others when some are cut.
pagesText :: (Page -> T.Text -> Maybe T.Text) -> [T.Text] -> Int -> [Page] -> T.Text
pagesText linkField fields total pages = T.unlines $ header : concatMap pageLines pages
  where
    shown = length pages
    header | total == shown = count total
           | otherwise = T.concat [ count total, ", ", T.pack (show shown)
                                  , " shown (limit): narrow with tags, fields or since, or raise limit" ]
    count n = T.concat [T.pack (show n), if n == 1 then " page" else " pages"
                       , ", the most recently modified first"]
    pageLines p = pageRef p : [ T.concat ["  ", f, ": ", v] | f <- fields, Just v <- [field p f] ]
                  ++ [ "" | not (null fields) ]
    field p f = case f of
      "title" -> Just (pageTitle p)
      "tags" -> list (map tagText $ pageTags p)
      "summary" -> pageSummary p
      "lang" -> Just (showCustomValue $ KeyLang $ pageLang p)
      "modified" -> Just (dayText $ pageMTime p)
      "content_size" -> Just (sizeText (pageContentSize p) (pageContentLines p))
      "links" -> linkField p f
      "backlinks" -> linkField p f
      "keywords" -> linkField p f
      "related" -> linkField p f
      _ -> list [ showCustomValue k | CustomMetaData f' ks <- pageCustomMeta p, f' == f, k <- ks ]
    list vs = if null vs then Nothing else Just (T.intercalate ", " vs)

-- | Facets of some pages of a source: their tags (each one with its parents,
-- as written in the headers, and its number of pages), their langs, their
-- folders (for the admins), and their custom fields (with their type and
-- description) with their values, each with its number of pages; the tags,
-- langs and folders only when given
facetsText :: T.Text -> Int -> Maybe [(TagId, Int)] -> Maybe [(T.Text, Int)] ->
              Maybe [(T.Text, Int)] -> [(T.Text, T.Text, [(T.Text, Int)])] -> T.Text
facetsText source total tagsM langsM foldersM customs = T.unlines $
  [ T.concat ["source ", source, ": ", T.pack (show total), if total == 1 then " page" else " pages"] ]
  ++ (case tagsM of
        Nothing -> []
        Just [] -> ["tags (pages):", "  (none)"]
        Just tags -> "tags (pages):" : [ T.concat ["  ", tagText t, " (", T.pack (show n), ")"] | (t, n) <- tags ])
  ++ [ "lang: " <> counts langs | Just langs <- [langsM] ]
  ++ [ "folder (pages): " <> counts folders | Just folders <- [foldersM] ]
  ++ [ T.concat [name, " (", typ, "): ", counts values] | (name, typ, values) <- customs ]
  where
    counts vs = T.intercalate ", " [ T.concat [v, " (", T.pack (show n), ")"] | (v, n) <- vs ]


-- | A section of a page: its heading (level and text) and its lines in the
-- text of the page, from the heading to the line before the next heading of
-- the same or a higher level
data Section = Section { sectionLevel :: Int
                       , sectionTitle :: T.Text
                       , sectionFrom :: Int
                       , sectionTo :: Int }
  deriving (Eq, Show)

-- | Sections of a page, from its document (its top-level headings have their
-- line in the content, see 'lineKey') and the number of lines of its
-- content; the lines are those of the content, without the header
sections :: PD.Pandoc -> Int -> [Section]
sections (PD.Pandoc _ blocks) total =
  let headings = mapMaybe heading blocks
      next (lvl, _, _) rest =
        case [ l | (lvl', _, l) <- rest, lvl' <= lvl ] of
          l : _ -> l - 1
          []    -> total
      go [] = []
      go (h@(lvl, title, from) : rest) = Section lvl title from (next h rest) : go rest
  in go headings
  where
    heading b = case b of
      PD.Header lvl (_, _, kvs) ils -> do
        line <- lookup lineKey kvs >>= readMaybe . T.unpack
        Just (lvl, inlinesText ils, line)
      _ -> Nothing

-- Unused since the links are fields of the lists (show=links, backlinks)
-- -- | Links of pages of a source: (uid of the page, uid of the page it links,
-- -- whether this one exists for the user), one by line, "(missing)" after a
-- -- link to a page that does not exist or that the user can not read
-- linksText :: T.Text -> [(T.Text, T.Text, Bool)] -> T.Text
-- linksText source links = T.unlines $
--   T.concat [ "source ", source, ": ", T.pack (show $ length links)
--            , if length links == 1 then " link" else " links" ]
--   : [ T.concat [from, " -> ", to, if exists then "" else " (missing)"] | (from, to, exists) <- links ]

-- | Outline of a page: its reference and title, tags, summary, path (for
-- admins), characters and lines of its content, the pages linking it, then its
-- sections, with their lines
-- (lines of the content, without the header). The hash of the file is not
-- in it (the ETag gives it).
outlineText :: Maybe FP.FilePath -> Page -> [Page] -> [Section] -> T.Text
outlineText pathsM p backlinks secs = T.unlines $
  [ T.intercalate "  " [pageRef p, pageTitle p]
  , "tags: " <> T.intercalate ", " (map tagText $ pageTags p) ]
  ++ [ "summary: " <> s | Just s <- [pageSummary p] ]
  ++ [ "path: " <> sourcePath cd p | Just cd <- [pathsM] ]
  ++ [ "content_size: " <> sizeText (pageContentSize p) (pageContentLines p) ]
  ++ [ "backlinks: " <> if null backlinks then "(none)"
                        else T.intercalate "; " [ pageRef b <> " " <> pageTitle b | b <- backlinks ]
     , if null secs then "sections: (none)" else "sections (lines):" ]
  ++ [ T.concat [ "  ", T.pack (show $ sectionFrom s), "-", T.pack (show $ sectionTo s), "  "
                , T.replicate (sectionLevel s) "#", " ", sectionTitle s ]
     | s <- secs ]

-- | Lines of a text, from the first given one, each one after its number and
-- a tab (as "cat -n")
numberedLines :: Int -> [T.Text] -> T.Text
numberedLines from ls = T.unlines
  [ T.justifyRight 6 ' ' (T.pack $ show n) <> "\t" <> l | (n, l) <- zip [from ..] ls ]

-- | The lines of a text from one line to another one (from 1, included),
-- with the first one; the range is bounded by the text
linesRange :: Int -> Int -> T.Text -> (Int, [T.Text])
linesRange from to t =
  let from' = max 1 from
  in (from', take (to - from' + 1) $ drop (from' - 1) $ T.lines t)

-- | Folders of pages (paths from the top of their source, "." for the
-- top), sorted, each one with the number of its pages (sub-folders not
-- included), from the paths of the pages: the parent folders of a folder
-- are given too, with 0 when they have no page of their own
folderCounts :: [FP.FilePath] -> [(T.Text, Int)]
folderCounts paths = [ (T.pack d, n) | (d, n) <- M.toList counts ]
  where
    counts = M.fromListWith (+) $
               [ (FP.takeDirectory p, 1 :: Int) | p <- paths ] ++
               [ (d, 0) | p <- paths, d <- parents (FP.takeDirectory p) ]
    -- the parent folders of a folder, which may have no page of their own
    parents d = case FP.splitDirectories d of
      ["."] -> []
      segs  -> "." : [ FP.joinPath (take n segs) | n <- [1 .. length segs - 1] ]
