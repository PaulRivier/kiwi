-- | Texts of the answers of the API (see "Kiwi.Api"): compact, made to be
-- read by an agent (a language model) as well as by a person. One record by
-- line, the page first as SOURCE/UID, the fields always in the same order;
-- a first line tells what was found and what was cut.
module Kiwi.Api.Text
  ( pageRef
  , pagesText
  , vocabularyText
  , Section (..)
  , sections
  , outlineText
  , numberedLines
  , linesRange
  , foldersText
  , tagText
  , dayText
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
import           Kiwi.Utils (inlinesText)


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

-- | List of pages, one by line (SOURCE/UID, day of modification, title,
-- tags), with their summary and, when the paths are given (admins), the path
-- of their file on the next lines. The first line gives the number of pages
-- found and of pages shown.
pagesText :: Maybe FP.FilePath -> Int -> [Page] -> T.Text
pagesText pathsM total pages = T.unlines $ header : concatMap pageLines pages
  where
    shown = length pages
    header | total == shown = count total
           | otherwise = T.concat [count total, ", ", T.pack (show shown), " shown (limit)"]
    count n = T.concat [T.pack (show n), if n == 1 then " page" else " pages"
                       , ", the most recently modified first"]
    pageLines p =
      T.intercalate "  " [ pageRef p, dayText (pageMTime p), pageTitle p
                         , "[" <> T.intercalate ", " (map tagText $ pageTags p) <> "]" ]
      : [ "  summary: " <> s | Just s <- [pageSummary p] ]
      ++ [ "  path: " <> sourcePath cd p | Just cd <- [pathsM] ]

-- | Vocabulary of a source: its tags (each one with its parents, as written
-- in the headers, and its number of pages), its langs, and its custom
-- fields with their values, each with its number of pages
vocabularyText :: T.Text -> Int -> [(TagId, Int)] -> [(T.Text, Int)] ->
                  [(T.Text, T.Text, [(T.Text, Int)])] -> T.Text
vocabularyText source total tags langs customs = T.unlines $
  [ T.concat ["source ", source, ": ", T.pack (show total), " pages"]
  , "tags (pages):" ]
  ++ (if null tags then ["  (none)"]
      else [ T.concat ["  ", tagText t, " (", T.pack (show n), ")"] | (t, n) <- tags ])
  ++ [ "lang: " <> counts langs ]
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
-- line in the content, see 'lineKey'), the number of lines before its content
-- in the text, and the number of lines of the text
sections :: PD.Pandoc -> Int -> Int -> [Section]
sections (PD.Pandoc _ blocks) offset total =
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
        Just (lvl, inlinesText ils, line + offset)
      _ -> Nothing

-- | Outline of a page: its reference and title, tags, summary, path (for
-- admins), hash of the file, number of lines, the pages linking it, then
-- its sections, with their lines
outlineText :: Maybe FP.FilePath -> Page -> T.Text -> Int -> [Page] -> [Section] -> T.Text
outlineText pathsM p hash total backlinks secs = T.unlines $
  [ T.intercalate "  " [pageRef p, pageTitle p]
  , "tags: " <> T.intercalate ", " (map tagText $ pageTags p) ]
  ++ [ "summary: " <> s | Just s <- [pageSummary p] ]
  ++ [ "path: " <> sourcePath cd p | Just cd <- [pathsM] ]
  ++ [ "hash: " <> hash
     , T.concat ["lines: ", T.pack (show total)]
     , "backlinks: " <> if null backlinks then "(none)"
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

-- | Folders of a source (paths from its top, "." for the top), sorted, each
-- one with the number of its pages (sub-folders not included), from the
-- paths of its pages: a folder without page, at any depth, is not listed
foldersText :: T.Text -> [FP.FilePath] -> T.Text
foldersText source paths = T.unlines $
  ("source " <> source <> ": folders (pages)") :
  [ T.concat ["  ", T.pack d, " (", T.pack (show n), ")"] | (d, n) <- M.toList counts ]
  where
    counts = M.fromListWith (+) $
               [ (FP.takeDirectory p, 1 :: Int) | p <- paths ] ++
               [ (d, 0) | p <- paths, d <- parents (FP.takeDirectory p) ]
    -- the parent folders of a folder, which may have no page of their own
    parents d = case FP.splitDirectories d of
      ["."] -> []
      segs  -> "." : [ FP.joinPath (take n segs) | n <- [1 .. length segs - 1] ]
