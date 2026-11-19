-- | Full-text search of the API, apart from the one of the web pages (see
-- 'Kiwi.Search.searchPages'): same index, same words and stems, but other
-- rules, made for an agent. The pages that contain one of the terms, in the
-- order of the score of the engine (the more terms, the better); each page
-- tells which terms it contains,
-- and where (title, tags, content), and gives a few passages of its text with
-- their line and section. A query can hold exact phrases ("first aid") and
-- excluded words (-baby).
module Kiwi.Api.Search
  ( Query (..)
  , parseQuery
  , Hit (..)
  , hasPhrase
  , Passage (..)
  , passages
  , searchText
  ) where

import           Data.Char (isSpace)
import           Data.List (isInfixOf, nub, sortOn)
import           Data.Maybe (fromMaybe)
import           Data.Ord (Down (..))
import qualified Data.Set as S
import qualified Data.Text as T

import qualified Kiwi.Api.Text as AT
import           Kiwi.Search (alg, wordsTerms)
import           Kiwi.Types


-- | A query: its words (those of its phrases included), its exact phrases
-- (between double quotes) and its excluded words (after a "-")
data Query = Query { queryWords :: [T.Text]
                   , queryPhrases :: [T.Text]
                   , queryExcluded :: [T.Text] }
  deriving (Eq, Show)

-- | Query of a text: a quote opens a phrase until the next quote (or the
-- end), a word starting with "-" is excluded
parseQuery :: T.Text -> Query
parseQuery = go (Query [] [] [])
  where
    go q t
      | T.null t' = q { queryWords = reverse (queryWords q)
                      , queryPhrases = reverse (queryPhrases q)
                      , queryExcluded = reverse (queryExcluded q) }
      | Just rest <- T.stripPrefix "\"" t' =
          let (phrase, after) = T.breakOn "\"" rest
              ws = T.words phrase
          in go q { queryWords = reverse ws ++ queryWords q
                  , queryPhrases = [ T.unwords ws | not (null ws) ] ++ queryPhrases q }
                (T.drop 1 after)
      | otherwise =
          let (w, after) = T.break (\c -> isSpace c || c == '"') t'
          in case T.stripPrefix "-" w of
               Just ex | not (T.null ex) -> go q { queryExcluded = ex : queryExcluded q } after
               _ -> go q { queryWords = w : queryWords q } after
      where t' = T.stripStart t

-- | A page found: its score, the terms of the query it contains, and the
-- fields that contain them
data Hit = Hit { hitPage :: Page
               , hitScore :: Float
               , hitTerms :: [T.Text]
               , hitFields :: [PageField] }

-- Unused since the API search gives the union of the terms, as /search
-- -- | The pages to show: those with all the terms of the query (True) when
-- -- there are some, else those with some of them (False), best first, with
-- -- the number of the other pages found
-- data Ranked = Ranked { rankedHits :: [Hit]
--                      , rankedComplete :: Bool
--                      , rankedOthers :: Int }
--
-- -- | Pages found for these terms (see 'Ranked')
-- rankHits :: [T.Text] -> [Hit] -> Ranked
-- rankHits terms hits =
--   let n = length (nub terms)
--       (complete, partial) = (filter full hits, filter (not . full) hits)
--       full h = length (hitTerms h) >= n
--       byTerms = sortOn (\h -> (Down (length $ hitTerms h), Down (hitScore h)))
--   in if null complete
--      then Ranked (byTerms partial) False 0
--      else Ranked (byTerms complete) True (length partial)

-- | Whether the title or the content of a page has the phrase: its terms
-- follow one another
hasPhrase :: Page -> T.Text -> Bool
hasPhrase p phrase =
  let terms t = concat $ wordsTerms (alg $ pageLang p) (T.words t)
      ts = terms phrase
  in not (null ts) && any ((ts `isInfixOf`) . terms) [pageTitle p, pagePlainText p]


-- | A line of a page that contains terms of the query: its number in the
-- text of the page, the title of its section (if any), and the words around
-- the first term
data Passage = Passage { passageLine :: Int
                       , passageSection :: Maybe T.Text
                       , passageText :: T.Text }
  deriving (Eq, Show)

-- | At most 'maxPassages' lines of the content of a page that contain some of
-- the terms (by stem, in the lang of the page), given its content (lines
-- counted from its first line) and its sections. A heading is given whole,
-- without section.
passages :: Lang -> [T.Text] -> T.Text -> [AT.Section] -> [Passage]
passages lang terms content secs =
  take maxPassages
    [ if heading then Passage n Nothing (T.strip l) else Passage n (sectionOf n) (window ws i)
    | (n, l) <- zip [1 ..] (T.lines content)
    , let heading = n `elem` map AT.sectionFrom secs
    , let ws = T.words l
    , i : _ <- [ [ j | (j, wts) <- zip [0 ..] (wordsTerms (alg lang) ws)
                     , any (`S.member` termSet) wts ] ] ]
  where
    termSet = S.fromList terms
    -- the innermost section of the line, the one starting last before it
    sectionOf n = case sortOn (Down . AT.sectionFrom)
                         [ s | s <- secs, AT.sectionFrom s <= n, n <= AT.sectionTo s ] of
      s : _ -> Just (AT.sectionTitle s)
      []    -> Nothing
    window ws i =
      let start = max 0 (i - passageBefore)
          shown = take (passageBefore + passageAfter) (drop start ws)
      in T.unwords $ [ "…" | start > 0 ] ++ shown
                     ++ [ "…" | start + length shown < length ws ]

-- | Passages of a page, and words of a passage before and after its first
-- term
maxPassages, passageBefore, passageAfter :: Int
maxPassages = 3
passageBefore = 8
passageAfter = 24

-- | Text of the results: a first line with the number of pages found, the
-- query and its lang, then each page (see 'AT.pagesText'), the terms it
-- contains (shown by the words of the query that give them) and where, its
-- summary and its passages
searchText :: T.Text -> Lang -> [(T.Text, T.Text)] -> Int -> [(Hit, [Passage])] -> T.Text
searchText query lang terms total results = T.unlines $ header : concatMap hitLines results
  where
    n = length (nub $ map fst terms)
    wordOf t = fromMaybe t (lookup t terms)
    shown = length results
    header = T.concat $
      [ T.pack (show total), if total == 1 then " page" else " pages", " for ", query
      , " (", T.toLower (T.pack $ show lang), ")" ]
      ++ [ ", best first" | total > 1 ]
      ++ [ T.concat [", ", T.pack (show shown), " shown (limit)"] | shown < total ]
    hitLines (h, ps) =
      let p = hitPage h
      in T.intercalate "  "
           [ AT.pageRef p, AT.dayText (pageMTime p), pageTitle p
           , "[" <> T.intercalate ", " (map AT.tagText $ pageTags p) <> "]"
           , T.concat [ T.pack (show $ length $ hitTerms h), "/", T.pack (show n)
                      , if length (hitTerms h) < n
                        then " (" <> T.intercalate ", " (map wordOf $ hitTerms h) <> ")" else ""
                      , " in ", T.intercalate "+" (map fieldName $ hitFields h) ] ]
         : [ "  summary: " <> s | Just s <- [pageSummary p] ]
         ++ [ T.concat [ "  l.", T.pack (show $ passageLine x)
                       , maybe "" (\s -> " § " <> s) (passageSection x), ": ", passageText x ]
            | x <- ps ]
    fieldName f = case f of
      TitleField   -> "title"
      TagsField    -> "tags"
      ContentField -> "content"
