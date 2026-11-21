-- | Keywords of the pages, for the agents: the terms that tell a page from
-- the others (TF-IDF), computed from the statistics of the search engine
-- (see 'Data.SearchEngine.documentTermStats'), so that nobody has to write
-- them. Design in @CLAUDE/projects/mots-cles-pages-2026-11-21.md@.
module Kiwi.Keywords
  ( pagesKeywords
  , keywordsText
  , bestTerms        -- exported for tests
  , maxKeywords      -- exported for tests
  ) where

import           Control.Applicative ((<|>))
import           Data.List (sortOn)
import qualified Data.Map.Strict as M
import           Data.Maybe (fromMaybe)
import           Data.Ord (Down (..))
import qualified Data.Set as S
import qualified Data.Text as T

import           Data.SearchEngine (TermStats (..), documentCount, documentTermStats)
import           Kiwi.Search (alg, extractTerms, writtenTerms)
import           Kiwi.Types

-- | At most this number of keywords by page
maxKeywords :: Int
maxKeywords = 10

-- | A term is a keyword when its score is at least this part of the best
-- score of the page: a short page has fewer keywords, rather than common
-- words
relativeThreshold :: Double
relativeThreshold = 0.4

-- | Keywords of the pages, the best first, for each page of the search
-- engine among the given ones (the pages of the index). The terms of a page
-- come from the search engine; their words, from the text of the page. The
-- words of the given keywords (those of the previous load, for the pages
-- that were not loaded again) are kept, so that only the new keywords of a
-- page need its text. The keywords are evaluated.
pagesKeywords :: PageSearchEngine -> M.Map PageUID [Keyword] -> [Page] -> M.Map PageUID [Keyword]
pagesKeywords se previous pages =
  M.fromList [ (pageUID p, forced (keywords p)) | p <- pages ]
  where
    n = documentCount se
    forced ks = foldr seq () ks `seq` ks
    keywords p =
      let best = bestTerms n (headingTerms p) $ fromMaybe [] $ documentTermStats se (pageUID p)
          known = M.fromList [ (kwStem k, kwWord k) | k <- M.findWithDefault [] (pageUID p) previous ]
          missing = S.fromList (filter (`M.notMember` known) best)
          found = if S.null missing then M.empty else pageWords p missing
      -- a term whose word is not found (a stem that the words of the text
      -- do not give back) is left out
      in [ Keyword t w | t <- best, Just w <- [M.lookup t known <|> M.lookup t found] ]

-- | Terms of the headings of a page, with their number of occurrences (all
-- the headings at once: one stemmer by call of 'extractTerms')
headingTerms :: Page -> M.Map T.Text Int
headingTerms p = M.fromListWith (+)
  [ (t, 1) | t <- extractTerms (alg $ pageLang p) (T.unlines $ pageHeadings p) ]

-- | The best terms of a page, the best first, by their score: the weight of
-- the term in the page (occurrences in the content, plus 3 times those in
-- the title, plus 2 times those in the headings: these are also in the
-- content), sublinear, times its inverse document frequency (n documents).
-- Left out: the terms of weight 1 (seen once, not in a title), numbers and
-- quantities (a number followed by lower case letters: 350g, 15min), and the
-- terms under the relative threshold.
bestTerms :: Int -> M.Map T.Text Int -> [TermStats PageField] -> [T.Text]
bestTerms n headings stats = case scored of
  [] -> []
  (_, top) : _ -> take maxKeywords [ t | (t, s) <- scored, s >= relativeThreshold * top ]
  where
    scored = sortOn (\(t, s) -> (Down s, t))
      [ (termText ts, (1 + log (fromIntegral w)) * idf ts)
      | ts <- stats
      , let w = weight ts
      , w >= 2
      , not (quantity $ termText ts) ]
    weight ts = count ts ContentField + 3 * count ts TitleField
                + 2 * M.findWithDefault 0 (termText ts) headings
    count ts f = fromMaybe 0 (lookup f (termCounts ts))
    idf ts = log (fromIntegral n / fromIntegral (max 1 $ termDocuments ts)) :: Double
    quantity t = case T.uncons t of
      Just (c, _) | isDigit' c -> T.all (\x -> isDigit' x || x `elem` ("abcdefghijklmnopqrstuvwxyz.," :: String)) t
      _ -> False
    isDigit' c = c >= '0' && c <= '9'

-- | For each given term of a page, the word of its title and content that
-- gives it the most often, as written (the shortest one, then the first in
-- order, on a tie)
pageWords :: Page -> S.Set T.Text -> M.Map T.Text T.Text
pageWords p terms = M.map best counts
  where
    text = pageTitle p <> " " <> pagePlainText p
    counts = M.fromListWith (M.unionWith (+))
      [ (t, M.singleton w (1 :: Int)) | (t, w) <- writtenTerms (alg $ pageLang p) text
                                      , S.member t terms ]
    best ws = case sortOn (\(w, c) -> (Down c, T.length w, w)) (M.toList ws) of
      (w, _) : _ -> w
      [] -> ""  -- not reached: each term has a word

-- | The keywords of a page as a field of the lists ("a, b, c"), none
-- without them
keywordsText :: PagesDB -> Page -> Maybe T.Text
keywordsText db p = case M.findWithDefault [] (pageUID p) (pageKeywords db) of
  [] -> Nothing
  ks -> Just (T.intercalate ", " (map kwWord ks))
