{-# LANGUAGE TemplateHaskell #-}
-- | Keywords of the pages, for the agents: the terms that tell a page from
-- the others (TF-IDF), computed from the text of the pages, so that nobody
-- has to write them. Design in @CLAUDE/projects/mots-cles-pages-2026-11-21.md@
-- (iterations 8 and 9).
module Kiwi.Keywords
  ( pagesKeywords
  , readKeywords
  , keywordsText
  , bestTerms        -- exported for tests
  , maxKeywords      -- exported for tests
  ) where

import           Control.Concurrent.MVar (readMVar)
import           Data.FileEmbed (embedFileRelative)
import           Data.List (sortOn)
import qualified Data.Map.Strict as M
import           Data.Ord (Down (..))
import           Data.ByteString (ByteString)
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import           Kiwi.Search (alg, stripAccents, tokenize, wordStems, writtenWords)
import           Kiwi.Types

-- | At most this number of keywords by page
maxKeywords :: Int
maxKeywords = 10

-- | A term is a keyword when its score is at least this part of the best
-- score of the page: a short page has fewer keywords, rather than common
-- words
relativeThreshold :: Double
relativeThreshold = 0.4

-- | Keywords of the pages, the best first, the words as written in each
-- page. All the pages are needed, as the score of a term depends on the
-- number of pages that contain it. The keywords are evaluated.
pagesKeywords :: [Page] -> M.Map PageUID [T.Text]
pagesKeywords pages = M.fromList [ (pageUID p, forced (keywords ts)) | (p, ts) <- tables ]
  where
    tables = [ (p, pageTerms p) | p <- pages ]
    n = length pages
    docs = M.unionsWith (+) [ M.map (const 1) weights | (_, (weights, _)) <- tables ]
    keywords (weights, written) =
      [ T.copy w | t <- bestTerms n docs weights, Just w <- [mostWritten t written] ]
    forced ks = foldr seq () ks `seq` ks

-- | Terms of a page, stop words of its lang left out, with their weight:
-- those of its content (weight 1), of its title (3) and of its headings (2
-- more, as they are also in the content); and the words of the content and
-- of the title, with their term and number of occurrences. Each distinct
-- word is stemmed once.
pageTerms :: Page -> (M.Map T.Text Int, [(T.Text, T.Text, Int)])
pageTerms p = (M.fromListWith (+) [ (t, w * c) | (w, ws) <- texts, (t, _, c) <- ws ], written)
  where
    texts = [ (1, terms (pagePlainText p)), (3, terms (pageTitle p))
            , (2, terms (T.unlines $ pageHeadings p)) ]
    written = concat [ ws | (w, ws) <- texts, w /= (2 :: Int) ]
    lg = pageLang p
    stops = stopWords lg
    terms text =
      let counts = M.toList $ M.fromListWith (+) [ (x, 1 :: Int) | x <- writtenWords text ]
          kept = [ wc | wc@(x, _) <- counts, stripAccents (T.toLower x) `S.notMember` stops ]
      in [ (t, x, c) | (t, (x, c)) <- zip (wordStems (alg lg) (map fst kept)) kept ]

-- | The word that gives a term the most often (the shortest one, then the
-- first in order, on a tie), none for a term of the headings alone
mostWritten :: T.Text -> [(T.Text, T.Text, Int)] -> Maybe T.Text
mostWritten t written =
  case sortOn (\(w, c) -> (Down c, T.length w, w)) (M.toList counts) of
    (w, _) : _ -> Just w
    [] -> Nothing
  where counts = M.fromListWith (+) [ (w, c) | (t', w, c) <- written, t' == t ]

-- | The best terms of a page, the best first, by their score: the weight of
-- the term in the page (see 'pageTerms'), sublinear, times its inverse
-- document frequency (n pages, the number of pages of each term). Left out:
-- the terms of weight 1 (seen once, not in a title), numbers and quantities
-- (a number followed by lower case letters: 350g, 15min), and the terms
-- under the relative threshold.
bestTerms :: Int -> M.Map T.Text Int -> M.Map T.Text Int -> [T.Text]
bestTerms n docs weights = case scored of
  [] -> []
  (_, top) : _ -> take maxKeywords [ t | (t, s) <- scored, s >= relativeThreshold * top ]
  where
    scored = sortOn (\(t, s) -> (Down s, t))
      [ (t, (1 + log (fromIntegral w)) * idf t)
      | (t, w) <- M.toList weights
      , w >= 2
      , not (quantity t) ]
    idf t = log (fromIntegral n / fromIntegral (max 1 $ M.findWithDefault 1 t docs)) :: Double
    quantity t = case T.uncons t of
      Just (c, _) | isDigit' c -> T.all (\x -> isDigit' x || x `elem` ("abcdefghijklmnopqrstuvwxyz.," :: String)) t
      _ -> False
    isDigit' c = c >= '0' && c <= '9'

-- | Stop words of a lang (lower case, without accents), none for the langs
-- without a list. Lists of stopwords-iso (MIT, see @static/stopwords@).
stopWords :: Lang -> S.Set T.Text
stopWords l = case l of
  French -> frenchStopWords
  English -> englishStopWords
  Spanish -> spanishStopWords
  _ -> S.empty

frenchStopWords, englishStopWords, spanishStopWords :: S.Set T.Text
frenchStopWords = stopWordList $(embedFileRelative "static/stopwords/stopwords-fr.txt")
englishStopWords = stopWordList $(embedFileRelative "static/stopwords/stopwords-en.txt")
spanishStopWords = stopWordList $(embedFileRelative "static/stopwords/stopwords-es.txt")

-- | The words of a list, split as the pages are ("aujourd'hui" gives
-- "aujourd" and "hui")
stopWordList :: ByteString -> S.Set T.Text
stopWordList = S.fromList . map stripAccents . tokenize . TE.decodeUtf8

-- | The keywords of the pages of a database: waits for them when they are
-- not computed yet (see 'pageKeywords'); none for a database that was not
-- loaded
readKeywords :: PagesDB -> IO (M.Map PageUID [T.Text])
readKeywords db = maybe (return M.empty) readMVar (pageKeywords db)

-- | The keywords of a page as a field of the lists ("a, b, c"), none
-- without them
keywordsText :: M.Map PageUID [T.Text] -> Page -> Maybe T.Text
keywordsText kws p = case M.findWithDefault [] (pageUID p) kws of
  [] -> Nothing
  ks -> Just (T.intercalate ", " ks)
