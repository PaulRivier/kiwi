{-# LANGUAGE TemplateHaskell #-}
-- | Keywords of the pages, for the agents: the terms that tell a page from
-- the others (TF-IDF), computed from the text of the pages, so that nobody
-- has to write them. Design in @CLAUDE/projects/mots-cles-pages-2026-11-21.md@
-- (iterations 8 and 9) and @CLAUDE/projects/session-23-nov.md@ (iterations 4
-- and 5: update by load).
module Kiwi.Keywords
  ( updateKeywords
  , emptyKeywords
  , keywordsText
  , pagesKeywords    -- exported for tests
  , bestTerms        -- exported for tests
  , maxKeywords      -- exported for tests
  ) where

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
maxKeywords = 20

-- | A term is a keyword when its score is at least this part of the best
-- score of the page: a short page has fewer keywords, rather than common
-- words
relativeThreshold :: Double
relativeThreshold = 0.4

-- | No keywords, before the first load
emptyKeywords :: Keywords
emptyKeywords = Keywords M.empty M.empty 0 0

-- | The keywords after a load, given all the pages, the pages removed (old
-- versions of the pages read again included) and the pages inserted. The
-- number of pages of each term is updated, and the keywords of the inserted
-- pages computed with it; the other pages keep theirs, computed with former
-- numbers. When the pages changed since the last computation of every page
-- reach a tenth of the pages (10 at least), every page is computed again
-- (true with the result). The keywords are evaluated.
updateKeywords :: [Page] -> [Page] -> [Page] -> Keywords -> (Keywords, Bool)
updateKeywords pages removed inserted kw
  | changes - kwFullAt kw >= max 10 (n `div` 10) =
      ((allKeywords pages) { kwChanges = changes, kwFullAt = changes }, True)
  | otherwise =
      ( Keywords { kwDocs = docs
                 , kwPages = M.union (M.fromList [ (pageUID p, keywordsOf n docs t)
                                                  | (p, t) <- newTables ])
                                     (foldl' (flip M.delete) (kwPages kw) (map pageUID removed))
                 , kwChanges = changes
                 , kwFullAt = kwFullAt kw }
      , False )
  where
    n = length pages
    changes = kwChanges kw + S.size (S.fromList $ map pageUID (removed ++ inserted))
    newTables = [ (p, pageTerms p) | p <- inserted ]
    docs = M.filter (> 0) $ addDocs 1 (map snd newTables) $
             addDocs (-1) (map pageTerms removed) (kwDocs kw)

-- | Every page computed (see 'updateKeywords')
allKeywords :: [Page] -> Keywords
allKeywords pages =
  Keywords { kwDocs = docs
           , kwPages = M.fromList [ (pageUID p, keywordsOf n docs t) | (p, t) <- tables ]
           , kwChanges = 0
           , kwFullAt = 0 }
  where
    tables = [ (p, pageTerms p) | p <- pages ]
    n = length pages
    docs = addDocs 1 (map snd tables) M.empty

-- | Keywords of the pages, the best first, all of them computed
pagesKeywords :: [Page] -> M.Map PageUID [T.Text]
pagesKeywords = kwPages . allKeywords

-- | The number of pages of each term, with the terms of the given pages
-- added (1) or removed (-1)
addDocs :: Int -> [Terms] -> M.Map T.Text Int -> M.Map T.Text Int
addDocs d tables docs0 = foldl' add docs0 tables
  where add docs (weights, _) = M.foldlWithKey' (\m t _ -> M.insertWith (+) t d m) docs weights

-- | The keywords of a page, given the number of pages and the number of
-- pages of each term, as written in the page, evaluated
keywordsOf :: Int -> M.Map T.Text Int -> Terms -> [T.Text]
keywordsOf n docs (weights, forms) = forced
  [ T.copy w | t <- bestTerms n docs weights, Just w <- [M.lookup t forms] ]
  where forced ks = foldr seq () ks `seq` ks

-- | The terms of a page with their weight, and the word written for each
-- (see 'pageTerms')
type Terms = (M.Map T.Text Int, M.Map T.Text T.Text)

-- | Terms of a page, stop words of its lang left out, with their weight:
-- those of its content (weight 1), of its title (3), of its summary (2) and
-- of its headings (2 more, as they are also in the content); and the word
-- written for each term: the one that gives it the most often in the
-- content, the title and the summary (the shortest one, then the first in
-- order, on a tie; none for a term of the headings alone). Each distinct
-- word is stemmed once.
pageTerms :: Page -> Terms
pageTerms p = (M.fromListWith (+) [ (t, w * c) | (w, _, ws) <- texts, (t, _, c) <- ws ], forms)
  where
    -- weight, and whether the words are written forms
    texts = [ (1 :: Int, True, terms (pagePlainText p)), (3, True, terms (pageTitle p))
            , (2, True, maybe [] terms (pageSummary p))
            , (2, False, terms (T.unlines $ pageHeadings p)) ]
    written = M.fromListWith (+) [ ((t, x), c) | (_, True, ws) <- texts, (t, x, c) <- ws ]
    forms = M.map fst $ M.fromListWith better [ (t, (x, c)) | ((t, x), c) <- M.toList written ]
    better new old = if rank new < rank old then new else old
    rank (x, c) = (Down c, T.length x, x)
    lg = pageLang p
    stops = stopWords lg
    terms text =
      let counts = M.toList $ M.fromListWith (+) [ (x, 1 :: Int) | x <- writtenWords text ]
          kept = [ wc | wc@(x, _) <- counts, stripAccents (T.toLower x) `S.notMember` stops ]
      in [ (t, x, c) | (t, (x, c)) <- zip (wordStems (alg lg) (map fst kept)) kept ]

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

-- | The keywords of a page as a field of the lists ("a, b, c"), none
-- without them
keywordsText :: M.Map PageUID [T.Text] -> Page -> Maybe T.Text
keywordsText kws p = case M.findWithDefault [] (pageUID p) kws of
  [] -> Nothing
  ks -> Just (T.intercalate ", " ks)
