-- | Related pages: the pages that look like a page, by their keywords, their
-- tags and the custom metadata marked @relating: yes@ in @kiwi.ini@. Each
-- page is a set of weighted features, and two pages are as close as the
-- cosine of their sets. Computed when shown, over the given pages. Design
-- in @CLAUDE/projects/pages-connexes-2026-11-23.md@.
module Kiwi.Related
  ( relatedPages
  , maxRelated      -- exported for tests
  , similarity      -- exported for tests
  ) where

import           Data.List (sortOn)
import qualified Data.Map.Strict as M
import           Data.Ord (Down (..))
import qualified Data.Set as S
import qualified Data.Text as T

import           Kiwi.Tags (segmentsToTag, tagSubSegments)
import           Kiwi.Types

-- | At most this number of related pages
maxRelated :: Int
maxRelated = 5

-- | A related page has at least this similarity (between 0 and 1), so that
-- one common tag of many pages does not bring them all
minSimilarity :: Double
minSimilarity = 0.2

-- | What brings pages together
data Feature = FKeyword T.Text             -- ^ stem of a keyword
             | FTag TagId                  -- ^ a tag or one of its parents
             | FCustom T.Text DocIndexKey  -- ^ value of a relating custom field
  deriving (Eq, Ord, Show)

-- | The features of a page with their weight: keywords 1, tags 2 (each tag
-- and its parents: @a > b@ gives @a@ and @a > b@), values of the relating
-- custom fields 1
pageFeatures :: S.Set T.Text -> M.Map PageUID [Keyword] -> Page -> M.Map Feature Double
pageFeatures relating kws p = M.fromList $
     [ (FKeyword (keywordStem k), 1) | k <- M.findWithDefault [] (pageUID p) kws ]
  ++ [ (FTag (segmentsToTag s), 2) | t <- pageTags p, s <- tagSubSegments t ]
  ++ [ (FCustom (cmdName c) k, 1) | c <- pageCustomMeta p, cmdName c `S.member` relating
                                  , k <- cmdKeys c ]

-- | Cosine of two sets of weighted features: 1 for the same features, 0
-- without common feature
similarity :: M.Map Feature Double -> M.Map Feature Double -> Double
similarity a b
  | na == 0 || nb == 0 = 0
  | otherwise = sum (M.intersectionWith (*) a b) / sqrt (na * nb)
  where na = norm a
        nb = norm b
        norm = sum . map (^ (2 :: Int)) . M.elems

-- | The pages related to a page among the given ones (itself left out), the
-- closest first (then by title), at most 'maxRelated', at least
-- 'minSimilarity'; given the custom fields of the config and the keywords
-- of the pages
relatedPages :: [CustomMetaConfig] -> M.Map PageUID [Keyword] -> Page -> [Page] -> [Page]
relatedPages config kws p candidates =
  map snd $ take maxRelated $ sortOn (\(s, q) -> (Down s, pageTitle q, pageUID q))
    [ (s, q) | q <- candidates, pageUID q /= pageUID p
             , let s = similarity features (pageFeatures relating kws q)
             , s >= minSimilarity ]
  where relating = S.fromList [ cmcName c | c <- config, cmcRelating c ]
        features = pageFeatures relating kws p
