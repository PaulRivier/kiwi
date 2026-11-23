-- | Related pages: the pages that look like a page, by their keywords, their
-- tags and the custom metadata marked @relating: yes@ in @kiwi.ini@. Each
-- page is a set of weighted features, and two pages are as close as the
-- cosine of their sets. The features of every page are computed at each
-- load, the related pages when shown, over the given pages. Design
-- in @CLAUDE/projects/pages-connexes-2026-11-23.md@.
module Kiwi.Related
  ( relatedFeaturesOf
  , relatedPages
  , maxRelated      -- exported for tests
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

-- | The features of the pages, given the custom fields of the config and
-- the keywords of the pages; computed at each load (see
-- 'Kiwi.PagesDB.updatePagesDB'), as they change with the keywords
relatedFeaturesOf :: [CustomMetaConfig] -> M.Map PageUID [Keyword] -> [Page] -> M.Map PageUID Features
relatedFeaturesOf config kws pages =
  M.fromList [ (pageUID p, pageFeatures relating kws p) | p <- pages ]
  where relating = S.fromList [ cmcName c | c <- config, cmcRelating c ]

-- | The features of a page with their weight: keywords 1, tags 2 (each tag
-- and its parents: @a > b@ gives @a@ and @a > b@), values of the relating
-- custom fields 1
pageFeatures :: S.Set T.Text -> M.Map PageUID [Keyword] -> Page -> Features
pageFeatures relating kws p = Features weights (sum $ map (^ (2 :: Int)) $ M.elems weights)
  where
    weights = M.fromList $
         [ (FKeyword (keywordStem k), 1) | k <- M.findWithDefault [] (pageUID p) kws ]
      ++ [ (FTag (segmentsToTag s), 2) | t <- pageTags p, s <- tagSubSegments t ]
      ++ [ (FCustom (cmdName c) k, 1) | c <- pageCustomMeta p, cmdName c `S.member` relating
                                      , k <- cmdKeys c ]

-- | Cosine of two sets of weighted features: 1 for the same features, 0
-- without common feature
similarity :: Features -> Features -> Double
similarity a b
  | featureNorm a == 0 || featureNorm b == 0 = 0
  | otherwise = sum (M.intersectionWith (*) (featureWeights a) (featureWeights b))
                / sqrt (featureNorm a * featureNorm b)

-- | The pages related to a page among the given ones (itself left out), the
-- closest first (then by title), at most 'maxRelated', at least
-- 'minSimilarity', given the features of the pages
relatedPages :: M.Map PageUID Features -> Page -> [Page] -> [Page]
relatedPages features p candidates =
  map snd $ take maxRelated $ sortOn (\(s, q) -> (Down s, pageTitle q, pageUID q))
    [ (s, q) | q <- candidates, pageUID q /= pageUID p
             , let s = similarity (featuresOf p) (featuresOf q)
             , s >= minSimilarity ]
  where featuresOf q = M.findWithDefault (Features M.empty 0) (pageUID q) features
