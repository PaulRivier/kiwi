-- | Related pages: the pages that look like a page, by their keywords, their
-- tags and the custom metadata marked @relating: yes@ in @kiwi.ini@. Each
-- page is a set of weighted features, and two pages are as close as the
-- cosine of their sets. The features of every page, and the pages of each
-- feature, are computed at each load, the related pages when shown. Design
-- in @CLAUDE/projects/pages-connexes-2026-11-23.md@.
module Kiwi.Related
  ( relatedIndexOf
  , emptyRelatedIndex
  , relatedPages
  , maxRelated      -- exported for tests
  ) where

import           Data.List (sortOn)
import qualified Data.Map.Strict as M
import           Data.Ord (Down (..))
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

-- | No page, before the first load
emptyRelatedIndex :: RelatedIndex
emptyRelatedIndex = RelatedIndex M.empty M.empty

-- | The features of the pages and the pages of each feature, given the
-- custom fields of the config and the keywords of the pages; computed at
-- each load (see 'Kiwi.PagesDB.updatePagesDB'), as they change with the
-- keywords
relatedIndexOf :: [CustomMetaConfig] -> M.Map PageUID [Keyword] -> [Page] -> RelatedIndex
relatedIndexOf config kws pages =
  RelatedIndex { riFeatures = features
               , riPages = M.fromListWith (++) [ (f, [u]) | (u, fs) <- M.toList features
                                                         , f <- M.keys (featureWeights fs) ] }
  where
    relating = M.fromList [ (cmcName c, cmcRelating c) | c <- config, cmcRelating c > 0 ]
    features = M.fromList [ (pageUID p, pageFeatures relating kws p) | p <- pages ]

-- | The features of a page with their weight, which depends on the kind of
-- feature only: keywords 1, tags 2 (each tag and its parents: @a > b@ gives
-- @a@ and @a > b@), values of the relating custom fields their weight
-- (given by field name)
pageFeatures :: M.Map T.Text Double -> M.Map PageUID [Keyword] -> Page -> Features
pageFeatures relating kws p = Features weights (sum $ map (^ (2 :: Int)) $ M.elems weights)
  where
    weights = M.fromList $
         [ (FKeyword (keywordStem k), 1) | k <- M.findWithDefault [] (pageUID p) kws ]
      ++ [ (FTag (segmentsToTag s), 2) | t <- pageTags p, s <- tagSubSegments t ]
      ++ [ (FCustom (cmdName c) k, w) | c <- pageCustomMeta p, Just w <- [M.lookup (cmdName c) relating]
                                      , k <- cmdKeys c ]

-- | The pages related to a page (itself left out), among those that the
-- function gives, the closest first (then by title), at most 'maxRelated',
-- at least 'minSimilarity'. The similarity of two pages is the cosine of
-- their features: only the pages with a common feature are compared, found
-- by the pages of each feature of the page, which give the products of the
-- weights of the common features (the same weight in both pages).
relatedPages :: RelatedIndex -> (PageUID -> Maybe Page) -> Page -> [Page]
relatedPages index candidate p = case M.lookup (pageUID p) (riFeatures index) of
  Nothing -> []
  Just fp ->
    let products = M.fromListWith (+)
          [ (u, w * w) | (f, w) <- M.toList (featureWeights fp)
                       , u <- M.findWithDefault [] f (riPages index), u /= pageUID p ]
    in map snd $ take maxRelated $ sortOn (\(s, q) -> (Down s, pageTitle q, pageUID q))
         [ (s, q) | (u, d) <- M.toList products
                  , Just q <- [candidate u], Just fq <- [M.lookup u (riFeatures index)]
                  , let s = d / sqrt (featureNorm fp * featureNorm fq)
                  , s >= minSimilarity ]
