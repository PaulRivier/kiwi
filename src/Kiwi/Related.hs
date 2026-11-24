-- | Related pages: the pages that look like a page, by their keywords, their
-- tags and the custom metadata with a @relating: WEIGHT@ in @kiwi.ini@. Each
-- page is a set of weighted features, the keys of these fields in the index
-- of the pages, and two pages are as close as the cosine of their sets. The
-- index gives the pages of each feature; the norm of each page is computed
-- with its keywords, after each load ('withKeywords'). Design in
-- @CLAUDE/projects/pages-connexes-2026-11-23.md@ (iteration 7) and
-- @CLAUDE/projects/docindex-vues-2026-11-24.md@.
module Kiwi.Related
  ( withKeywords
  , relatedPages
  , maxRelated      -- exported for tests
  ) where

import           Data.List (find, sortOn)
import qualified Data.Map.Strict as M
import           Data.Ord (Down (..))
import qualified Data.Set as S

import           Kiwi.Types
import qualified Utils.DocIndex.Restricted as RDI

-- | At most this number of related pages
maxRelated :: Int
maxRelated = 5

-- | A related page has at least this similarity (between 0 and 1), so that
-- one common tag of many pages does not bring them all
minSimilarity :: Double
minSimilarity = 0.2

-- | The weight of the features of a field: keywords 3, tags (each tag and
-- its parents) 5, the values of a custom field the weight of its
-- @relating@; none for the other fields
fieldWeight :: [CustomMetaConfig] -> MetaField -> Maybe Double
fieldWeight config f = case f of
  FieldKeyword -> Just 3
  FieldTag -> Just 5
  FieldCustom n -> case find ((== n) . cmcName) config of
    Just c | cmcRelating c > 0 -> Just (cmcRelating c)
    _ -> Nothing
  _ -> Nothing

-- | The features of a page with their weight, given the keys of a field of
-- a page in the index: those of the weighted fields
pageFeatures :: [CustomMetaConfig] -> (MetaField -> Page -> [DocIndexKey]) -> Page
             -> [((MetaField, DocIndexKey), Double)]
pageFeatures config keys p =
  [ ((f, k), w) | f <- FieldKeyword : FieldTag : map (FieldCustom . cmcName) config
                , Just w <- [fieldWeight config f], k <- keys f p ]

-- | The page with these keywords, and the sum of the squares of the weights
-- of its features ('pageRelatedNorm'), given the keys of a field of a page
-- in the index: the only way to give a page its keywords, so that its norm
-- follows them
withKeywords :: [CustomMetaConfig] -> (MetaField -> Page -> [DocIndexKey]) -> [Keyword] -> Page
             -> Page
withKeywords config keys ks p =
  p' { pageRelatedNorm = sum [ w * w | (_, w) <- pageFeatures config keys p' ] }
  where p' = p { pageKeywords = ks }

-- | The pages related to a page (itself left out), among the pages of the
-- index that the predicate keeps, the closest first (then by title), at
-- most 'maxRelated', at least 'minSimilarity'. The similarity of two pages
-- is the cosine of their features: only the pages with a common feature
-- are compared, found by the index for each feature of the page, which
-- give the products of the weights of the common features (the same weight
-- in both pages).
relatedPages :: [CustomMetaConfig] -> RDI.RestrictedDocIndex Page PageUID MetaField DocIndexKey
             -> (PageUID -> Bool) -> Page -> [Page]
relatedPages config idx kept p =
  map snd $ take maxRelated $ sortOn (\(s, q) -> (Down s, pageTitle q, pageUID q))
    [ (s, q) | (u, d) <- M.toList products, kept u, Just q <- [RDI.findDoc idx u]
             , let s = d / sqrt (pageRelatedNorm p * pageRelatedNorm q)
             , s >= minSimilarity ]
  where
    products = M.fromListWith (+)
      [ (u, w * w) | ((f, k), w) <- pageFeatures config (RDI.docFieldKeys idx) p
                   , u <- S.toList (RDI.lookupIndex idx f k), u /= pageUID p ]
