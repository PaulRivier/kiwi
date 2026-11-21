{-# LANGUAGE NamedFieldPuns #-}

-- | Statistics of the index, to weigh the terms of a document (TF-IDF for
-- instance) without extracting them again: number of documents, and for each
-- term of a document its occurrences by field and the number of documents
-- that contain it.
--
-- (Added for Kiwi, not part of full-text-search 0.2.2.3.)
module Data.SearchEngine.Stats (
    TermStats(..),
    documentCount,
    documentTermStats,
  ) where

import Data.SearchEngine.Types
import qualified Data.SearchEngine.SearchIndex as SI
import qualified Data.SearchEngine.DocTermIds as DocTermIds
import qualified Data.SearchEngine.DocIdSet as DocIdSet

import Data.Ix (Ix)
import qualified Data.Ix as Ix

-- | A term of a document
data TermStats field = TermStats {
       -- | the term, as 'extractDocumentTerms' gave it
       termText      :: !Term,
       -- | its occurrences in each field of the document (fields where it
       -- does not occur excluded; at most 255, the index does not count
       -- further)
       termCounts    :: [(field, Int)],
       -- | the number of documents of the index that contain it
       termDocuments :: !Int
     }
  deriving Show

-- | /O(1)/ Number of documents in the index
documentCount :: SearchEngine doc key field feature -> Int
documentCount SearchEngine{searchIndex} = SI.docCount searchIndex

-- | The terms of a document, in no particular order, or 'Nothing' when the
-- document is not in the index
documentTermStats :: (Ord key, Ix field, Bounded field) =>
                     SearchEngine doc key field feature -> key
                  -> Maybe [TermStats field]
documentTermStats SearchEngine{searchIndex} key = do
    docTerms <- SI.lookupDocKey searchIndex key
    let (n, termIdAt, countAt) = DocTermIds.denseTable docTerms
    return
      [ TermStats {
          termText      = SI.getTerm searchIndex termId,
          termCounts    = [ (f, c) | f <- fields, let c = countAt i f, c > 0 ],
          termDocuments = DocIdSet.size (SI.lookupTermId searchIndex termId)
        }
      | i <- [0 .. n - 1]
      , let termId = termIdAt i ]
  where
    fields = Ix.range (minBound, maxBound)
