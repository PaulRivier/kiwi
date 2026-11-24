{- |
   Module      : Utils.DocIndex.Restricted
   Copyright   : Copyright (C) 2026 - Paul Rivier
   License     : GNU GPL, version 3 or above

A view of a 'DocIndex' restricted to some of its documents: the whole index
and the set of the visible ids. Restricting a view is only an intersection of
sets, the index is not copied: the restriction is applied to what is read,
when it is read. As every query is made of unions and intersections, a query
on a view gives the documents of the query on the whole index that are
visible.

A view is read only: the documents are inserted in and removed from the
'DocIndex', of which 'whole' gives a new view. The whole index of a view is
not exported, so that no read gives a hidden document: 'visibleIndex' gives
the index of the visible documents only.

Invariant: the visible ids are ids of stored documents.
-}
module Utils.DocIndex.Restricted
  ( RestrictedDocIndex
  , whole
  , visibleIndex
  , narrow
  , narrowOnSet
  , query
  , queryIdSet
  , findDoc
  , documentsList
  , cardinal
  , lookupIndex
  , getFieldKeys
  , docFieldKeys
  ) where

import qualified Data.Map.Strict as M
import qualified Data.Set as S

import           Utils.DocIndex (DocIndex, Query)
import qualified Utils.DocIndex as DI


data RestrictedDocIndex doc id field key =
  RestrictedDocIndex { docIndex :: !(DocIndex doc id field key)  -- ^ the whole index
                     , visible  :: !(Maybe (S.Set id))  -- ^ Nothing: every document
                     }


-- | Every document of the index
whole :: DocIndex doc id field key -> RestrictedDocIndex doc id field key
whole idx = RestrictedDocIndex idx Nothing


-- | The index of the visible documents: the index itself for a whole view
-- (nothing is copied), else a copy restricted to them (see
-- 'DI.narrowOnSet'), so that the hidden documents are never given
visibleIndex :: (Ord id) => RestrictedDocIndex doc id field key -> DocIndex doc id field key
visibleIndex r = maybe (docIndex r) (DI.narrowOnSet $ docIndex r) (visible r)


-- | The visible ids among these
restrict :: (Ord id) => RestrictedDocIndex doc id field key -> S.Set id -> S.Set id
restrict r ids = maybe ids (S.intersection ids) (visible r)


-- | The view restricted to the documents matching the query
narrow :: (Ord field, Ord id, Ord key) =>
          RestrictedDocIndex doc id field key -> Query field key ->
          RestrictedDocIndex doc id field key
narrow r q = r { visible = Just (queryIdSet r q) }


-- | The view restricted to the given documents
narrowOnSet :: (Ord id) =>
               RestrictedDocIndex doc id field key -> S.Set id ->
               RestrictedDocIndex doc id field key
narrowOnSet r ids = r { visible = Just $ case visible r of
                          Nothing -> S.intersection ids (M.keysSet $ DI.store $ docIndex r)
                          Just v -> S.intersection ids v }


-- | Visible documents matching the query, by id
query :: (Ord field, Ord id, Ord key) =>
         RestrictedDocIndex doc id field key -> Query field key -> [doc]
query r q = M.elems $ M.restrictKeys (DI.store $ docIndex r) (queryIdSet r q)


queryIdSet :: (Ord field, Ord id, Ord key) =>
              RestrictedDocIndex doc id field key -> Query field key -> S.Set id
queryIdSet r q = restrict r $ DI.queryIdSet (docIndex r) q


findDoc :: (Ord id) => RestrictedDocIndex doc id field key -> id -> Maybe doc
findDoc r docId
  | maybe True (S.member docId) (visible r) = DI.findDoc (docIndex r) docId
  | otherwise = Nothing


-- | The visible documents, by id
documentsList :: (Ord id) => RestrictedDocIndex doc id field key -> [doc]
documentsList r = case visible r of
  Nothing -> DI.documentsList (docIndex r)
  Just v -> M.elems $ M.restrictKeys (DI.store $ docIndex r) v


-- | Number of visible documents
cardinal :: RestrictedDocIndex doc id field key -> Int
cardinal r = maybe (DI.cardinal $ docIndex r) S.size (visible r)


-- | Ids of the visible documents having the key
lookupIndex :: (Ord field, Ord id, Ord key) =>
               RestrictedDocIndex doc id field key -> field -> key -> S.Set id
lookupIndex r f k = restrict r $ DI.lookupIndex (docIndex r) f k


-- | The keys of the field that a visible document has
getFieldKeys :: (Ord field, Ord id) =>
                RestrictedDocIndex doc id field key -> field -> [key]
getFieldKeys r f = case visible r of
  Nothing -> DI.getFieldKeys (docIndex r) f
  Just v -> [ k | (k, ids) <- M.toList $ DI.lookupFieldMap (DI.index $ docIndex r) f
                , not (S.disjoint ids v) ]


-- | The keys of the field of a document, as the index gives them (see
-- 'DI.docFieldKeys')
docFieldKeys :: (Eq field) => RestrictedDocIndex doc id field key -> field -> doc -> [key]
docFieldKeys r = DI.docFieldKeys (docIndex r)
