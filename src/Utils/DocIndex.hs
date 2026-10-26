{- |
   Module      : Utils.DocIndex
   Copyright   : Copyright (C) 2020 - Paul Rivier
   License     : GNU GPL, version 3 or above

An in-memory index of documents by the keys of their fields: each field of a
document gives a list of keys (its tags, its year...), and the index maps each
key of each field to the set of the ids of the documents having it.

Invariants:

* every id in the index is the id of a stored document;
* the index has no empty set of ids, nor field without keys: it only depends
  on the stored documents, not on the order of the insertions and removals;
* a document without any key is stored, but never matches a query.

The ids of the fields must be distinct: when two fields have the same id, only
the keys of the last one are indexed.
-}

module Utils.DocIndex where

import           Data.List (foldl1', sortOn)
import qualified Data.Map.Strict as M
import qualified Data.Set as S


-- | A field of the documents, and how to get its keys from a document
data Field doc field key = Field
  { fieldId          :: !field
  , fieldExtractKeys :: doc -> [key] }


type ExtractId doc id = doc -> id

-- | Ids of the documents having each key of a field
type FieldMap key id = M.Map key (S.Set id)

type DocIndexMap field key id = M.Map field (FieldMap key id)

data DocIndex doc id field key =
  DocIndex { extractId :: ExtractId doc id
           , fields    :: [Field doc field key]
           , store     :: !(M.Map id doc)
           , index     :: !(DocIndexMap field key id)
           }


-- | An index without documents
empty :: ExtractId doc id ->
         [Field doc field key] ->
         DocIndex doc id field key
empty ei fds = DocIndex ei fds M.empty M.empty


findDoc :: (Ord id) =>
           DocIndex doc id field key ->
           id ->
           Maybe doc
findDoc docIdx docId = M.lookup docId (store docIdx)


-- | The documents, by id
documentsList :: DocIndex doc id field key -> [doc]
documentsList docIdx = M.elems $ store docIdx


-- | Add a document, replacing the one with the same id
insert :: (Ord field, Ord id, Ord key) =>
          DocIndex doc id field key ->
          doc ->
          DocIndex doc id field key
insert docIdx doc =
  let docId = extractId docIdx doc
      cleared = remove docIdx docId
  in cleared { store = M.insert docId doc (store cleared)
             , index = M.unionWith (M.unionWith S.union) (index cleared) $
                       makeFieldsKeysMap (fields docIdx) doc docId }


insertMany :: (Ord field, Ord id, Ord key) =>
              DocIndex doc id field key ->
              [doc] ->
              DocIndex doc id field key
insertMany = foldl' insert


-- | Remove a document, if it is stored, and the keys only it had
remove :: (Ord field, Ord id, Ord key) =>
          DocIndex doc id field key ->
          id ->
          DocIndex doc id field key
remove docIdx docId =
  case findDoc docIdx docId of
    Nothing -> docIdx
    Just doc ->
      docIdx { store = M.delete docId (store docIdx)
             , index = M.differenceWith removeKeys (index docIdx) $
                       makeFieldsKeysMap (fields docIdx) doc docId }
  where
    removeKeys fieldMap docKeys =
      nonEmpty $ M.differenceWith (\ids _ -> nonEmpty $ S.delete docId ids)
                                  fieldMap docKeys


removeMany :: (Ord field, Ord id, Ord key) =>
              DocIndex doc id field key ->
              [id] ->
              DocIndex doc id field key
removeMany = foldl' remove


-- | The index of a single document, without the fields it has no key for
makeFieldsKeysMap :: (Ord field, Ord key) =>
                     [Field doc field key] ->
                     doc ->
                     id  ->
                     DocIndexMap field key id
makeFieldsKeysMap flds doc docId =
  M.fromList [ (fieldId f, M.fromList [ (k, S.singleton docId) | k <- keys ])
             | f <- flds, let keys = fieldExtractKeys f doc, not (null keys) ]


-- | The index restricted to the given documents. The sets of ids are
-- restricted, which is cheaper than indexing the documents again, and the
-- index is kept as is when it has no other documents.
narrowOnSet :: (Ord id) =>
               DocIndex doc id field key ->
               S.Set id ->
               DocIndex doc id field key
narrowOnSet docIdx ids
  | M.size newStore == M.size (store docIdx) = docIdx
  | otherwise =
      docIdx { store = newStore
             , index = M.mapMaybe (nonEmpty . M.mapMaybe restrictIds) (index docIdx) }
  where
    newStore = M.restrictKeys (store docIdx) ids
    restrictIds = nonEmpty . S.intersection ids


-- | Nothing for an empty set or map, so that the index keeps none
nonEmpty :: Foldable t => t a -> Maybe (t a)
nonEmpty x = if null x then Nothing else Just x


-- Query

-- | 'And' and 'All' without any query or key match no document, as 'Or' and
-- 'Any'. The keys of 'All' and 'Any' are those of a single field.
data Query field key = And [Query field key]     -- ^ documents matching every query
                     | Or [Query field key]      -- ^ documents matching one of the queries
                     | Only field key            -- ^ documents having the key
                     | All field (S.Set key)     -- ^ documents having all the keys
                     | Any field (S.Set key)     -- ^ documents having one of the keys
                     | HasField field            -- ^ documents having a key for the field
                     | Range field key key       -- ^ documents having a key between both, included
                     | GreaterThanOrEq field key -- ^ documents having a key >= this one
                     | LowerThanOrEq field key   -- ^ documents having a key <= this one
  deriving (Eq, Show)


-- | Documents matching the query, by id
query :: (Ord field, Ord id, Ord key) =>
         DocIndex doc id field key ->
         Query field key ->
         [doc]
query docIdx q = M.elems $ M.restrictKeys (store docIdx) (queryIdSet docIdx q)


-- | The index restricted to the documents matching the query
narrow :: (Ord field, Ord id, Ord key) =>
          DocIndex doc id field key ->
          Query field key ->
          DocIndex doc id field key
narrow docIdx q = narrowOnSet docIdx (queryIdSet docIdx q)


queryIdSet :: (Ord field, Ord id, Ord key) =>
              DocIndex doc id field key ->
              Query field key ->
              S.Set id
queryIdSet docIdx q = case q of
  And [] -> S.empty
  -- the next queries are not run once the intersection is empty
  And qs -> foldr1 S.intersection $ map (queryIdSet docIdx) qs
  Or qs -> S.unions $ map (queryIdSet docIdx) qs
  Only f k -> lookupIndex docIdx f k
  All f ks -> case traverse (`M.lookup` fieldMap f) (S.toList ks) of
    Just sets@(_ : _) -> foldl1' S.intersection $ sortOn S.size sets
    -- no key, or a key that no document has
    _ -> S.empty
  Any f ks -> S.unions $ M.restrictKeys (fieldMap f) ks
  HasField f -> S.unions $ fieldMap f
  Range f k1 k2 -> S.unions $ M.takeWhileAntitone (<= k2) $
                   M.dropWhileAntitone (< k1) $ fieldMap f
  GreaterThanOrEq f k -> S.unions $ M.dropWhileAntitone (< k) $ fieldMap f
  LowerThanOrEq f k -> S.unions $ M.takeWhileAntitone (<= k) $ fieldMap f
  where
    fieldMap = lookupFieldMap (index docIdx)



-- normalizeQuery :: Query -> Query
-- normalizeQuery q = case q of
--   And qs -> And (joinQS qs)
--   Or  qs -> Or (joinQS qs)
--   x -> x


lookupFieldMap :: (Ord field) =>
                  DocIndexMap field key id ->
                  field ->
                  FieldMap key id
lookupFieldMap idx f = M.findWithDefault M.empty f idx


elemsAtFieldKeys :: (Ord field, Ord key) =>
                    DocIndexMap field key id ->
                    field -> S.Set key ->
                    [S.Set id]
elemsAtFieldKeys idx f keys = M.elems $
                              M.restrictKeys (lookupFieldMap idx f) keys


-- | Ids of the documents having the key
lookupIndex :: (Ord field, Ord key) =>
               DocIndex doc id field key ->
               field ->
               key ->
               S.Set id
lookupIndex docIdx f key = M.findWithDefault S.empty key $
                           lookupFieldMap (index docIdx) f

-- | The fields that have keys
getFields :: DocIndex doc id field key ->
             [field]
getFields docIdx = M.keys (index docIdx)


getFieldKeys :: (Ord field) =>
                DocIndex doc id field key ->
                field ->
                [key]
getFieldKeys docIdx f = M.keys $ lookupFieldMap (index docIdx) f


-- | Number of documents
cardinal :: DocIndex doc id field key ->
            Int
cardinal docIdx = M.size $ store docIdx

nodup :: Ord a => [a] -> [a]
nodup = S.toList . S.fromList


splitInclude :: (Ord a) => a -> S.Set a -> (S.Set a, S.Set a)
splitInclude n s = case S.splitMember n s of
  (l,True,h) -> (S.insert n l, S.insert n h)
  (l,False,h) -> (l, h)
