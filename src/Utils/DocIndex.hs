{-# LANGUAGE BangPatterns #-}

{- |
   Module      : Utils.DocIndex
   Copyright   : Copyright (C) 2020 - Paul Rivier 
   License     : GNU GPL, version 3 or above

-}

module Utils.DocIndex where

import Prelude hiding (id, last, init)
-- import qualified Prelude
import           Data.List (foldl', foldl1')
import           Data.Maybe (catMaybes)
import qualified Data.Map.Strict as M
import qualified Data.Set as S


data Field doc field key = Field
  { fieldId          :: !field
  , fieldExtractKeys :: doc -> [key] }
  


type ExtractId doc id = doc -> id

type FieldMap key id= M.Map key (S.Set id)

type DocIndexMap field key id = M.Map field (FieldMap key id)

data DocIndex doc id field key =
  DocIndex { extractId :: ExtractId doc id
           , fields    :: [Field doc field key]
           , store     :: !(M.Map id doc)
           , index     :: !(DocIndexMap field key id)
           }


init :: (Ord field, Ord id, Ord key) => 
        ExtractId doc id ->
        [Field doc field key] ->
        DocIndex doc id field key
init ei fds = DocIndex ei fds M.empty M.empty


findDoc :: (Ord field, Ord id, Ord key) =>
        DocIndex doc id field key ->
        id ->
        Maybe doc
findDoc docIdx id = M.lookup id (store docIdx)


documentsList :: DocIndex doc id field key -> [doc]
documentsList docIdx = M.elems $ store docIdx

insert :: (Ord field, Ord id, Ord key) =>
          DocIndex doc id field key ->
          doc ->
          DocIndex doc id field key
insert !docIdx' doc =
  let !id = (extractId docIdx') doc
      !docIdx = case findDoc docIdx' id of
        Nothing -> docIdx'
        Just _ -> remove docIdx' id
      !newStore = M.insert id doc (store docIdx)
      !newDocIndex = makeFieldsKeysMap (fields docIdx) doc id
      !newIndex = M.unionWith (M.unionWith S.union) (index docIdx) newDocIndex
  in docIdx { store = newStore, index = newIndex }


insertMany :: (Ord field, Ord id, Ord key) =>
              DocIndex doc id field key ->
              [doc] ->
              DocIndex doc id field key
insertMany !docIdx docs = foldl' insert docIdx docs




remove :: (Ord field, Ord id, Ord key) =>
          DocIndex doc id field key ->
          id ->
          DocIndex doc id field key
remove !docIdx id =
  case findDoc docIdx id of
    Nothing -> docIdx
    Just doc -> 
      let !newStore = M.delete id (store docIdx)
          !docDocIndex = makeFieldsKeysMap (fields docIdx) doc id
          !newIndex = M.differenceWith diffFieldMaps (index docIdx) docDocIndex
      in docIdx { store = newStore, index = newIndex }
  where
    diffFieldMaps !a !b = Just $ M.differenceWith diffSets a b
    diffSets !a !b = let newSet = S.difference a b in
      if S.null newSet then Nothing
      else Just newSet


removeMany :: (Ord field, Ord id, Ord key) =>
          DocIndex doc id field key ->
          [id] ->
          DocIndex doc id field key
removeMany !docIdx docs = foldl' remove docIdx docs
          


makeFieldsKeysMap :: (Ord field, Ord id, Ord key) =>
                     [Field doc field key] ->
                     doc ->
                     id  ->
                     DocIndexMap field key id
makeFieldsKeysMap flds doc id = M.fromList $
                                map (\f -> (fieldId f, fieldKeysMap f doc id)) $
                                flds
  where
    fieldKeysMap :: (Ord key) =>
                    Field doc field key ->
                    doc -> id ->
                    FieldMap key id
    fieldKeysMap fld doc' id' = M.fromList $
                                map (\k -> (k, S.singleton id')) $
                                (fieldExtractKeys fld) doc'


narrowOnSet :: (Ord field, Ord id, Ord key) =>
               DocIndex doc id field key ->
               S.Set id ->
               DocIndex doc id field key
narrowOnSet docIdx ids =
  let !newDocs = M.elems $ M.restrictKeys (store docIdx) ids 
      !newDocIdx = init (extractId docIdx) (fields docIdx)
  in insertMany newDocIdx newDocs


-- Query

data Query field key = And [Query field key]
                     | Or [Query field key]
                     | Only field key
                     | All field (S.Set key)
                     | Any field (S.Set key)
                     | HasField field
                     | Range field key key
                     | GreaterThanOrEq field key
                     | LowerThanOrEq field key



query :: (Ord field, Ord id, Ord key) =>
         DocIndex doc id field key ->
         Query field key ->
         [doc]
query docIdx q = let !ids = queryIdSet docIdx q
  in catMaybes $ map (\id -> M.lookup id (store docIdx)) (S.toList ids)


narrow :: (Ord field, Ord id, Ord key) =>
          DocIndex doc id field key ->
          Query field key ->
          DocIndex doc id field key
narrow docIdx q = let ids = queryIdSet docIdx q
  in narrowOnSet docIdx ids


queryIdSet :: (Ord field, Ord id, Ord key) =>
              DocIndex doc id field key ->
              Query field key ->
              S.Set id
queryIdSet !docIdx q = case q of
  And l -> runAnd l
  Or  l -> runOr l
  Only f k -> lookupIndex docIdx f k
  All f ks -> runAll f ks
  Any f ks -> runAny f ks
  HasField f -> runHasField f
  Range f k1 k2 -> runRange f k1 k2
  GreaterThanOrEq f k -> runGTE f k
  LowerThanOrEq f k -> runLTE f k
  where
    runAnd [] = S.empty
    runAnd (last:[]) = queryIdSet docIdx last
    runAnd (h:tl) = S.intersection (queryIdSet docIdx h) (runAnd tl)

    runOr [] = S.empty
    runOr (last:[]) = queryIdSet docIdx last
    runOr (h:tl) = S.union (queryIdSet docIdx h) (runOr tl)

    runAll f ks = case elemsAtFieldKeys (index docIdx) f ks of
                    [] -> S.empty
                    sets -> foldl1' S.intersection sets

    runAny f ks = S.unions $ elemsAtFieldKeys (index docIdx) f ks

    runHasField f = S.unions $ M.elems $ lookupFieldMap (index docIdx) f

    runRange f k1 k2 =
      let fieldMap = lookupFieldMap (index docIdx) f
          allKeys = M.keysSet fieldMap
          (_,keysHigh) = splitInclude k1 allKeys
          (ks,_) = splitInclude k2 keysHigh
      in runAny f ks

    runGTE f k = let (_,ks) = splitInclude k $ M.keysSet $
                              lookupFieldMap (index docIdx) f
                 in runAny f ks

    runLTE f k = let (ks,_) = splitInclude k $ M.keysSet $
                              lookupFieldMap (index docIdx) f
                 in runAny f ks



-- normalizeQuery :: Query -> Query
-- normalizeQuery q = case q of
--   And qs -> And (joinQS qs)
--   Or  qs -> Or (joinQS qs)
--   x -> x


lookupFieldMap :: (Ord field, Ord id, Ord key) =>
                  DocIndexMap field key id ->
                  field -> 
                  FieldMap key id
lookupFieldMap idx f = M.findWithDefault M.empty f idx


elemsAtFieldKeys :: (Ord field, Ord id, Ord key) =>
                    DocIndexMap field key id ->
                    field -> S.Set key ->
                    [S.Set id]
elemsAtFieldKeys idx f keys = M.elems $
                              M.restrictKeys (lookupFieldMap idx f) keys


lookupIndex :: (Ord field, Ord id, Ord key) =>
        DocIndex doc id field key ->
        field ->
        key ->
        S.Set id
lookupIndex docIdx f key = M.findWithDefault (S.empty) key $
                           M.findWithDefault M.empty f $
                           index docIdx

getFields :: (Ord field) =>
             DocIndex doc id field key ->
             [field]
getFields docIdx = M.keys (index docIdx)


getFieldKeys :: (Ord field, Ord id, Ord key) =>
                DocIndex doc id field key ->
                field ->
                [key]
getFieldKeys docIdx f = M.keys $ lookupFieldMap (index docIdx) f


cardinal :: DocIndex doc id field key ->
            Int
cardinal docIdx = M.size $ store docIdx

nodup :: Ord a => [a] -> [a]
nodup = S.toList . S.fromList
    

splitInclude :: (Ord a) => a -> S.Set a -> (S.Set a, S.Set a)
splitInclude n s = case S.splitMember n s of
  (l,True,h) -> (S.insert n l, S.insert n h)
  (l,False,h) -> (l, h)


