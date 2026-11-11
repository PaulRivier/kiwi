-- | Requests of the browse page, and their URLs
module Kiwi.Browse
  ( BrowseAllReq
  , Combination (..)
  , fieldCombination
  , reqToQuery
  , sourcesReq
  , createReq  -- exported for tests
  , addToReq
  , removeFromReq
  , browseUrl
  , decodeReq
  , showKeyLabel
  ) where

import           Control.Monad (foldM)
import qualified Data.List as L
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Time as Time

import           Kiwi.Tags (tagToSegments, segmentsToTag)
import           Kiwi.Types
import           Kiwi.Url (contextParams, renderParams)
import           Kiwi.Utils (readCustomValue)
import qualified Utils.DocIndex as DI


-- | Filter of the browse page: the keys of each field are combined according
-- to 'fieldCombination', and the pages must match every field. In URLs, it is
-- a query string with one parameter per key, e.g.
-- @/browse?source=s&tag=a%3Eb&tag=c&m.author=Smith@: "source", "tag",
-- "access", "lang", and "m." followed by the name of a custom metadata field.
type BrowseAllReq = M.Map MetaField (S.Set DocIndexKey)

-- | How the keys of a field are combined in a request
data Combination = Intersection  -- ^ the pages have all the keys
                 | Union         -- ^ the pages have one of the keys

-- | Tags and custom fields narrow the selection, sources are alternatives
fieldCombination :: MetaField -> Combination
fieldCombination FieldSource = Union
fieldCombination _ = Intersection

reqToQuery :: BrowseAllReq -> DI.Query MetaField DocIndexKey
reqToQuery r = DI.And [ combine (fieldCombination f) f ks | (f, ks) <- M.toList r ]
  where combine Intersection = DI.All
        combine Union = DI.Any

-- | Request with only the sources of the context, where the navigation starts
sourcesReq :: S.Set T.Text -> BrowseAllReq
sourcesReq ss
  | S.null ss = M.empty
  | otherwise = M.singleton FieldSource (S.map KeyText ss)

createReq :: (MetaField, DocIndexKey) -> BrowseAllReq
createReq = addToReq M.empty

-- | Add a key to a request. The parents of a new tag are removed, since
-- pages are indexed with all the parents of their tags.
addToReq :: BrowseAllReq -> (MetaField, DocIndexKey) -> BrowseAllReq
addToReq r (f,k) = M.insertWith S.union f (S.singleton k) $ case (f,k) of
  (FieldTag, KeyText t) -> M.adjust (S.filter (not . isParentOf t)) FieldTag r
  _ -> r
  where
    isParentOf t (KeyText p) = let ps = tagToSegments p
                                   ts = tagToSegments t
                               in ps /= ts && L.isPrefixOf ps ts
    isParentOf _ _ = False

removeFromReq :: BrowseAllReq -> (MetaField, DocIndexKey) -> BrowseAllReq
removeFromReq r (f,k) = M.update remove f r
  where remove ks = let ks' = S.delete k ks
                    in if S.null ks' then Nothing else Just ks'

-- | Link to the browse page of a request, in the navigation context. On the
-- browse page, the sources context is part of the request: the sources of the
-- request replace those of the context. Fields and keys are always in the
-- same order, so a request has only one URL.
browseUrl :: Context -> BrowseAllReq -> T.Text
browseUrl ctx r = T.append "/browse" $ renderParams $
  [ (n, v) | (f, ks) <- M.toList r, Just n <- [fieldParam f]
           , k <- S.toList ks, Just v <- [keyParam k] ]
  ++ contextParams ctx { contextSources = S.empty }
  where
    fieldParam FieldSource = Just "source"
    fieldParam FieldTag = Just "tag"
    fieldParam FieldAccess = Just "access"
    fieldParam FieldLang = Just "lang"
    fieldParam (FieldCustom n) = Just $ T.append "m." n
    fieldParam _ = Nothing
    keyParam (KeyText t) = Just t
    keyParam (KeyInt i) = Just $ T.pack $ show i
    keyParam (KeyDay d) = Just $ T.pack $ Time.showGregorian d
    keyParam (KeyBool b) = Just $ if b then "true" else "false"
    keyParam (KeyLang l) = Just $ T.toLower $ T.pack $ show l
    keyParam _ = Nothing

-- | Decode the parameters of a browse URL (see 'BrowseAllReq'). Custom
-- fields must be in the config, and their keys are read according to its
-- type. Unknown parameters and invalid keys are errors.
decodeReq :: [CustomMetaConfig] -> [(T.Text, T.Text)] -> Either T.Text BrowseAllReq
decodeReq cmcs = foldM addParam M.empty
  where
    addParam r (n, v) = do
      (f, readKey) <- maybe (Left $ T.append "unknown parameter: " n) Right $
                      fieldReader n
      k <- maybe (Left $ T.concat ["invalid value for ", n, ": ", v]) Right $
           readKey v
      return $ addToReq r (f, k)

    fieldReader "source" = Just (FieldSource, readText)
    fieldReader "tag" = Just (FieldTag, readText . segmentsToTag . tagToSegments)
    fieldReader "access" = Just (FieldAccess, readText)
    fieldReader "lang" = Just (FieldLang, either (const Nothing) (Just . KeyLang) . parseLang)
    fieldReader n = do
      name <- T.stripPrefix "m." n
      cmc <- L.find ((== name) . cmcName) cmcs
      return (FieldCustom name, nonEmpty $ readCustomValue $ cmcType cmc)

    readText t = if T.null t then Nothing else Just $ KeyText t

    -- the keys of custom fields are read as in the pages (see
    -- 'readCustomValue'), an empty one is invalid
    nonEmpty f t = if T.null t then Nothing else f t

    -- readCustom CmtText = readText
    -- readCustom CmtInt = fmap KeyInt . readMaybe . T.unpack
    -- readCustom CmtDate = fmap KeyDay .
    --   Time.parseTimeM False Time.defaultTimeLocale "%Y-%-m-%-d" . T.unpack
    -- readCustom CmtBool = \b -> case b of
    --   "true" -> Just $ KeyBool True
    --   "false" -> Just $ KeyBool False
    --   _ -> Nothing

showKeyLabel :: DocIndexKey -> T.Text
showKeyLabel k = case k of
  KeyText t -> t
  KeyInt i -> T.pack $ show i
  KeyDay d -> T.pack $ show d
  KeyBool b -> T.pack $ show b
  KeyLang l -> T.pack $ show l
  KeyPairText p -> T.pack $ show p
