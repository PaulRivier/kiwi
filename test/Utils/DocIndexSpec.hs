module Utils.DocIndexSpec (docIndexSpec) where

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T
import           Test.Hspec
import           Test.Hspec.QuickCheck (prop)
import           Test.QuickCheck

import           Kiwi.Types
import qualified Utils.DocIndex as DI

-- | (id, tags, year)
type Doc = (Int, [T.Text], Int)

type DocQuery = DI.Query String DocIndexKey

docId :: Doc -> Int
docId (i, _, _) = i

-- | Keys of the fields "tag" and "year", none for the other fields
docKeys :: String -> Doc -> [DocIndexKey]
docKeys "tag" (_, ts, _) = map KeyText ts
docKeys "year" (_, _, y) = [KeyInt y]
docKeys _ _ = []

docsIndex :: [Doc] -> DI.DocIndex Doc Int String DocIndexKey
docsIndex = DI.insertMany $ DI.empty docId
  [ DI.Field "tag" (docKeys "tag"), DI.Field "year" (docKeys "year") ]

-- | Reference semantics of a query, on a single document
matches :: DocQuery -> Doc -> Bool
matches q d = case q of
  DI.And [] -> False
  DI.And qs -> all (`matches` d) qs
  DI.Or qs -> any (`matches` d) qs
  DI.Only f k -> k `elem` docKeys f d
  DI.All f ks -> not (S.null ks) && all (`elem` docKeys f d) ks
  DI.Any f ks -> any (`elem` docKeys f d) ks
  DI.HasField f -> not $ null $ docKeys f d
  DI.Range f k1 k2 -> any (\k -> k1 <= k && k <= k2) $ docKeys f d
  DI.GreaterThanOrEq f k -> any (>= k) $ docKeys f d
  DI.LowerThanOrEq f k -> any (<= k) $ docKeys f d

-- | Documents with few ids and keys, so that they often share them, and with
-- the same id sometimes
genDocs :: Gen [Doc]
genDocs = listOf $ (,,) <$> choose (1, 12) <*> listOf genTag <*> choose (2018, 2023)

genTag :: Gen T.Text
genTag = elements ["a", "b", "c", "d"]

-- | Queries on the fields of the documents, an unknown field, an unknown tag
-- ("z") and years without documents
genQuery :: Int -> Gen DocQuery
genQuery n = oneof $ leaves ++ if n > 0 then [ DI.And <$> subQueries
                                              , DI.Or <$> subQueries ] else []
  where
    subQueries = resize 3 $ listOf $ genQuery (n `div` 2)
    tag = KeyText <$> oneof [genTag, pure "z"]
    year = KeyInt <$> choose (2017, 2024)
    leaves = [ DI.Only "tag" <$> tag
             , DI.Only "year" <$> year
             , DI.All "tag" . S.fromList <$> resize 3 (listOf tag)
             , DI.Any "tag" . S.fromList <$> resize 3 (listOf tag)
             , DI.HasField <$> elements ["tag", "year", "none"]
             , DI.Range "year" <$> year <*> year
             , DI.GreaterThanOrEq "year" <$> year
             , DI.LowerThanOrEq "year" <$> year ]

-- | The documents of the index built from these ones: the last one with each id
storedDocs :: [Doc] -> [Doc]
storedDocs ds = M.elems $ M.fromList [ (docId d, d) | d <- ds ]

-- | Same documents and same index (the index only depends on the documents)
sameIndex :: DI.DocIndex Doc Int String DocIndexKey -> [Doc] -> Property
sameIndex idx ds = (DI.documentsList idx, DI.index idx)
                   === (ds, DI.index $ docsIndex ds)

sampleDocs :: [Doc]
sampleDocs = [ (1, ["a", "b"], 2019)
             , (2, ["a"], 2020)
             , (3, ["c"], 2021)
             , (4, [], 2022) ]

ids :: DI.DocIndex Doc Int String DocIndexKey -> DI.Query String DocIndexKey -> [Int]
ids idx q = S.toList $ DI.queryIdSet idx q

docIndexSpec :: Spec
docIndexSpec = do
  let idx = docsIndex sampleDocs
      tag t = DI.Only "tag" (KeyText t)
      year y = KeyInt y

  it "stores and finds documents" $ do
    DI.cardinal idx `shouldBe` 4
    DI.findDoc idx 2 `shouldBe` Just (2, ["a"], 2020)
    DI.findDoc idx 5 `shouldBe` Nothing

  it "indexes field keys" $
    DI.getFieldKeys idx "tag" `shouldBe` map KeyText ["a", "b", "c"]

  it "runs And / Or / All / Any queries" $ do
    ids idx (tag "a") `shouldBe` [1, 2]
    ids idx (DI.And [tag "a", tag "b"]) `shouldBe` [1]
    ids idx (DI.Or [tag "b", tag "c"]) `shouldBe` [1, 3]
    ids idx (DI.All "tag" $ S.fromList [KeyText "a", KeyText "b"]) `shouldBe` [1]
    ids idx (DI.Any "tag" $ S.fromList [KeyText "b", KeyText "c"]) `shouldBe` [1, 3]
    ids idx (DI.HasField "tag") `shouldBe` [1, 2, 3]
    ids idx (DI.And []) `shouldBe` []
    ids idx (DI.All "tag" S.empty) `shouldBe` []

  it "matches no document with All and a missing key" $ do
    ids idx (DI.All "tag" $ S.fromList [KeyText "a", KeyText "z"]) `shouldBe` []
    ids idx (DI.Any "tag" $ S.fromList [KeyText "a", KeyText "z"]) `shouldBe` [1, 2]

  it "returns the documents by id" $
    DI.query idx (DI.Or [tag "c", tag "a"]) `shouldBe`
      [(1, ["a", "b"], 2019), (2, ["a"], 2020), (3, ["c"], 2021)]

  it "runs range queries, bounds included" $ do
    ids idx (DI.Range "year" (year 2020) (year 2021)) `shouldBe` [2, 3]
    ids idx (DI.Range "year" (year 2019) (year 2019)) `shouldBe` [1]
    ids idx (DI.GreaterThanOrEq "year" (year 2021)) `shouldBe` [3, 4]
    ids idx (DI.LowerThanOrEq "year" (year 2020)) `shouldBe` [1, 2]
    ids idx (DI.Range "year" (year 2021) (year 2020)) `shouldBe` []
    ids idx (DI.Range "none" (year 2019) (year 2022)) `shouldBe` []

  it "removes documents and their keys" $ do
    let idx' = DI.removeMany idx [1, 3, 5]
    DI.cardinal idx' `shouldBe` 2
    ids idx' (tag "a") `shouldBe` [2]
    DI.getFieldKeys idx' "tag" `shouldBe` [KeyText "a"]

  it "removes the fields without keys" $ do
    DI.getFields (DI.removeMany idx [1, 2, 3]) `shouldBe` ["year"]
    DI.getFields (DI.narrow idx $ DI.Only "year" $ year 2022) `shouldBe` ["year"]

  it "replaces a document inserted twice" $ do
    let idx' = DI.insert idx (2, ["z"], 2020)
    DI.cardinal idx' `shouldBe` 4
    ids idx' (tag "a") `shouldBe` [1]
    ids idx' (tag "z") `shouldBe` [2]

  it "narrows an index to a query" $ do
    let idx' = DI.narrow idx (tag "a")
    DI.cardinal idx' `shouldBe` 2
    DI.getFieldKeys idx' "tag" `shouldBe` map KeyText ["a", "b"]
    ids idx' (DI.HasField "year") `shouldBe` [1, 2]

  prop "runs queries as the reference semantics" $
    forAll genDocs $ \ds -> forAll (genQuery 4) $ \q ->
      ids (docsIndex ds) q === [ docId d | d <- storedDocs ds, matches q d ]

  prop "narrows as if the matching documents were indexed again" $
    forAll genDocs $ \ds -> forAll (genQuery 4) $ \q ->
      sameIndex (DI.narrow (docsIndex ds) q) (filter (matches q) $ storedDocs ds)

  prop "removes as if the other documents were indexed again" $
    forAll genDocs $ \ds -> forAll (listOf $ choose (1, 12)) $ \removed ->
      sameIndex (DI.removeMany (docsIndex ds) removed)
                [ d | d <- storedDocs ds, docId d `notElem` removed ]

