module Kiwi.TagsSpec (tagsSpec) where

import           Test.Hspec

import qualified Kiwi.Tags as Tags

tagsSpec :: Spec
tagsSpec = do
  it "splits metadata on commas, with escaping" $
    Tags.splitMeta "a, b\\, c, ,d" `shouldBe` ["a", "b, c", "d"]

  it "splits on several escaped separators" $
    Tags.splitMeta "a\\,b\\,c, d" `shouldBe` ["a,b,c", "d"]

  it "keeps other backslashes" $
    Tags.splitMeta "a\\b, c\\" `shouldBe` ["a\\b", "c\\"]

  it "splits tags without groups as metadata" $
    Tags.splitTags "a, b > c\\, d, ,e \\> f" `shouldBe` ["a", "b > c, d", "e \\> f"]

  it "expands groups of tags" $ do
    Tags.splitTags "sport > {alpinisme, escalade,rando }"
      `shouldBe` ["sport > alpinisme", "sport > escalade", "sport > rando"]
    Tags.splitTags "{sport, loisir} > montagne, a"
      `shouldBe` ["sport > montagne", "loisir > montagne", "a"]

  it "expands nested groups" $
    Tags.splitTags "sport > {montagne > {alpinisme, rando}, natation}"
      `shouldBe` [ "sport > montagne > alpinisme", "sport > montagne > rando"
                 , "sport > natation" ]

  it "drops empty groups and blank tags of groups" $
    Tags.splitTags "sport > {}, a > {b, }, c" `shouldBe` ["a > b", "c"]

  it "keeps escaped braces in tags" $
    Tags.splitTags "a\\{b\\}, c > {d\\,e}" `shouldBe` ["a{b}", "c > d,e"]

  it "takes unbalanced braces literally" $ do
    Tags.splitTags "sport > {a, b" `shouldBe` ["sport > {a", "b"]
    Tags.splitTags "a}, b" `shouldBe` ["a}", "b"]

  it "splits tags in segments" $
    Tags.tagToSegments "loisir > sport>escalade" `shouldBe` ["loisir", "sport", "escalade"]

  it "computes tag sub segments" $
    Tags.tagSubSegments "loisir>sport>escalade"
      `shouldBe` [["loisir"], ["loisir", "sport"], ["loisir", "sport", "escalade"]]

  it "roundtrips segments with escaped >" $ do
    Tags.segmentsToTag ["a>b", "c"] `shouldBe` "a\\>b>c"
    Tags.tagToSegments (Tags.segmentsToTag ["a>b", "c"]) `shouldBe` ["a>b", "c"]

  it "prefixes and normalizes tags" $
    Tags.prefixNormalizeTag ["root"] "a > b" `shouldBe` "root>a>b"

  it "pretty prints tags" $
    Tags.prettyTag "a>b" `shouldBe` "a > b"

  it "keeps only root tags" $
    Tags.filterRootTags [["a"], ["a", "b"], ["c", "d"], ["c", "d", "e"]]
      `shouldBe` [["a"], ["c", "d"]]

