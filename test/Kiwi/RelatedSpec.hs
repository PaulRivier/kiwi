{-# LANGUAGE OverloadedStrings #-}
module Kiwi.RelatedSpec (relatedSpec) where

import qualified Data.Map.Strict as M
import qualified Data.Text as T
import           Test.Hspec

import           Kiwi.Related (maxRelated, relatedIndexOf, relatedPages)
import           Kiwi.Types
import           TestUtils (mkPage)

-- | A page of the source "s" with its tags and custom metadata
tagged :: T.Text -> [TagId] -> [CustomMetaData] -> Page
tagged uid tags custom = (mkPage ("s", uid) tags [] False 1) { pageCustomMeta = custom }

-- | The uids of the pages related to a page, with these keywords
related :: [CustomMetaConfig] -> M.Map PageUID [Keyword] -> Page -> [Page] -> [T.Text]
related cfg kws p pages =
  map (snd . pageUID) $ relatedPages (relatedIndexOf cfg kws (p : pages)) (`M.lookup` byUid) p
  where byUid = M.fromList [ (pageUID q, q) | q <- pages ]

relatedSpec :: Spec
relatedSpec = do
  it "brings the pages with the same tags first, then those with part of them" $ do
    let p = tagged "p" ["cuisine > dessert"] []
        same = tagged "same" ["cuisine > dessert"] []
        part = tagged "part" ["cuisine > plat"] []
        other = tagged "other" ["jardin"] []
    related [] M.empty p [p, other, part, same] `shouldBe` ["same", "part"]

  it "brings the pages with common keywords, by their stem" $ do
    let kw s = Keyword s s
        kws = M.fromList [ (("s", "p"), [kw "recet", kw "menth"])
                         , (("s", "q"), [Keyword "recet" "recettes", kw "menth"])
                         , (("s", "r"), [kw "jardin", kw "menth", kw "arros", kw "pomm"]) ]
        page uid = tagged uid [] []
    related [] kws (page "p") (map page ["p", "q", "r"]) `shouldBe` ["q", "r"]

  it "brings the pages with the same value of a relating custom field only" $ do
    let author = CustomMetaData "author" [KeyText "Dupont"]
        p = tagged "p" [] [author]
        q = tagged "q" [] [author]
        cfg relating = [CustomMetaConfig "author" CmtText 1 Nothing relating]
    related (cfg True) M.empty p [p, q] `shouldBe` ["q"]
    related (cfg False) M.empty p [p, q] `shouldBe` []

  it "keeps at most maxRelated pages, the closest first, and not the far ones" $ do
    let p = tagged "p" ["a", "b", "c", "d", "e"] []
        -- page i has the i first tags of p
        near = [ tagged (T.pack $ show i) (take i ["a", "b", "c", "d", "e"]) [] | i <- [1 .. 5 :: Int] ]
        far = tagged "far" ["a", "x", "y", "z", "w", "v", "u", "t", "s", "r"] []
    related [] M.empty p (far : near ++ map (\q -> q { pageUID = ("t", snd $ pageUID q) }) near)
      `shouldBe` ["5", "5", "4", "4", "3"]
    length (related [] M.empty p (far : near)) `shouldBe` min maxRelated 5
    related [] M.empty p [far] `shouldBe` []
