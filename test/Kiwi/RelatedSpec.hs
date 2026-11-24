{-# LANGUAGE OverloadedStrings #-}
module Kiwi.RelatedSpec (relatedSpec) where

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T
import           Test.Hspec

import           Kiwi.PagesDB (setKeywords)
import           Kiwi.Related (maxRelated, relatedPages)
import           Kiwi.Types
import qualified Utils.DocIndex.Restricted as RDI
import           TestUtils (mkPage, mkPagesDBWith)

-- | A page of the source "s" with its tags and custom metadata
tagged :: T.Text -> [TagId] -> [CustomMetaData] -> Page
tagged uid tags custom = (mkPage ("s", uid) tags [] False 1) { pageCustomMeta = custom }

-- | The index of these pages with these keywords (none for the others), as
-- after a load
indexed :: [CustomMetaConfig] -> M.Map PageUID [Keyword] -> [Page]
        -> RDI.RestrictedDocIndex Page PageUID MetaField DocIndexKey
indexed cfg kws pages =
  RDI.whole $ setKeywords cfg (RDI.visibleIndex $ pagesIndex $ mkPagesDBWith cfg pages) $
    M.union kws (M.fromList [ (pageUID q, []) | q <- pages ])

-- | The uids of the pages related to a page, with these keywords
related :: [CustomMetaConfig] -> M.Map PageUID [Keyword] -> Page -> [Page] -> [T.Text]
related cfg kws p pages = case RDI.findDoc idx (pageUID p) of
  Just p' -> map (snd . pageUID) $ relatedPages cfg idx (const True) p'
  Nothing -> error "the page is not indexed"
  where idx = indexed cfg kws (p : pages)

relatedSpec :: Spec
relatedSpec = do
  it "gives each page the sum of the squares of the weights of its features" $ do
    let author = CustomMetaData "author" [KeyText "Dupont"]
        p = tagged "p" ["a > b"] [author]
        cfg = [CustomMetaConfig "author" CmtText 1 Nothing 2]
        kws = M.fromList [ (("s", "p"), [Keyword "recet" "recette", Keyword "menth" "menthe"]) ]
        norm = fmap pageRelatedNorm . (`RDI.findDoc` ("s", "p"))
    -- tags "a" and "a > b" (5), two keywords (3), the author (2)
    norm (indexed cfg kws [p]) `shouldBe` Just (2 * 25 + 2 * 9 + 4)
    norm (indexed [] kws [p]) `shouldBe` Just (2 * 25 + 2 * 9)
    fmap (map keywordWord . pageKeywords) (RDI.findDoc (indexed [] kws [p]) ("s", "p"))
      `shouldBe` Just ["recette", "menthe"]

  it "keeps only the visible pages of the index that the predicate keeps" $ do
    let idx = indexed [] M.empty [tagged u ["a"] [] | u <- ["p", "q", "r"]]
        uids = map (snd . pageUID)
    case RDI.findDoc idx ("s", "p") of
      Nothing -> expectationFailure "the page is not indexed"
      Just p -> do
        uids (relatedPages [] idx (const True) p) `shouldBe` ["q", "r"]
        uids (relatedPages [] idx (/= ("s", "q")) p) `shouldBe` ["r"]
        -- "r" is not visible
        uids (relatedPages [] (RDI.narrowOnSet idx $ S.fromList [("s", "p"), ("s", "q")]) (const True) p)
          `shouldBe` ["q"]

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

  it "brings the pages with the same value of a relating custom field only, by its weight" $ do
    let author = CustomMetaData "author" [KeyText "Dupont"]
        p = tagged "p" ["a", "b"] [author]
        q = tagged "q" [] [author]
        r = tagged "r" ["a", "b"] []
        cfg w = [CustomMetaConfig "author" CmtText 1 Nothing w]
    -- the tags weigh 5
    related (cfg 3) M.empty p [p, q, r] `shouldBe` ["r", "q"]
    related (cfg 10) M.empty p [p, q, r] `shouldBe` ["q", "r"]
    related (cfg 0) M.empty p [p, q, r] `shouldBe` ["r"]

  it "keeps at most maxRelated pages, the closest first, and not the far ones" $ do
    let p = tagged "p" ["a", "b", "c", "d", "e"] []
        -- page i has the i first tags of p
        near = [ tagged (T.pack $ show i) (take i ["a", "b", "c", "d", "e"]) [] | i <- [1 .. 5 :: Int] ]
        far = tagged "far" ["a", "x", "y", "z", "w", "v", "u", "t", "s", "r"] []
    related [] M.empty p (far : near ++ map (\q -> q { pageUID = ("t", snd $ pageUID q) }) near)
      `shouldBe` ["5", "5", "4", "4", "3"]
    length (related [] M.empty p (far : near)) `shouldBe` min maxRelated 5
    related [] M.empty p [far] `shouldBe` []
