{-# LANGUAGE OverloadedStrings #-}
module Kiwi.KeywordsSpec (keywordsSpec) where

import qualified Data.Map.Strict as M
import qualified Data.Text as T
import           Test.Hspec

import           Kiwi.Keywords (bestTerms, emptyKeywords, maxKeywords, pagesKeywords, updateKeywords)
import           Kiwi.Types
import           TestUtils (mkPage)

-- | A page of the source "s" in a lang, with its title, plain text and
-- headings
textPage :: T.Text -> Lang -> T.Text -> T.Text -> [T.Text] -> Page
textPage uid lg title text headings =
  (mkPage ("s", uid) [] [] False 1) { pageTitle = title, pagePlainText = text
                                    , pageHeadings = headings, pageLang = lg }

keywordsSpec :: Spec
keywordsSpec = do
  it "weighs the terms of a page by their occurrences and their rarity in the wiki" $ do
    -- 10 pages: (term, number of pages)
    let docs = M.fromList [ ("menth", 1), ("glac", 2), ("beaucoup", 1), ("350g", 1)
                          , ("la", 9), ("sucr", 3), ("titr", 2) ]
        weights = M.fromList
          [ ("menth", 3)       -- (1 + log 3) × log 10: the best
          , ("glac", 5)
          , ("beaucoup", 1)    -- seen once: left out
          , ("350g", 3)        -- a quantity: left out
          , ("la", 4)          -- in most pages: under the threshold
          , ("sucr", 2)
          , ("titr", 3) ]      -- once, but also in a heading
    bestTerms 10 docs weights `shouldBe` ["menth", "glac", "titr", "sucr"]

  it "gives at most maxKeywords terms, by text on a tie, none for a page without terms" $ do
    let same = M.fromList [ (T.pack [c], 2) | c <- reverse ['a' .. 'z'] ]
        docs = M.map (const 1) same
    bestTerms 10 docs same `shouldBe` map (T.pack . (: [])) (take maxKeywords ['a' .. 'z'])
    bestTerms 10 M.empty M.empty `shouldBe` []
    bestTerms 10 (M.fromList [("x", 1)]) (M.fromList [("x", 1)]) `shouldBe` []

  it "leaves out the stop words of the lang of the page, and gives the words as written" $ do
    let fr = textPage "fr" French "Glaces"
               "Cependant la glace à la menthe, toutefois la glace et les Glaces. Menthe. The the the."
               []
        en = textPage "en" English "Soups"
               "However the soup, the carrot soup. Carrots and soups, however."
               []
        others = [ textPage (T.pack (show i)) English "Other" "Something else entirely" [] | i <- [1 .. 8 :: Int] ]
        kws = M.map (map keywordWord) $ pagesKeywords (fr : en : others)
    -- French stop words out, English ones kept in a French page; "glace" and
    -- "the" are the most frequent forms, "Menthe" the first in order of a tie
    M.lookup ("s", "fr") kws `shouldBe` Just ["glace", "the", "Menthe"]
    M.lookup ("s", "en") kws `shouldBe` Just ["soup", "carrot"]

  it "weighs the title and the headings, the words taken from the content" $ do
    let p = textPage "p" English "Notes" "rails and ruby, rails" ["Ruby"]
        others = [ textPage (T.pack (show i)) English "Other" "Something else entirely" [] | i <- [1 .. 8 :: Int] ]
    fmap (map keywordWord) (M.lookup ("s", "p") (pagesKeywords (p : others))) `shouldBe` Just ["Notes", "ruby", "rails"]

  it "weighs the summary, its words written as in the content" $ do
    let p = (textPage "p" English "Notes" "rails and ruby, ruby rails, gems" [])
              { pageSummary = Just "About Ruby" }
        others = [ textPage (T.pack (show i)) English "Other" "Something else entirely" [] | i <- [1 .. 8 :: Int] ]
    -- "ruby": 2 in the content, 2 in the summary, over the title (3) and
    -- "rails" (2); written as in the content, more often than "Ruby"
    fmap (map keywordWord) (M.lookup ("s", "p") (pagesKeywords (p : others))) `shouldBe` Just ["ruby", "Notes", "rails"]

  it "updates the number of pages of each term as a computation of every page would" $ do
    let (kw0, full0) = updateKeywords pages0 [] pages0 emptyKeywords
        -- page 3 changed, page 4 removed, page 31 added
        p3 = numbered 3 "bananas bananas cherries"
        p31 = numbered 31 "cherries cherries apples"
        old3 = pages0 !! 2
        old4 = pages0 !! 3
        pages1 = p3 : p31 : drop 4 pages0 ++ take 2 pages0
        (kw1, full1) = updateKeywords pages1 [old3, old4] [p3, p31] kw0
    (full0, kwChanges kw0, kwFullAt kw0) `shouldBe` (True, 30, 30)
    (full1, kwChanges kw1, kwFullAt kw1) `shouldBe` (False, 33, 30)
    kwDocs kw1 `shouldBe` kwDocs (fst $ updateKeywords pages1 [] pages1 emptyKeywords)
    M.lookup (pageUID p3) (kwPages kw1) `shouldBe` M.lookup (pageUID p3) (pagesKeywords pages1)
    M.lookup (pageUID p31) (kwPages kw1) `shouldBe` M.lookup (pageUID p31) (pagesKeywords pages1)
    M.member (pageUID old4) (kwPages kw1) `shouldBe` False
    M.size (kwPages kw1) `shouldBe` 30

  it "computes every page again once the changed pages reach a tenth of them, 10 at least" $ do
    let kw0 = fst $ updateKeywords pages0 [] pages0 emptyKeywords
        changed k = [ numbered i "cherries cherries plums" | i <- [1 .. k] ]
        pagesAfter k = changed k ++ drop k pages0
        update k = updateKeywords (pagesAfter k) (take k pages0) (changed k) kw0
        (kw9, full9) = update 9
        (kw10, full10) = update 10
    (full9, kwChanges kw9, kwFullAt kw9) `shouldBe` (False, 39, 30)
    (full10, kwChanges kw10, kwFullAt kw10) `shouldBe` (True, 40, 40)
    kwPages kw10 `shouldBe` pagesKeywords (pagesAfter 10)
    -- the next update counts from there
    let (_, full) = updateKeywords (pagesAfter 10) (changed 1) [numbered 1 "plums"] kw10
    full `shouldBe` False
  where
    numbered :: Int -> T.Text -> Page
    numbered i text = textPage (T.pack (show i)) English ("Page " <> T.pack (show i)) text []
    -- 30 pages
    pages0 = [ numbered i ("apples pears " <> T.replicate i "kiwis ") | i <- [1 .. 30] ]
