{-# LANGUAGE OverloadedStrings #-}
module Kiwi.KeywordsSpec (keywordsSpec) where

import qualified Data.Map.Strict as M
import qualified Data.Text as T
import           Test.Hspec

import           Kiwi.Keywords (bestTerms, maxKeywords, pagesKeywords)
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

  it "gives at most 10 terms, by text on a tie, none for a page without terms" $ do
    let same = M.fromList [ (T.pack [c], 2) | c <- "lkjihgfedcba" ]
        docs = M.map (const 1) same
    bestTerms 10 docs same `shouldBe` map (T.pack . (: [])) (take maxKeywords "abcdefghij")
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
        kws = pagesKeywords (fr : en : others)
    -- French stop words out, English ones kept in a French page; "glace" and
    -- "the" are the most frequent forms, "Menthe" the first in order of a tie
    M.lookup ("s", "fr") kws `shouldBe` Just ["glace", "the", "Menthe"]
    M.lookup ("s", "en") kws `shouldBe` Just ["soup", "carrot"]

  it "weighs the title and the headings, the words taken from the content" $ do
    let p = textPage "p" English "Notes" "rails and ruby, rails" ["Ruby"]
        others = [ textPage (T.pack (show i)) English "Other" "Something else entirely" [] | i <- [1 .. 8 :: Int] ]
    M.lookup ("s", "p") (pagesKeywords (p : others)) `shouldBe` Just ["Notes", "ruby", "rails"]
