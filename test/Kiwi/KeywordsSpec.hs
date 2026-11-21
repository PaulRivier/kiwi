{-# LANGUAGE OverloadedStrings #-}
module Kiwi.KeywordsSpec (keywordsSpec) where

import qualified Data.Map.Strict as M
import qualified Data.Text as T
import           Test.Hspec

import           Data.SearchEngine (TermStats (..))
import           Kiwi.Keywords (bestTerms, maxKeywords)
import           Kiwi.Types

-- | A term in the content (and the title), in some documents
term :: T.Text -> Int -> Int -> Int -> TermStats PageField
term t content title docs =
  TermStats t ([ (TitleField, title) | title > 0 ] ++ [ (ContentField, content) | content > 0 ]) docs

keywordsSpec :: Spec
keywordsSpec = do
  it "weighs the terms of a page by their occurrences and their rarity in the wiki" $ do
    let stats = [ term "menth" 3 0 1       -- (1 + log 3) × log 10: the best
                , term "glac" 2 1 2        -- weight 5 (title 3 times)
                , term "beaucoup" 1 0 1    -- seen once: left out
                , term "350g" 3 0 1        -- a quantity: left out
                , term "la" 4 0 9          -- in most pages: under the threshold
                , term "sucr" 2 0 3
                , term "titr" 1 0 2        -- once, but also in a heading
                ]
    bestTerms 10 (M.fromList [("titr", 1)]) stats `shouldBe` ["menth", "glac", "titr", "sucr"]

  it "gives at most 10 terms, by text on a tie, none for a page without terms" $ do
    let same = [ term (T.pack [c]) 2 0 1 | c <- "lkjihgfedcba" ]
    bestTerms 10 M.empty same `shouldBe` map (T.pack . (: [])) (take maxKeywords "abcdefghij")
    bestTerms 10 M.empty [] `shouldBe` []
    bestTerms 10 M.empty [term "x" 1 0 1] `shouldBe` []
