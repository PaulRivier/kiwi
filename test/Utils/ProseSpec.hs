{-# LANGUAGE QuasiQuotes #-}
module Utils.ProseSpec (proseSpec) where

import qualified Data.Text as T
import           Test.Hspec

import           Utils.Prose (prose, foldProse)


proseSpec :: Spec
proseSpec = do
  it "joins the lines of a paragraph by a space, without their indentation" $
    foldProse "\n    Full-text search\n      in the wiki.  \n  " `shouldBe` "Full-text search in the wiki."
  it "separates the paragraphs by a blank line" $
    foldProse "\n  One\n  two.\n\n\n  Three.\n" `shouldBe` "One two.\n\nThree."
  it "keeps the quotes and backslashes as written" $
    foldProse "\"exact phrase\" -word \\n" `shouldBe` "\"exact phrase\" -word \\n"
  it "gives a Text" $
    ([prose|
       A first
       paragraph.
     |] :: T.Text) `shouldBe` "A first paragraph."
