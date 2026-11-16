module Kiwi.MigrationSpec (migrationSpec) where

import           Control.Monad (forM_)
import qualified Data.Map as M
import qualified Data.Text as T
import           Test.Hspec

import           Kiwi.Meta (emptyMeta)
import           Kiwi.Migration
import           Kiwi.Types

-- | Content of a page of folder "sub/dir", converted with these former ids
convert :: [(T.Text, T.Text)] -> T.Text -> (T.Text, [T.Text])
convert ids = convertBody (`lookup` ids) "sub/dir"

migrationSpec :: Spec
migrationSpec = do
  it "gives the id field, or else the id of the path, as former id" $ do
    let header = emptyMeta { metaTitle = Just "t", metaOther = M.fromList [("id", ["Home"])] }
    formerIds "a/b" header `shouldBe` (["Home"], header { metaOther = M.empty })
    formerIds "a/b" emptyMeta `shouldBe` (["a:b"], emptyMeta)
    formerIds "a" emptyMeta { metaOther = M.fromList [("id", ["  "])] }
      `shouldBe` (["a"], emptyMeta)

  it "converts relative and absolute links, as they are resolved in the page" $
    convert [ ("sub:dir:x", "X"), ("sub:y", "Y"), ("top", "T"), ("sub:dir:a b", "AB") ]
      "[x](page:x) [x2](<page:./x.md>) [y](page:../y \"title\") [t](page:/top.markdown) \
      \[t2](page:../../../top) [ab](<page:a b>) [ab2](page:a%20b)"
      `shouldBe` ( "[x](uid:X) [x2](<uid:X>) [y](uid:Y \"title\") [t](uid:T) \
                   \[t2](uid:T) [ab](<uid:AB>) [ab2](uid:AB)", [] )

  it "keeps the fragment of the links" $
    convert [("sub:dir:x", "X")] "[a](page:x#part) [b](<page:x.md#part 2>)"
      `shouldBe` ("[a](uid:X#part) [b](<uid:X#part 2>)", [])

  it "converts the reference definitions" $
    convert [("sub:dir:x", "X"), ("top", "T")] "[a][r]\n\n[r]: page:x\n[s]:  <page:/top>\n"
      `shouldBe` ("[a][r]\n\n[r]: uid:X\n[s]:  <uid:T>\n", [])

  it "keeps the links whose target is not found, and the text out of links" $
    convert [("sub:dir:x", "X")] "page:x is (page:none) <page:x> homepage:x [a]( page:x)"
      `shouldBe` ("page:x is (page:none) <page:x> homepage:x [a]( uid:X)", ["none"])

  it "converts the file links written with a path to links with the uid of the file" $
    convertFileBody (`lookup` [("sub/dir/a.png", "AAAAAAAAAA"), ("b c.pdf", "BBBBBBBBBB")])
      "sub/dir"
      "![x](file:a.png) [y](<file:/b c.pdf#page=2>) [z](file:AAAAAAAAAA#p) \
      \[w](file:gone.png) [u](<file:../dir/a.png>) profile:x\n\n[r]: file:./a.png\n"
      `shouldBe` ( "![x](file:AAAAAAAAAA) [y](<file:BBBBBBBBBB#page=2>) [z](file:AAAAAAAAAA#p) \
                   \[w](file:gone.png) [u](<file:../dir/a.png>) profile:x\n\n\
                   \[r]: file:AAAAAAAAAA\n"
                 , ["gone.png", "../dir/a.png"] )

  it "resolves the file paths written in a page, in NFC" $ do
    fileTarget "" "e\x301\&te\x301\&e\x301.png" `shouldBe` Just (["\xe9t\xe9\xe9.png"], "")
    fileTarget "re\x301" "a.png#x" `shouldBe` Just (["r\xe9", "a.png"], "#x")
    fileTarget "sub" "./e//./f.pdf" `shouldBe` Just (["sub", "e", "f.pdf"], "")
    fileTarget "sub" "/100%25.ods" `shouldBe` Just (["100%.ods"], "")
    forM_ [ "../a.png", "/x/../a.png", ".git/config", "_a.pdf", "x/_b/c.png", "p.md"
          , "p.md~", "%23p.md%23", "a~", "", "#anchor", "./", "x/" ] $ \t ->
      (t, fileTarget "sub" t) `shouldBe` (t, Nothing)
