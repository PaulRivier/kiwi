module Kiwi.MetaSpec (metaSpec) where

import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Time as Time
import           Test.Hspec

import           Kiwi.Meta
import           Kiwi.Types

cfg :: [CustomMetaConfig]
cfg = [ CustomMetaConfig "date" CmtDate 1, CustomMetaConfig "authors" CmtText 1
      , CustomMetaConfig "count" CmtInt 1 ]

parse :: T.Text -> MetaData
parse = either error id . parseHeader cfg

-- | The normalized text of a header
normalize :: T.Text -> T.Text
normalize = writeHeader cfg . parse

metaSpec :: Spec
metaSpec = do
  it "writes the keys in a fixed order, one line by key" $
    normalize "---\nzz: 2\nauthors: Jack\\, Jr, John\nlang: french\ntags: b\naa: 1\n\
              \title: My: title\naccess: friends, family\nid: x\ntags: a\n\
              \files-dir: f\nimages-dir: i\narchive: 1\ndate: 2020-1-5\n..."
      `shouldBe` "title: My: title\nid: x\ntags: a, b\naccess: friends, family\n\
                 \lang: french\narchive: yes\nimages-dir: i\nfiles-dir: f\n\
                 \date: 2020-01-05\nauthors: Jack\\, Jr, John\naa: 1\nzz: 2\n"

  it "sorts the tags, expands their groups and removes duplicates" $
    normalize "tags: sport > {escalade, alpinisme}, b, sport>escalade, a\\, b, x \\> y"
      `shouldBe` "tags: a\\, b, b, sport > alpinisme, sport > escalade, x \\> y\n"

  it "keeps the values that can not be read, and the unknown keys" $ do
    normalize "lang: klingon\narchive: maybe\ncount: 3, x\nfoo: a\nfoo: b"
      `shouldBe` "lang: klingon\narchive: maybe\ncount: 3, x\nfoo: a\nfoo: b\n"
    normalize "lang: auto\nlang: auto / German" `shouldBe` "lang: auto/german\n"

  it "keeps empty values, which override the defaults" $
    normalize "title:\ntags:\nauthors:\nfoo:" `shouldBe` "title:\ntags:\nauthors:\nfoo:\n"

  it "reads back what it writes" $ do
    let headers = [ "title: t\ntags: a > b, c\naccess: x, y\nlang: auto/french\n\
                    \archive: no\nauthors: A\\, B, C\ndate: 2021-12-31\ncount: 1"
                  , "lang: zz\ncount: 1, z\nother: v\nother: w"
                  , "" ]
    mapM_ (\h -> parse (normalize h) `shouldBe` parse h) headers
    mapM_ (\h -> normalize (normalize h) `shouldBe` normalize h)
          (headers ++ ["tags: {a, b} > c, a>b\\>c, a\\{b\\}"])

  it "writes a page: header, blank line, content with a final newline" $ do
    writePage cfg (parse "title: t") "Hello\n\n" `shouldBe` "title: t\n\nHello\n"
    writePage cfg (parse "title: t") "" `shouldBe` "title: t\n\n"
    -- without metadata, the page starts with a blank line
    writePage cfg emptyMeta "# T" `shouldBe` "\n\n# T\n"
    fmap snd (splitPage cfg $ writePage cfg emptyMeta "a: b\n\nc") `shouldBe` Right "a: b\n\nc\n"

  it "merges metadata, the second one first" $ do
    let m = mergeMeta (parse "title: a\ntags: x\nauthors: A\nfoo: 1")
                      (parse "title: b\ncount: 2\nfoo: 2\nbar: 3")
    metaTitle m `shouldBe` Just "b"
    metaTags m `shouldBe` Just ["x"]
    map cmdName (metaCustom m) `shouldBe` ["count", "authors"]
    metaOther m `shouldBe` M.fromList [("foo", ["2"]), ("bar", ["3"])]

  it "clears or keeps fields by key, with their invalid values" $ do
    let m = parse "title: t\naccess: a\nid: i\nlang: zz\nauthors: A\nfoo: 1"
    writeHeader cfg (clearMeta ["access", "id", "lang", "authors"] m)
      `shouldBe` "title: t\nfoo: 1\n"
    writeHeader cfg (keepOnlyMeta ["access", "id", "lang", "foo"] m)
      `shouldBe` "id: i\naccess: a\nlang: zz\nfoo: 1\n"

  it "rebuilds an edited header without the fields the editor can not change" $ do
    let protected = ["access", "id", "archive"]
        original = parse "title: t\ntags: x\naccess: family\narchive: yes"
        -- the editor removed the tags and tried to change the access
        edited = parse "title: new\naccess: public"
    writeHeader cfg (mergeMeta (keepOnlyMeta protected original) (clearMeta protected edited))
      `shouldBe` "title: new\naccess: family\narchive: yes\n"

  it "reads custom dates written with or without leading zeros" $
    map cmdKeys (metaCustom $ parse "date: 2020-01-05, 2020-1-5")
      `shouldBe` [replicate 2 (KeyDay $ Time.fromGregorian 2020 1 5)]
