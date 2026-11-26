module Kiwi.MetaSpec (metaSpec) where

import qualified Data.ByteString as BS
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Time as Time
import           Test.Hspec

import           Kiwi.Meta
import           Kiwi.Tags (normalizeTag)
import           Kiwi.Types

cfg :: [CustomMetaConfig]
cfg = [ CustomMetaConfig "date" CmtDate 1 Nothing 0, CustomMetaConfig "authors" CmtText 1 Nothing 0
      , CustomMetaConfig "count" CmtInt 1 Nothing 0 ]

parse :: T.Text -> MetaData
parse = either error id . parseHeader cfg

-- | The normalized text of a header
normalize :: T.Text -> T.Text
normalize = writeHeader cfg . parse

metaSpec :: Spec
metaSpec = do
  it "writes the keys in a fixed order, one line by key" $
    normalize "---\nzz: 2\nauthors: Jack\\, Jr, John\nlang: french\ntags: b\naa: 1\n\
              \title: My: title\naccess: friends, family\nuid: x\ntags: a\n\
              \files-dir: f\nimages-dir: i\narchive: 1\ndate: 2020-1-5\n..."
      `shouldBe` "title: My: title\nuid: x\ntags: a, b\naccess: family, friends\n\
                 \lang: french\narchive: yes\n\
                 \date: 2020-01-05\nauthors: Jack\\, Jr, John\naa: 1\nfiles-dir: f\n\
                 \images-dir: i\nzz: 2\n"

  it "sorts the tags, removes duplicates and groups them by common prefix" $ do
    normalize "tags: sport > {escalade, alpinisme}, b, sport>escalade, a\\, b, x \\> y"
      `shouldBe` "tags: a\\, b, b, sport > {alpinisme, escalade}, x \\> y\n"
    normalize "tags: plantes > arbre, plantes > fabacée" `shouldBe` "tags: plantes > {arbre, fabacée}\n"
    -- groups at each level, a group of one is not one
    normalize "tags: loisir > sport > vélo, loisir > lecture, loisir > sport > escalade, a > b > c"
      `shouldBe` "tags: a > b > c, loisir > {lecture, sport > {escalade, vélo}}\n"
    normalize "tags: a > b > c, a > b > d" `shouldBe` "tags: a > b > {c, d}\n"
    -- a tag that is the prefix of others comes before them
    normalize "tags: cuisine > plat, cuisine, cuisine > dessert, a > b > c, a > d, a > b"
      `shouldBe` "tags: a > {b, b > c, d}, cuisine, cuisine > {dessert, plat}\n"
    -- groups on the right are expanded
    normalize "tags: {a, b} > c" `shouldBe` "tags: a > c, b > c\n"
    -- the special characters of the segments are escaped
    normalize "tags: x > a\\, b, x > c\\{d\\}, x > e \\> f"
      `shouldBe` "tags: x > {a\\, b, c\\{d\\}, e \\> f}\n"

  it "reads back the tags it groups" $ do
    let tags = fmap (L.sort . map normalizeTag) . metaTags . parse
        headers = [ "tags: loisir > sport > vélo, loisir > lecture, loisir > sport > escalade"
                  , "tags: cuisine > plat, cuisine, cuisine > dessert, a > b > c, a > d, a > b"
                  , "tags: x > a\\, b, x > c\\{d\\}, x > e \\> f, x, y \\> z > {t, u}" ]
    mapM_ (\h -> tags (normalize h) `shouldBe` tags h) headers
    mapM_ (\h -> normalize (normalize h) `shouldBe` normalize h) headers

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
    let m = parse "title: t\naccess: a\nuid: i\nlang: zz\nauthors: A\nfoo: 1"
    writeHeader cfg (clearMeta ["access", "uid", "lang", "authors"] m)
      `shouldBe` "title: t\nfoo: 1\n"
    writeHeader cfg (keepOnlyMeta ["access", "uid", "lang", "foo"] m)
      `shouldBe` "uid: i\naccess: a\nlang: zz\nfoo: 1\n"

  it "rebuilds an edited header without the fields the editor can not change" $ do
    let protected = ["access", "uid", "archive"]
        original = parse "title: t\ntags: x\naccess: family\narchive: yes"
        -- the editor removed the tags and tried to change the access
        edited = parse "title: new\naccess: public"
    writeHeader cfg (mergeMeta (keepOnlyMeta protected original) (clearMeta protected edited))
      `shouldBe` "title: new\naccess: family\narchive: yes\n"

  it "writes the uid and the previous uids after the title, escaping commas" $ do
    normalize "tags: x\nprevious-uids: a:b, c\\,d\nuid: U\ntitle: t"
      `shouldBe` "title: t\nuid: U\nprevious-uids: a:b, c\\,d\ntags: x\n"
    metaPreviousUids (parse "previous-uids: a:b, c\\,d") `shouldBe` Just ["a:b", "c,d"]

  it "reads the summary, written after the title" $ do
    metaSummary (parse "summary: First aid\nsummary: and burns\n") `shouldBe`
      Just "First aid and burns"
    normalize "uid: x\nsummary: Short: text\ntitle: T\n" `shouldBe`
      "title: T\nsummary: Short: text\nuid: x\n"
    unknownMetaKeys cfg (parse "summary: s\n") `shouldBe` []

  it "checks the uids" $ do
    map isValidUid ["a", "Home", "menthe-poivrée_2", T.replicate 64 "x"]
      `shouldBe` replicate 4 True
    map isValidUid ["", "a:b", "a/b", "a b", "a#b", T.replicate 65 "x"]
      `shouldBe` replicate 6 False

  it "makes a uid of Crockford base32 characters from random bytes" $ do
    uidFromBytes (BS.pack [0, 1, 9, 10, 17, 18, 21, 26, 31, 32, 255])
      `shouldBe` "019AHJNTZ0Z"
    T.length (uidFromBytes $ BS.replicate 10 7) `shouldBe` 10

  it "reads custom dates written with or without leading zeros" $
    map cmdKeys (metaCustom $ parse "date: 2020-01-05, 2020-1-5")
      `shouldBe` [replicate 2 (KeyDay $ Time.fromGregorian 2020 1 5)]

  it "reads the rights of the access groups" $ do
    parseAccess "family:w, friends, me : A, both:rw, both:r, x:, y:z, public:w, :w"
      `shouldBe` ( [ ("both", AccessWrite), ("family", AccessWrite), ("friends", AccessRead)
                   , ("me", AccessAdmin) ]
                 , ["x:", "y:z", "public:w", ":w"] )
    parseAccess "public, public:r" `shouldBe` ([("public", AccessRead)], [])

  it "writes the access by right, keeping the invalid groups" $ do
    normalize "access: friends, me:a, family:rw, public, zz:x"
      `shouldBe` "access: me:a, family:w, friends, public, zz:x\n"
    -- the invalid groups are ignored: without valid one, no one can read
    metaAccess (parse "access: x:z") `shouldBe` Just []
    normalize "access: x:z" `shouldBe` "access: x:z\n"
    invalidMetaValues cfg (parse "access: a, x:z, public:a")
      `shouldBe` [("access", "x:z"), ("access", "public:a")]
