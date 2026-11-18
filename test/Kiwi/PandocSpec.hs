module Kiwi.PandocSpec (loadPageSpec, metaDataSpec) where

import           Control.Monad (forM_)
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Time as Time
import           Test.Hspec
import qualified Text.Pandoc.Definition as PD
import qualified Text.Pandoc.Walk as PW

import qualified Kiwi.ConfigFile as Conf
import           Data.Either (isLeft)
import           Data.Maybe (fromMaybe)
import           Kiwi.Meta (parseHeader, parseHeaderFields, sourceMeta, splitPage,
                            unknownMetaKeys, invalidMetaValues)
import           Kiwi.Pandoc (linksInContext, loadPage, pageDoc, pagePath)
import           Kiwi.Types
import qualified Kiwi.Utils as U
import           TestUtils

-- | (classes, url) of every link, and url of every image
links :: PandocPage -> [([T.Text], T.Text)]
links = docLinks . pandocDoc

docLinks :: PD.Pandoc -> [([T.Text], T.Text)]
docLinks = PW.query f
  where f (PD.Link (_, cls, _) _ (url, _)) = [(cls, url)]
        f _ = []

images :: PandocPage -> [T.Text]
images = docImages . pandocDoc

docImages :: PD.Pandoc -> [T.Text]
docImages = PW.query f
  where f (PD.Image _ _ (url, _)) = [url]
        f _ = []

blocks :: PandocPage -> [PD.Block]
blocks p = let PD.Pandoc _ bs = pandocDoc p in bs

-- | Header of a page text
header :: [CustomMetaConfig] -> T.Text -> MetaData
header cfg = either error fst . splitPage cfg

-- | Custom metadata of a header
customMeta :: [CustomMetaConfig] -> T.Text -> [CustomMetaData]
customMeta cfg = metaCustom . either error id . parseHeader cfg

unknownKeys :: [CustomMetaConfig] -> T.Text -> [T.Text]
unknownKeys cfg = unknownMetaKeys cfg . header cfg

invalidValues :: [CustomMetaConfig] -> T.Text -> [(T.Text, T.Text)]
invalidValues cfg = invalidMetaValues cfg . header cfg

loadPageSpec :: Spec
loadPageSpec = do
  it "reads metadata header and content" $ do
    let p = load "title: My title\ntags: a, b > c\naccess: public\nlang: french\n\nHello"
        m = pandocMeta p
    metaTitle m `shouldBe` Just "My title"
    metaTags m `shouldBe` Just ["a", "b > c"]
    metaAccess m `shouldBe` Just [("public", AccessRead)]
    metaLang m `shouldBe` Just (LangFixed French)
    blocks p `shouldBe` [PD.Para [PD.Str "Hello"]]

  it "separates the blocks in the plain text of the content" $
    U.pageContent (pandocDoc $ load
      "title: t\n\n# Titre\n## Sous-titre\nUn *texte*\n\n- a\n- b\n\n> c\n\n\
      \| x | y |\n|---|---|\n| z | w |")
      `shouldBe` "Titre Sous-titre Un texte a b c x y z w"

  it "uses default metadata when fields are missing" $ do
    let m = pandocMeta $ load "foo: bar\n\nHello"
    metaTitle m `shouldBe` Just "untitled"
    metaTags m `shouldBe` Just ["untagged"]
    metaAccess m `shouldBe` Just [("private", AccessRead)]
    metaLang m `shouldBe` Just (LangFixed English)
    metaUid m `shouldBe` Nothing

  it "joins repeated fields" $ do
    let m = pandocMeta $ load "title: a\ntags: t1\ntags: t2, t3\n\n"
    metaTags m `shouldBe` Just ["t1", "t2", "t3"]

  it "expands groups of tags, on several lines" $ do
    let m = pandocMeta $ load "title: a\ntags: sport > {alpinisme, escalade}\ntags: b\n\n"
    metaTags m `shouldBe` Just ["sport > alpinisme", "sport > escalade", "b"]

  it "reads the uid and the previous uids, in the header of the file" $ do
    let p = load "uid: my-id\nprevious-uids: a:b, X\\,Y\n\n"
    metaUid (pandocMeta p) `shouldBe` Just "my-id"
    metaUid (pandocHeader p) `shouldBe` Just "my-id"
    metaPreviousUids (pandocHeader p) `shouldBe` Just ["a:b", "X,Y"]
    metaTitle (pandocHeader p) `shouldBe` Nothing

  it "ignores an empty uid" $ do
    metaUid (pandocMeta $ load "uid:\n\n") `shouldBe` Nothing
    metaUid (pandocMeta $ load "uid:   \n\n") `shouldBe` Nothing

  it "refuses windows line endings and a byte order mark" $ do
    splitPage [] "title: T\r\ntags: x\r\n\r\nline 1\r\nline 2" `shouldSatisfy` isLeft
    splitPage [] "\xFEFFtitle: T\n\nHello" `shouldSatisfy` isLeft

  it "accepts a file with header only" $ do
    let p = load "title: Only header"
    metaTitle (pandocMeta p) `shouldBe` Just "Only header"
    blocks p `shouldBe` []

  it "fixes page links by uid, with spaces around the uid, and collects them" $ do
    let p = load "title: t\n\n[a](page:ABC) [b](<page: XYZ >) [c](<page: XYZ #part 2>) \
                 \[d](page:A%20B#x) [e](uid:ABC)"
    links p `shouldBe` [ (["kiwi-link-page"], "/page/src/ABC")
                       , (["kiwi-link-page"], "/page/src/XYZ")
                       , (["kiwi-link-page"], "/page/src/XYZ#part 2")
                       , (["kiwi-link-page"], "/page/src/A%20B#x")
                       -- no longer a page link
                       , (["kiwi-link-external"], "uid:ABC") ]
    pandocPageLinks p `shouldBe` [ ("src", "ABC"), ("src", "XYZ"), ("src", "XYZ")
                                 , ("src", "A B") ]

  it "links the files by uid, the class given by the extension of the file" $ do
    let doc = displayWith [ ("AAAAAAAAAA", "sub/a.png"), ("BBBBBBBBBB", "b.JPG")
                          , ("CCCCCCCCCC", "c d/e.ods") ]
                "[i](<file:AAAAAAAAAA>) [j](file:BBBBBBBBBB) [f](<file: CCCCCCCCCC#page=2>) \
                \![x](file:AAAAAAAAAA)"
    docLinks doc `shouldBe` [ (["kiwi-link-image"], "/file/src/AAAAAAAAAA")
                            , (["kiwi-link-image"], "/file/src/BBBBBBBBBB")
                            , (["kiwi-link-file"], "/file/src/CCCCCCCCCC#page=2") ]
    docImages doc `shouldBe` ["/file/src/AAAAAAAAAA"]

  it "breaks the file links whose target is not the uid of a file of the source" $
    forM_ [ "a.png", "DDDDDDDDDD", "aaaaaaaaaa", "AAAAAAAAA", "AAAAAAAAAAA", "", "#x"
          , "sub/a.png" ] $ \t -> do
      let doc = displayWith [("AAAAAAAAAA", "sub/a.png")] $
                  T.concat ["[l](<file:", t, ">) ![i](<file:", t, ">)"]
      (t, docLinks doc) `shouldBe` (t, [(["kiwi-link-broken"], "")])
      (t, docImages doc) `shouldBe` (t, [""])

  it "collects the files linked, as written" $
    pandocFiles (load "title: t\n\n[a](<file:AAAAAAAAAA#p>) ![b](<file: BBBBBBBBBB >) \
                      \[c](<file:x%20y.png>) [d](file:) [e](file:AAAAAAAAAA)")
      `shouldBe` ["AAAAAAAAAA", "BBBBBBBBBB", "x y.png"]

  it "keeps anchors and external links" $ do
    let p = load "title: t\n\n[a](#section) [b](https://example.org)"
    links p `shouldBe` [ (["kiwi-link-anchor"], "#section")
                       , (["kiwi-link-external"], "https://example.org") ]

  it "keeps the navigation context in page links only" $ do
    let p = load "title: t\n\n[a](<page:Other#part>) [b](#section) [c](https://example.org)"
        inCtx archive = links p { pandocDoc = linksInContext
                                    (Context (S.fromList ["x"]) archive) (pandocDoc p) }
    inCtx False `shouldBe` [ (["kiwi-link-page"], "/page/src/Other?source=x#part")
                           , (["kiwi-link-anchor"], "#section")
                           , (["kiwi-link-external"], "https://example.org") ]
    take 1 (inCtx True)
      `shouldBe` [(["kiwi-link-page"], "/page/src/Other?source=x&archive=yes#part")]

  it "keeps the navigation context before the anchor of a page with a '#'" $ do
    let p = load "title: t\n\n[a](<page:C%23#part>)"
        inCtx = links p { pandocDoc = linksInContext
                            (Context (S.fromList ["x"]) False) (pandocDoc p) }
    inCtx `shouldBe` [(["kiwi-link-page"], "/page/src/C%23?source=x#part")]

  it "reads the archive metadata" $ do
    let archived = fromMaybe False . metaArchive . pandocMeta . load
    archived "title: t\n\nx" `shouldBe` False
    archived "title: t\narchive: Yes\n\nx" `shouldBe` True
    archived "title: t\narchive: 0\n\nx" `shouldBe` False
    archived "title: t\narchive: oui\n\nx" `shouldBe` False
    invalidValues [] "title: t\narchive: oui\n\narchive: x" `shouldBe` [("archive", "oui")]
    invalidValues [] "title: t\narchive: true\n\narchive: x" `shouldBe` []

  it "finds the invalid lang and typed custom metadata values" $ do
    let cfg = [ CustomMetaConfig "date" CmtDate 1 Nothing, CustomMetaConfig "count" CmtInt 1 Nothing
              , CustomMetaConfig "flag" CmtBool 1 Nothing, CustomMetaConfig "authors" CmtText 1 Nothing ]
    invalidValues cfg "lang: fr\ndate: 2020-1-5, 2020-13-1\ncount: 3, x\n\
                          \flag: yes, oui\nauthors: anything\n\nlang: x"
      `shouldBe` [ ("lang", "fr"), ("date", "2020-13-1"), ("count", "x"), ("flag", "oui") ]
    invalidValues cfg "lang: auto/french\ndate: 2020-1-5\ncount: 3\nflag: No\n\n"
      `shouldBe` []
    -- the default lang is used
    metaLang (pandocMeta $ load "title: t\nlang: fr\n\nx") `shouldBe` Just (LangFixed English)

  it "does not read image: links any more, nor images-dir and files-dir" $ do
    let p = load "title: t\nimages-dir: pics\nfiles-dir: docs\n\n\
                 \![x](<image:a.png>) [i](<image:b.png>)"
    images p `shouldBe` ["image:a.png"]
    links p `shouldBe` [(["kiwi-link-external"], "image:b.png")]
    unknownKeys [] "title: t\nimages-dir: pics\nfiles-dir: docs\n\nx"
      `shouldBe` ["files-dir", "images-dir"]

  it "parses a page again, from its content, to display it" $ do
    let body = "[a](<page:Other>) ![x](<file:a.png>) [f](<file:c.ods>)"
        c = "title: t\n\n" <> body
        page = Page { pageUID = ("src", "sub:p")
                    , pageAbsoluteFSPath = "/k/content/src/sub/p.md"
                    , pageMTime = Time.UTCTime (Time.fromGregorian 2026 1 1) 0
                    , pageBody = body
                    , pageFiles = ["a.png", "c.ods"]
                    , pagePlainText = ""
                    , pageTitle = "t"
                    , pageSummary = Nothing
                    , pageTags = []
                    , pageLang = English
                    , pageAccess = []
                    , pageLinks = []
                    , pageArchive = False
                    , pageCustomMeta = []
                    , pagePreviousUids = []
                    }
    pandocBody (load c) `shouldBe` body
    pandocFiles (load c) `shouldBe` ["a.png", "c.ods"]
    -- with the lines of the top-level blocks (a paragraph: in a div)
    let noLines (PD.Pandoc m bs) = PD.Pandoc m (concatMap unwrap bs)
        unwrap (PD.Div ("", [], [("kiwi-line", _)]) [b]) = [b]
        unwrap b = [b]
    fmap noLines (pageDoc M.empty page) `shouldBe` Right (pandocDoc $ load c)
    fmap (\(PD.Pandoc _ bs) -> length bs) (pageDoc M.empty page) `shouldBe` Right 1
    pagePath "/k/content" "/k/content/src/sub/p.md" `shouldBe` ("src", "sub/p")

metaDataSpec :: Spec
metaDataSpec = do
  it "reads the access of the sources config, ignoring empty ones" $ do
    let ini = T.unlines [ "[kiwi]", "content-directory: content", "port: 3456"
                        , "ui-lang: english", "[metadata-default]"
                        , "[source a]", "access: family, friends"
                        , "[source b]", "access: ,", "[source c]", "lang: french" ]
        access = map (\sc -> (scSourceName sc, scAccess sc)) . Conf.sourcesConfig
    fmap access (Conf.parseConfig ini) `shouldBe`
      Right [ ("a", Just [("family", AccessRead), ("friends", AccessRead)])
            , ("b", Nothing), ("c", Nothing) ]

  it "ignores the removed root-tag setting of a source" $ do
    let ini = T.unlines [ "[kiwi]", "content-directory: content", "port: 3456"
                        , "ui-lang: english", "[metadata-default]"
                        , "[source a]", "root-tag: x > y", "access: family" ]
    fmap (map scAccess . Conf.sourcesConfig) (Conf.parseConfig ini) `shouldBe`
      Right [Just [("family", AccessRead)]]

  it "uses the access of the source config, unless the page has one" $ do
    let sc = SourceConfig "src" (Just [("family", AccessWrite)]) Nothing
        access sM c = case loadPage "test.md" c "src" (sourceMeta sM defMeta) [] of
          Left e -> error e
          Right p -> map fst <$> metaAccess (pandocMeta p)
    access (Just sc) "title: t\n\n" `shouldBe` Just ["family"]
    access (Just sc) "title: t\naccess: public\n\n" `shouldBe` Just ["public"]
    access (Just sc { scAccess = Nothing }) "title: t\n\n" `shouldBe` Just ["private"]
    access Nothing "title: t\n\n" `shouldBe` Just ["private"]

  it "parses metadata lines, separators and blank lines" $
    parseHeaderFields [] "---\ntitle: a: b\n  \ntags: x\n tags : y \n...\nimages-dir:"
      `shouldBe` Right (M.fromList [ ("title", ["a: b"]), ("tags", ["x", "y"])
                                   , ("images-dir", [""]) ])

  it "refuses a header with anything else than metadata" $ do
    parseHeaderFields [] "title: t\nnot meta\ntags: x" `shouldBe`
      Left "invalid metadata header, line 2"
    parseHeaderFields [] "# Heading" `shouldBe` Left "invalid metadata header, line 1"
    parseHeaderFields [] "Some text: with a colon" `shouldBe`
      Left "invalid metadata header, line 1"
    parseHeaderFields [] ": no key" `shouldBe` Left "invalid metadata header, line 1"
    parseHeaderFields [] ":::" `shouldBe` Left "invalid metadata header, line 1"
    -- the names of the custom metadata may have several words
    parseHeaderFields ["my field"] "my field: x\nmy-key_2: y" `shouldBe`
      Right (M.fromList [("my field", ["x"]), ("my-key_2", ["y"])])

  it "refuses a page whose first block is not its metadata" $ do
    let loaded c = either Left (const $ Right ()) $ loadPage "test.md" c "src" defMeta []
    loaded "# Title\n\nText" `shouldBe` Left "invalid metadata header, line 1"
    loaded "Just some text, without blank line" `shouldBe`
      Left "invalid metadata header, line 1"
    loaded "title: t\nA sentence.\n\nText" `shouldBe` Left "invalid metadata header, line 2"
    -- an empty header: the content starts after the first blank line
    loaded "\n\n# Title" `shouldBe` Right ()
    loaded "" `shouldBe` Right ()
    fmap (metaTitle . pandocMeta) (loadPage "test.md" "\n\n# Title" "src" defMeta [])
      `shouldBe` Right (Just "untitled")

  it "parses custom metadata according to their type" $ do
    let cfg = [ CustomMetaConfig "date" CmtDate 1 Nothing
              , CustomMetaConfig "count" CmtInt 1 Nothing
              , CustomMetaConfig "flag" CmtBool 1 Nothing
              , CustomMetaConfig "authors" CmtText 1 Nothing
              , CustomMetaConfig "absent" CmtText 1 Nothing ]
        m = "date: 2020-1-5, 2021-12-31, bad\ncount: 3, x, 12\n\
            \flag: Yes\nauthors: John, Jack\\, Jr"
        res = [ (cmdName c, cmdKeys c) | c <- customMeta cfg m ]
    res `shouldBe` [ ("date", [ KeyDay (Time.fromGregorian 2020 1 5)
                              , KeyDay (Time.fromGregorian 2021 12 31) ])
                   , ("count", [KeyInt 3, KeyInt 12])
                   , ("flag", [KeyBool True])
                   , ("authors", [KeyText "John", KeyText "Jack, Jr"]) ]

  it "finds the unknown metadata keys, in the header only" $ do
    let cfg = [CustomMetaConfig "authors" CmtText 1 Nothing]
        c = "title: t\ntag: typo\nTitle: case\nauthors: a\nfiles-dir: f\n\nbody: no"
    unknownKeys cfg c `shouldBe` ["Title", "files-dir", "tag"]
    unknownKeys [] "title: t\nauthors: a" `shouldBe` ["authors"]

  it "reads booleans" $ do
    let cfg = [CustomMetaConfig "b" CmtBool 1 Nothing]
        m = "b: true, 1, Yes, no, false, 0, oui"
    -- invalid values are ignored
    map cmdKeys (customMeta cfg m)
      `shouldBe` [map KeyBool [True, True, True, False, False, False]]

  it "parses lang settings" $ do
    let parse = U.parseLangSetting
    parse "spanish" `shouldBe` Right (LangFixed Spanish)
    parse " Auto / French " `shouldBe` Right (LangAuto French)
    parse "auto" `shouldBe` Right (LangAuto English)
    either (const True) (const False) (parse "klingon") `shouldBe` True
    either (const True) (const False) (parse "auto/klingon") `shouldBe` True

  it "reads the lang of the config and of the sources" $ do
    let ini lang = T.unlines [ "[kiwi]", "content-directory: content", "port: 3456"
                             , "ui-lang: english", "[metadata-default]", lang
                             , "[source a]", "lang: auto", "[source b]", "lang: french"
                             , "[source c]" ]
        langs c = (metaLang (Conf.defaultMeta c), map scLang (Conf.sourcesConfig c))
    fmap langs (Conf.parseConfig $ ini "lang: auto/spanish") `shouldBe`
      Right (Just (LangAuto Spanish), [Just (LangAuto English), Just (LangFixed French), Nothing])
    fmap langs (Conf.parseConfig $ ini "") `shouldBe`
      Right (Just (LangFixed English), [Just (LangAuto English), Just (LangFixed French), Nothing])
    either (const True) (const False) (Conf.parseConfig $ ini "lang: klingon")
      `shouldBe` True

  it "uses the lang of the source config, unless the page has one" $ do
    let sc = SourceConfig "src" Nothing (Just (LangAuto Spanish))
        lang c = case loadPage "test.md" c "src" (sourceMeta (Just sc) defMeta) [] of
          Left e -> error e
          Right p -> metaLang $ pandocMeta p
    lang "title: t\n\n" `shouldBe` Just (LangAuto Spanish)
    lang "title: t\nlang: auto/french\n\n" `shouldBe` Just (LangAuto French)
    lang "title: t\nlang: german\n\n" `shouldBe` Just (LangFixed German)
    lang "title: t\nlang: klingon\n\n" `shouldBe` Just (LangAuto Spanish)

