module Kiwi.PandocSpec (loadPageSpec, metaDataSpec) where

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Time as Time
import           Test.Hspec
import qualified Text.Pandoc.Definition as PD
import qualified Text.Pandoc.Walk as PW

import qualified Kiwi.ConfigFile as Conf
import           Kiwi.Pandoc (linksInContext, loadPage, pageDoc, pagePath, parseMetaData,
                              parseCustomMeta, sourceMeta, unknownMetaKeys,
                              invalidMetaValues)
import           Kiwi.Types
import qualified Kiwi.Utils as U
import           TestUtils

-- | (classes, url) of every link, and url of every image
links :: PandocPage -> [([T.Text], T.Text)]
links = PW.query f . pandocDoc
  where f (PD.Link (_, cls, _) _ (url, _)) = [(cls, url)]
        f _ = []

images :: PandocPage -> [T.Text]
images = PW.query f . pandocDoc
  where f (PD.Image _ _ (url, _)) = [url]
        f _ = []

blocks :: PandocPage -> [PD.Block]
blocks p = let PD.Pandoc _ bs = pandocDoc p in bs

loadPageSpec :: Spec
loadPageSpec = do
  it "reads metadata header and content" $ do
    let p = load "title: My title\ntags: a, b > c\naccess: public\nlang: french\n\nHello"
        m = pandocMeta p
    metaTitle m `shouldBe` "My title"
    metaTags m `shouldBe` ["a", "b > c"]
    metaAccess m `shouldBe` ["public"]
    metaLang m `shouldBe` LangFixed French
    blocks p `shouldBe` [PD.Para [PD.Str "Hello"]]

  it "uses default metadata when fields are missing" $ do
    let m = pandocMeta $ load "foo: bar\n\nHello"
    metaTitle m `shouldBe` "untitled"
    metaTags m `shouldBe` ["untagged"]
    metaAccess m `shouldBe` ["private"]
    metaLang m `shouldBe` LangFixed English
    metaId m `shouldBe` Nothing

  it "joins repeated fields" $ do
    let m = pandocMeta $ load "title: a\ntags: t1\ntags: t2, t3\n\n"
    metaTags m `shouldBe` ["t1", "t2", "t3"]

  it "expands groups of tags, on several lines" $ do
    let m = pandocMeta $ load "title: a\ntags: sport > {alpinisme, escalade}\ntags: b\n\n"
    metaTags m `shouldBe` ["sport > alpinisme", "sport > escalade", "b"]

  it "reads custom id" $
    metaId (pandocMeta $ load "id: my-id\n\n") `shouldBe` Just "my-id"

  it "ignores an empty id" $ do
    metaId (pandocMeta $ load "id:\n\n") `shouldBe` Nothing
    metaId (pandocMeta $ load "id:   \n\n") `shouldBe` Nothing

  it "accepts windows line endings" $ do
    let p = load "title: T\r\ntags: x\r\n\r\nline 1\r\nline 2"
    metaTitle (pandocMeta p) `shouldBe` "T"
    metaTags (pandocMeta p) `shouldBe` ["x"]
    blocks p `shouldBe` [PD.Para [PD.Str "line", PD.Space, PD.Str "1",
                                  PD.SoftBreak, PD.Str "line", PD.Space, PD.Str "2"]]

  it "accepts a file with header only" $ do
    let p = load "title: Only header"
    metaTitle (pandocMeta p) `shouldBe` "Only header"
    blocks p `shouldBe` []

  it "fixes relative and absolute page links, and collects them" $ do
    let p = load "title: t\n\n[a](<page:Other>) [b](<page:/Top/Page>)"
    links p `shouldBe` [ (["kiwi-link-page"], "/page/src/sub:Other")
                       , (["kiwi-link-page"], "/page/src/Top:Page") ]
    pandocPageLinks p `shouldBe` [("src", "sub:Other"), ("src", "Top:Page")]

  it "fixes page links from a page at top of pages folder" $ do
    let p = loadIn "" "title: t\n\n[a](<page:Other>)"
    links p `shouldBe` [(["kiwi-link-page"], "/page/src/Other")]
    pandocPageLinks p `shouldBe` [("src", "Other")]

  it "separates the anchor of a page link from the page id" $ do
    let p = load "title: t\n\n[a](<page:Other#part 2>) [b](<page:/Top.md#x>)"
    links p `shouldBe` [ (["kiwi-link-page"], "/page/src/sub:Other#part 2")
                       , (["kiwi-link-page"], "/page/src/Top#x") ]
    pandocPageLinks p `shouldBe` [("src", "sub:Other"), ("src", "Top")]

  it "encodes the page id of a link, which may be percent-encoded" $ do
    let p = load "title: t\n\n[a](<page:Chiffon Cake>) [b](<page:Pourquoi ?>) \
                 \[c](<page:C%23>) [d](<page:100%>) [e](<page:100%25.md>) \
                 \[f](page:a%20b)"
    map snd (links p) `shouldBe` [ "/page/src/sub:Chiffon%20Cake"
                                 , "/page/src/sub:Pourquoi%20%3F"
                                 , "/page/src/sub:C%23"
                                 , "/page/src/sub:100%25"
                                 , "/page/src/sub:100%25"
                                 , "/page/src/sub:a%20b" ]
    pandocPageLinks p `shouldBe` [ ("src", "sub:Chiffon Cake"), ("src", "sub:Pourquoi ?")
                                 , ("src", "sub:C#"), ("src", "sub:100%")
                                 , ("src", "sub:100%"), ("src", "sub:a b") ]

  it "removes the extension of a page link" $ do
    let p = load "title: t\n\n[a](<page:a.md>) [b](<page:b.markdown>) [c](<page:c.txt>)"
    pandocPageLinks p `shouldBe` [("src", "sub:a"), ("src", "sub:b"), ("src", "sub:c.txt")]

  it "resolves the . and .. segments of page links, inside the source" $ do
    let p = loadIn "a/b" "title: t\n\n[a](<page:../x>) [b](<page:./y>) \
                         \[c](<page:../../../z>) [d](<page:/q/../r>) [e](<page:s//t/>)"
    pandocPageLinks p `shouldBe` [ ("src", "a:x"), ("src", "a:b:y"), ("src", "z")
                                 , ("src", "r"), ("src", "a:b:s:t") ]
    map snd (links p) `shouldBe` [ "/page/src/a:x", "/page/src/a:b:y", "/page/src/z"
                                 , "/page/src/r", "/page/src/a:b:s:t" ]

  it "encodes image and file paths, keeping their fragment" $ do
    let p = loadIn "a b" "title: t\n\n![i](<image:x y?.png>) [f](<file:../C%23.pdf#page=2>) \
                         \[g](<file:/../100%.ods>)"
    images p `shouldBe` ["/image/src/a%20b/x%20y%3F.png"]
    map snd (links p) `shouldBe` ["/file/src/C%23.pdf#page=2", "/file/src/100%25.ods"]

  it "fixes image and file links" $ do
    let p = load "title: t\n\n[i](<image:a.png>) [j](<image:/b.png>) \
                 \[f](<file:c.ods>) [g](<file:/d.ods>)"
    links p `shouldBe` [ (["kiwi-link-image"], "/image/src/sub/a.png")
                       , (["kiwi-link-image"], "/image/src/b.png")
                       , (["kiwi-link-file"], "/file/src/sub/c.ods")
                       , (["kiwi-link-file"], "/file/src/d.ods") ]
    pandocPageLinks p `shouldBe` []

  it "keeps anchors and external links" $ do
    let p = load "title: t\n\n[a](#section) [b](https://example.org)"
    links p `shouldBe` [ (["kiwi-link-anchor"], "#section")
                       , (["kiwi-link-external"], "https://example.org") ]

  it "keeps the navigation context in page links only" $ do
    let p = load "title: t\n\n[a](<page:Other#part>) [b](#section) [c](https://example.org)"
        inCtx archive = links p { pandocDoc = linksInContext
                                    (Context (S.fromList ["x"]) archive) (pandocDoc p) }
    inCtx False `shouldBe` [ (["kiwi-link-page"], "/page/src/sub:Other?source=x#part")
                           , (["kiwi-link-anchor"], "#section")
                           , (["kiwi-link-external"], "https://example.org") ]
    take 1 (inCtx True)
      `shouldBe` [(["kiwi-link-page"], "/page/src/sub:Other?source=x&archive=yes#part")]

  it "keeps the navigation context before the anchor of a page with a '#'" $ do
    let p = load "title: t\n\n[a](<page:C%23#part>)"
        inCtx = links p { pandocDoc = linksInContext
                            (Context (S.fromList ["x"]) False) (pandocDoc p) }
    inCtx `shouldBe` [(["kiwi-link-page"], "/page/src/sub:C%23?source=x#part")]

  it "reads the archive metadata" $ do
    let archived = metaArchive . pandocMeta . load
    archived "title: t\n\nx" `shouldBe` False
    archived "title: t\narchive: Yes\n\nx" `shouldBe` True
    archived "title: t\narchive: 0\n\nx" `shouldBe` False
    archived "title: t\narchive: oui\n\nx" `shouldBe` False
    invalidMetaValues [] "title: t\narchive: oui\n\narchive: x" `shouldBe` [("archive", "oui")]
    invalidMetaValues [] "title: t\narchive: true\n\narchive: x" `shouldBe` []

  it "finds the invalid lang and typed custom metadata values" $ do
    let cfg = [ CustomMetaConfig "date" CmtDate 1, CustomMetaConfig "count" CmtInt 1
              , CustomMetaConfig "flag" CmtBool 1, CustomMetaConfig "authors" CmtText 1 ]
    invalidMetaValues cfg "lang: fr\ndate: 2020-1-5, 2020-13-1\ncount: 3, x\n\
                          \flag: yes, oui\nauthors: anything\n\nlang: x"
      `shouldBe` [ ("lang", "fr"), ("date", "2020-13-1"), ("count", "x"), ("flag", "oui") ]
    invalidMetaValues cfg "lang: auto/french\ndate: 2020-1-5\ncount: 3\nflag: No\n\n"
      `shouldBe` []
    -- the default lang is used
    metaLang (pandocMeta $ load "title: t\nlang: fr\n\nx") `shouldBe` LangFixed English

  it "removes a byte order mark at the start of the file" $ do
    let p = load "\xFEFFtitle: T\r\ntags: x\r\n\r\nHello"
    metaTitle (pandocMeta p) `shouldBe` "T"
    blocks p `shouldBe` [PD.Para [PD.Str "Hello"]]
    unknownMetaKeys [] "\xFEFFtitle: T\n\nHello" `shouldBe` []

  it "fixes images paths" $ do
    let p = load "title: t\n\n![x](<image:a.png>) ![y](<image:/b.png>) ![z](http://e.org/c.png)"
    images p `shouldBe` ["/image/src/sub/a.png", "/image/src/b.png", "http://e.org/c.png"]

  it "uses images-dir and files-dir metadata" $ do
    let p = load "title: t\nimages-dir: pics\nfiles-dir: docs\n\n\
                 \![x](<image:a.png>) [f](<file:c.ods>)"
    images p `shouldBe` ["/image/src/pics/a.png"]
    links p `shouldBe` [(["kiwi-link-file"], "/file/src/docs/c.ods")]

  it "parses a page again, from its text, to display it" $ do
    let c = "title: t\r\nimages-dir: pics\r\n\r\n\
            \[a](<page:Other>) ![x](<image:a.png>) [f](<file:c.ods>)"
        page = Page { pageUID = ("src", "sub:p")
                    , pageAbsoluteFSPath = "/k/content/src/pages/sub/p.md"
                    , pageMTime = Time.UTCTime (Time.fromGregorian 2026 1 1) 0
                    , pageText = c
                    , pageTitle = "t"
                    , pageTags = []
                    , pageLang = English
                    , pageAccess = []
                    , pageLinks = []
                    , pageArchive = False
                    , pageCustomMeta = []
                    }
    pageDoc "/k/content" page `shouldBe` Right (pandocDoc $ load c)
    pagePath "/k/content" "/k/content/src/pages/sub/p.md" `shouldBe` ("src", "sub/p")

metaDataSpec :: Spec
metaDataSpec = do
  it "reads the access of the sources config, ignoring empty ones" $ do
    let ini = T.unlines [ "[kiwi]", "content-directory: content", "port: 3456"
                        , "ui-lang: english", "[metadata-default]"
                        , "[source a]", "access: family, friends"
                        , "[source b]", "access: ,", "[source c]", "root-tag: x > y" ]
        access = map (\sc -> (scSourceName sc, scAccess sc)) . Conf.sourcesConfig
    fmap access (Conf.parseConfig ini) `shouldBe`
      Right [ ("a", Just ["family", "friends"]), ("b", Nothing), ("c", Nothing) ]
    fmap (map scRootTag . Conf.sourcesConfig) (Conf.parseConfig ini) `shouldBe`
      Right [[], [], ["x", "y"]]

  it "uses the access of the source config, unless the page has one" $ do
    let sc = SourceConfig "src" [] (Just ["family"]) Nothing
        access sM c = case loadPage "test.md" c "src" (sourceMeta sM defMeta) [] "" of
          Left e -> error e
          Right p -> metaAccess $ pandocMeta p
    access (Just sc) "title: t\n\n" `shouldBe` ["family"]
    access (Just sc) "title: t\naccess: public\n\n" `shouldBe` ["public"]
    access (Just sc { scAccess = Nothing }) "title: t\n\n" `shouldBe` ["private"]
    access Nothing "title: t\n\n" `shouldBe` ["private"]

  it "parses metadata lines" $
    parseMetaData "title: a: b\nnot meta\ntags: x\n tags : y "
      `shouldBe` M.fromList [("title", ["a: b"]), ("tags", ["x", "y"])]

  it "parses custom metadata according to their type" $ do
    let cfg = [ CustomMetaConfig "date" CmtDate 1
              , CustomMetaConfig "count" CmtInt 1
              , CustomMetaConfig "flag" CmtBool 1
              , CustomMetaConfig "authors" CmtText 1
              , CustomMetaConfig "absent" CmtText 1 ]
        m = parseMetaData "date: 2020-1-5, 2021-12-31, bad\ncount: 3, x, 12\n\
                          \flag: Yes\nauthors: John, Jack\\, Jr"
        res = [ (cmdName c, cmdKeys c) | c <- parseCustomMeta m cfg ]
    res `shouldBe` [ ("date", [ KeyDay (Time.fromGregorian 2020 1 5)
                              , KeyDay (Time.fromGregorian 2021 12 31) ])
                   , ("count", [KeyInt 3, KeyInt 12])
                   , ("flag", [KeyBool True])
                   , ("authors", [KeyText "John", KeyText "Jack, Jr"]) ]

  it "finds the unknown metadata keys, in the header only" $ do
    let cfg = [CustomMetaConfig "authors" CmtText 1]
        c = "title: t\ntag: typo\nTitle: case\nauthors: a\nfiles-dir: f\n\nbody: no"
    unknownMetaKeys cfg c `shouldBe` ["Title", "tag"]
    unknownMetaKeys [] "title: t\nauthors: a" `shouldBe` ["authors"]

  it "reads booleans" $ do
    let cfg = [CustomMetaConfig "b" CmtBool 1]
        m = parseMetaData "b: true, 1, Yes, no, false, 0, oui"
    -- invalid values are ignored
    map cmdKeys (parseCustomMeta m cfg)
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
      Right (LangAuto Spanish, [Just (LangAuto English), Just (LangFixed French), Nothing])
    fmap langs (Conf.parseConfig $ ini "") `shouldBe`
      Right (LangFixed English, [Just (LangAuto English), Just (LangFixed French), Nothing])
    either (const True) (const False) (Conf.parseConfig $ ini "lang: klingon")
      `shouldBe` True

  it "uses the lang of the source config, unless the page has one" $ do
    let sc = SourceConfig "src" [] Nothing (Just (LangAuto Spanish))
        lang c = case loadPage "test.md" c "src" (sourceMeta (Just sc) defMeta) [] "" of
          Left e -> error e
          Right p -> metaLang $ pandocMeta p
    lang "title: t\n\n" `shouldBe` LangAuto Spanish
    lang "title: t\nlang: auto/french\n\n" `shouldBe` LangAuto French
    lang "title: t\nlang: german\n\n" `shouldBe` LangFixed German
    lang "title: t\nlang: klingon\n\n" `shouldBe` LangAuto Spanish

