module Main (main) where

import           Control.Exception (bracket, try)
import           Data.Char (isAscii, isAlphaNum)
import           Data.IORef (modifyIORef, newIORef, readIORef)
import           Data.List (sort)
import qualified Data.Map as M
import           Data.Maybe (fromMaybe)
import qualified Data.SearchEngine as SE
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as TL
import qualified Data.Time as Time
import           Lucid (renderText)
import qualified Network.HTTP.Types.URI as URI
import qualified System.Directory as D
import qualified System.FilePath as FP
import           System.IO (IOMode (WriteMode), hPutStr, withBinaryFile)
import           System.IO.Error (isAlreadyExistsError)
import           Test.Hspec
import           Test.Hspec.QuickCheck (prop)
import           Test.QuickCheck
import qualified Text.Pandoc.Definition as PD
import qualified Text.Pandoc.Walk as PW

import qualified Kiwi.Auth as Auth
import qualified Kiwi.Browse as Browse
import           Kiwi.Controller (BrowseAll (..), metaIndex, readableQuery)
import           Kiwi.HtmlWriter (writeHtml5)
import qualified Kiwi.ConfigFile as Conf
import           Kiwi.LangDetect (detectLang, resolveLang, stopWords)
import qualified Kiwi.Log as L
import qualified Kiwi.PagesDB as DB
import           Kiwi.Pandoc (linksInContext, loadPage, pageDoc, pagePath, parseMetaData,
                              parseCustomMeta, sourceMeta, unknownMetaKeys,
                              invalidMetaValues)
import           Kiwi.Search (initPageSearchEngine, searchPages, queryTerms)
import qualified Kiwi.Tags as Tags
import           Kiwi.Types
import qualified Kiwi.Url as Url
import qualified Kiwi.Utils as U
import qualified Kiwi.View as V
import qualified Utils.DocIndex as DI


main :: IO ()
main = hspec $ do
  describe "Kiwi.Pandoc.loadPage" loadPageSpec
  describe "Kiwi.Pandoc metadata" metaDataSpec
  describe "Kiwi.Tags" tagsSpec
  describe "Kiwi.Browse" browseReqSpec
  describe "Kiwi.Auth passwords" passwordSpec
  describe "Kiwi.Controller.readableQuery" readableQuerySpec
  describe "Kiwi.Controller.metaIndex" metaIndexSpec
  describe "Utils.DocIndex" docIndexSpec
  describe "Kiwi.LangDetect" langDetectSpec
  describe "Kiwi.Search" searchSpec
  describe "Kiwi.PagesDB.updatePagesDB" pagesDBSpec
  describe "Kiwi.HtmlWriter" htmlWriterSpec


--------------------------------------------------------------------------------
-- Pandoc

defMeta :: MetaData
defMeta = MetaData { metaId = Nothing
                   , metaTitle = "untitled"
                   , metaTags = ["untagged"]
                   , metaAccess = ["private"]
                   , metaLang = LangFixed English
                   , metaArchive = False
                   , metaCustom = []
                   }

-- | Load a page from source "src", located in pages subfolder "sub"
load :: T.Text -> PandocPage
load = loadIn "sub"

loadIn :: FilePath -> T.Text -> PandocPage
loadIn dir c = case loadPage "test.md" c "src" defMeta [] dir of
  Left e -> error e
  Right p -> p

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

  it "reads the archive metadata" $ do
    let archived = metaArchive . pandocMeta . load
    archived "title: t\n\nx" `shouldBe` False
    archived "title: t\narchive: Yes\n\nx" `shouldBe` True
    archived "title: t\narchive: 0\n\nx" `shouldBe` False
    archived "title: t\narchive: oui\n\nx" `shouldBe` False
    invalidMetaValues "title: t\narchive: oui\n\narchive: x" `shouldBe` [("archive", "oui")]
    invalidMetaValues "title: t\narchive: true\n\narchive: x" `shouldBe` []

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
        m = parseMetaData "b: true, 1, yes, no, false, other"
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


--------------------------------------------------------------------------------
-- Lang detection

langDetectSpec :: Spec
langDetectSpec = do
  it "has disjoint lists of words" $ do
    let sets = [ S.fromList ws | (_, ws) <- stopWords ]
    sum (map S.size sets) `shouldBe` S.size (S.unions sets)

  it "detects french, english and spanish" $ do
    detectLang "Le chat est sur le toit, et il n'en descendra pas avant ce soir."
      `shouldBe` Just French
    detectLang "The cat is on the roof, and it will not come down before tonight."
      `shouldBe` Just English
    detectLang "El gato está en el tejado y no bajará hasta la noche, como siempre."
      `shouldBe` Just Spanish

  it "tells french and spanish apart, with accents and elisions" $ do
    detectLang "C'est l'été : qu'il fasse beau, ça m'est égal, j'irai déjà à la plage."
      `shouldBe` Just French
    detectLang "¿Qué es eso? Él dijo que también vendría, pero todavía no está aquí."
      `shouldBe` Just Spanish

  it "gives up on short or mixed texts" $ do
    detectLang "Recette" `shouldBe` Nothing
    detectLang "the cat" `shouldBe` Nothing
    detectLang "The cat and the dog. Le chat et le chien." `shouldBe` Nothing
    detectLang "" `shouldBe` Nothing

  it "resolves the lang of a page from its title and content" $ do
    let fr = "Une recette pour les jours de pluie, avec du chocolat et des noix."
    resolveLang (LangFixed English) "Gâteau" fr `shouldBe` English
    resolveLang (LangAuto English) "Gâteau" fr `shouldBe` French
    resolveLang (LangAuto Spanish) "Cake" "" `shouldBe` Spanish
    resolveLang (LangAuto Spanish) "The joy of the cake and its icing" ""
      `shouldBe` English


--------------------------------------------------------------------------------
-- Tags, browse requests, accounts

tagsSpec :: Spec
tagsSpec = do
  it "splits metadata on commas, with escaping" $
    Tags.splitMeta "a, b\\, c, ,d" `shouldBe` ["a", "b, c", "d"]

  it "splits on several escaped separators" $
    Tags.splitMeta "a\\,b\\,c, d" `shouldBe` ["a,b,c", "d"]

  it "keeps other backslashes" $
    Tags.splitMeta "a\\b, c\\" `shouldBe` ["a\\b", "c\\"]

  it "splits tags without groups as metadata" $
    Tags.splitTags "a, b > c\\, d, ,e \\> f" `shouldBe` ["a", "b > c, d", "e \\> f"]

  it "expands groups of tags" $ do
    Tags.splitTags "sport > {alpinisme, escalade,rando }"
      `shouldBe` ["sport > alpinisme", "sport > escalade", "sport > rando"]
    Tags.splitTags "{sport, loisir} > montagne, a"
      `shouldBe` ["sport > montagne", "loisir > montagne", "a"]

  it "expands nested groups" $
    Tags.splitTags "sport > {montagne > {alpinisme, rando}, natation}"
      `shouldBe` [ "sport > montagne > alpinisme", "sport > montagne > rando"
                 , "sport > natation" ]

  it "drops empty groups and blank tags of groups" $
    Tags.splitTags "sport > {}, a > {b, }, c" `shouldBe` ["a > b", "c"]

  it "keeps escaped braces in tags" $
    Tags.splitTags "a\\{b\\}, c > {d\\,e}" `shouldBe` ["a{b}", "c > d,e"]

  it "takes unbalanced braces literally" $ do
    Tags.splitTags "sport > {a, b" `shouldBe` ["sport > {a", "b"]
    Tags.splitTags "a}, b" `shouldBe` ["a}", "b"]

  it "splits tags in segments" $
    Tags.tagToSegments "loisir > sport>escalade" `shouldBe` ["loisir", "sport", "escalade"]

  it "computes tag sub segments" $
    Tags.tagSubSegments "loisir>sport>escalade"
      `shouldBe` [["loisir"], ["loisir", "sport"], ["loisir", "sport", "escalade"]]

  it "roundtrips segments with escaped >" $ do
    Tags.segmentsToTag ["a>b", "c"] `shouldBe` "a\\>b>c"
    Tags.tagToSegments (Tags.segmentsToTag ["a>b", "c"]) `shouldBe` ["a>b", "c"]

  it "prefixes and normalizes tags" $
    Tags.prefixNormalizeTag ["root"] "a > b" `shouldBe` "root>a>b"

  it "pretty prints tags" $
    Tags.prettyTag "a>b" `shouldBe` "a > b"

  it "keeps only root tags" $
    Tags.filterRootTags [["a"], ["a", "b"], ["c", "d"], ["c", "d", "e"]]
      `shouldBe` [["a"], ["c", "d"]]


browseReqSpec :: Spec
browseReqSpec = do
  let cmcs = [ CustomMetaConfig "author" CmtText 1
             , CustomMetaConfig "year" CmtInt 1
             , CustomMetaConfig "date" CmtDate 1
             , CustomMetaConfig "done" CmtBool 1 ]
      -- decode the query string of an URL, as scotty does
      decodeUrl url = Browse.decodeReq cmcs $ map (fmap $ fromMaybe "") $
                      URI.parseQueryText $ TE.encodeUtf8 $ T.dropWhile (/= '?') url
      req = M.fromList . map (fmap S.fromList)

  it "roundtrips browseUrl / decodeReq" $ do
    let r = req [ (FieldTag, [KeyText "a>b", KeyText "x\\>y", KeyText "é ç/&?=+#%;"])
                , (FieldCustom "author", [KeyText "Smith & Wesson"])
                , (FieldCustom "year", [KeyInt 1999, KeyInt (-3)])
                , (FieldCustom "date", [KeyDay (Time.fromGregorian 2020 2 29)])
                , (FieldCustom "done", [KeyBool True])
                , (FieldAccess, [KeyText "family"])
                , (FieldLang, [KeyLang French])
                , (FieldSource, [KeyText "my source"]) ]
    decodeUrl (Browse.browseUrl noContext r) `shouldBe` Right r
    Browse.browseUrl noContext (req [(FieldLang, [KeyLang French])])
      `shouldBe` "/browse?lang=french"

  it "builds readable URLs, in a stable order" $ do
    Browse.browseUrl noContext M.empty `shouldBe` "/browse"
    Browse.browseUrl noContext (req [ (FieldCustom "done", [KeyBool False])
                     , (FieldTag, [KeyText "maison>produits"])
                     , (FieldSource, [KeyText "a-p"])
                     , (FieldCustom "date", [KeyDay (Time.fromGregorian 2020 2 9)]) ])
      `shouldBe` "/browse?source=a-p&tag=maison%3Eproduits&m.date=2020-02-09&m.done=false"

  it "keeps the archive mode of the context in browse URLs, not its sources" $
    Browse.browseUrl (Context (S.fromList ["x"]) True) (req [(FieldTag, [KeyText "t"])])
      `shouldBe` "/browse?tag=t&archive=yes"

  it "refuses unknown parameters and invalid values" $ do
    let isLeft = either (const True) (const False)
    mapM_ (\url -> decodeUrl url `shouldSatisfy` isLeft)
      [ "/browse?foo=bar", "/browse?author=Smith", "/browse?m.unknown=x"
      , "/browse?m.year=12a", "/browse?m.date=2020-02-30", "/browse?m.done=yes"
      , "/browse?tag=", "/browse?tag=>", "/browse?source="
      , "/browse?lang=klingon", "/browse?access=" ]

  it "normalizes the tags of a request" $ do
    decodeUrl "/browse?tag=maison%20%3E%20produits"
      `shouldBe` Right (req [(FieldTag, [KeyText "maison>produits"])])
    decodeUrl "/browse?tag=a&tag=a%3Eb&tag=a%3Eb%3Ec&tag=ab&tag=b"
      `shouldBe` Right (req [(FieldTag, [KeyText "a>b>c", KeyText "ab", KeyText "b"])])

  it "adds keys to a request, without the parents of a tag" $ do
    Browse.addToReq (Browse.createReq (FieldTag, KeyText "a")) (FieldTag, KeyText "b")
      `shouldBe` req [(FieldTag, [KeyText "a", KeyText "b"])]
    Browse.addToReq (Browse.createReq (FieldTag, KeyText "a")) (FieldTag, KeyText "a>b")
      `shouldBe` req [(FieldTag, [KeyText "a>b"])]
    Browse.addToReq (Browse.createReq (FieldTag, KeyText "a\\>b")) (FieldTag, KeyText "a>b")
      `shouldBe` req [(FieldTag, [KeyText "a\\>b", KeyText "a>b"])]

  it "combines sources with an union, the other fields with an intersection" $
    Browse.reqToQuery (req [ (FieldSource, [KeyText "a", KeyText "b"])
                      , (FieldTag, [KeyText "x", KeyText "y"]) ])
      `shouldBe` DI.And [ DI.Any FieldSource (S.fromList [KeyText "a", KeyText "b"])
                        , DI.All FieldTag (S.fromList [KeyText "x", KeyText "y"]) ]

  it "adds the navigation context to URLs, before the fragment" $ do
    let ss = S.fromList ["b c", "a"]
    Url.inContext noContext "/page/s/p#h" `shouldBe` "/page/s/p#h"
    Url.inContext (Context ss False) "/page/s/p#h"
      `shouldBe` "/page/s/p?source=a&source=b%20c#h"
    Url.inContext (Context ss True) "/page/s/p"
      `shouldBe` "/page/s/p?source=a&source=b%20c&archive=yes"
    Url.inContext (Context S.empty True) "/metadata" `shouldBe` "/metadata?archive=yes"
    Url.inContext (Context ss True) "/browse"
      `shouldBe` Browse.browseUrl (Context ss True) (Browse.sourcesReq ss)

  it "removes keys from a request" $ do
    let r = req [(FieldTag, [KeyText "a", KeyText "b"]), (FieldSource, [KeyText "s"])]
    Browse.removeFromReq r (FieldTag, KeyText "a")
      `shouldBe` req [(FieldTag, [KeyText "b"]), (FieldSource, [KeyText "s"])]
    Browse.removeFromReq r (FieldSource, KeyText "s")
      `shouldBe` req [(FieldTag, [KeyText "a", KeyText "b"])]


passwordSpec :: Spec
passwordSpec = do
  let salt = "0123456789abcdef"
      hash = fromMaybe (error "hashPassword failed") $ Auth.hashPassword salt "secret"
      accounts = [Account "paul" salt hash ["admin", "family"]]

  it "hashes passwords deterministically, as hex" $ do
    Auth.hashPassword salt "secret" `shouldBe` Just hash
    T.length hash `shouldBe` 64
    Auth.hashPassword salt "other" `shouldNotBe` Just hash
    Auth.hashPassword "fedcba9876543210" "secret" `shouldNotBe` Just hash

  it "refuses salts shorter than 8 bytes" $
    Auth.hashPassword "short" "secret" `shouldBe` Nothing

  it "verifies logins" $ do
    Auth.verifyLogin accounts "paul" "secret" `shouldBe` Just ("paul", ["admin", "family"])
    Auth.verifyLogin accounts "paul" "wrong" `shouldBe` Nothing
    Auth.verifyLogin accounts "nobody" "secret" `shouldBe` Nothing

  it "generates random alphanumeric texts" $ do
    t0 <- Auth.randomText 0
    t0 `shouldBe` ""
    t1 <- Auth.randomText 128
    t2 <- Auth.randomText 128
    T.length t1 `shouldBe` 128
    T.all (\c -> isAscii c && isAlphaNum c) t1 `shouldBe` True
    t1 `shouldNotBe` t2


--------------------------------------------------------------------------------
-- Access control and browse page

-- | A page of a source, with its tags, access groups, archive flag, and
-- modification day (in january 2026)
mkPage :: PageUID -> [TagId] -> [T.Text] -> Bool -> Int -> Page
mkPage uid tags access archive day =
  Page { pageUID = uid
       , pageAbsoluteFSPath = "/dev/null"
       , pageMTime = Time.UTCTime (Time.fromGregorian 2026 1 day) 0
       , pageText = ""
       , pageTitle = snd uid
       , pageTags = tags
       , pageLang = English
       , pageAccess = access
       , pageLinks = []
       , pageArchive = archive
       , pageCustomMeta = []
       }

-- | Database of these pages, indexed as by 'DB.updatePagesDB'
mkPagesDB :: [Page] -> PagesDB
mkPagesDB pages = let db = DB.emptyPagesDB defMeta [] []
                  in db { pagesIndex = DI.insertMany (pagesIndex db) pages }

readableQuerySpec :: Spec
readableQuerySpec = do
  let pages = [ mkPage ("s", "public") [] ["public"] False 1
              , mkPage ("s", "family") [] ["family"] False 1
              , mkPage ("s", "admin") [] ["admin"] False 1
              , mkPage ("s", "both") [] ["family", "friends"] False 1
              , mkPage ("s", "old-public") [] ["public"] True 1
              , mkPage ("s", "old-family") [] ["family"] True 1
              , mkPage ("s", "old-admin") [] ["admin"] True 1 ]
      idx = pagesIndex $ mkPagesDB pages
      t0 = Time.UTCTime (Time.fromGregorian 2026 1 1) 0
      user groups = Just ("user", groups, t0)
      readable loginM archive =
        sort $ map snd $ S.toList $ DI.queryIdSet idx $ readableQuery loginM archive

  it "gives anonymous users the public pages only" $ do
    readable Nothing False `shouldBe` ["public"]
    readable Nothing True `shouldBe` ["old-public"]

  it "gives logged in users the public pages and those of their groups" $ do
    readable (user ["family"]) False `shouldBe` ["both", "family", "public"]
    readable (user ["friends"]) False `shouldBe` ["both", "public"]
    readable (user []) False `shouldBe` ["public"]
    readable (user ["family"]) True `shouldBe` ["old-family", "old-public"]

  it "gives admins every page" $ do
    readable (user ["admin"]) False `shouldBe` ["admin", "both", "family", "public"]
    readable (user ["family", "admin"]) True
      `shouldBe` ["old-admin", "old-family", "old-public"]

  it "separates the archives from the other pages" $
    mapM_ (\loginM -> do
              readable loginM False `shouldSatisfy` all (not . T.isPrefixOf "old-")
              readable loginM True `shouldSatisfy` all (T.isPrefixOf "old-"))
      [Nothing, user ["family"], user ["admin"]]


metaIndexSpec :: Spec
metaIndexSpec = do
  let db = mkPagesDB [ mkPage ("s1", "a") ["sport>ski"] ["public"] False 1
                     , mkPage ("s1", "b") ["sport>escalade", "maison"] ["public"] False 2
                     , mkPage ("s2", "c") ["maison"] ["public"] False 3 ]
      tag t = (FieldTag, KeyText t)
      source s = (FieldSource, KeyText s)
      uids = map pageUID . baPages

  it "lists the latest pages, and the root tags, without request" $ do
    let ba = metaIndex db noContext M.empty
    baToggles ba `shouldBe`
      [ (FieldSource, [ V.Toggle "s1" "/browse?source=s1" False
                      , V.Toggle "s2" "/browse?source=s2" False ]) ]
    baNextStep ba `shouldBe`
      [(FieldTag, [("maison", "/browse?tag=maison"), ("sport", "/browse?tag=sport")])]
    baActive ba `shouldBe` []
    baListing ba `shouldBe` V.RecentPages
    uids ba `shouldBe` [("s2", "c"), ("s1", "b"), ("s1", "a")]

  it "narrows the request with the next tags, and lists all the matching pages" $ do
    let ba = metaIndex db noContext (Browse.createReq $ tag "sport")
    baToggles ba `shouldBe`
      [ (FieldSource, [ V.Toggle "s1" "/browse?source=s1&tag=sport" False
                      , V.Toggle "s2" "/browse?source=s2&tag=sport" False ]) ]
    baNextStep ba `shouldBe`
      [(FieldTag, [ ("maison", "/browse?tag=maison&tag=sport")
                  , ("> escalade", "/browse?tag=sport%3Eescalade")
                  , ("> ski", "/browse?tag=sport%3Eski") ])]
    baActive ba `shouldBe` [(FieldTag, "sport", "/browse")]
    baListing ba `shouldBe` V.AllPages
    uids ba `shouldBe` [("s1", "b"), ("s1", "a")]

  it "toggles the sources, and keeps the archive mode in the links" $ do
    let ctx = Context S.empty True
        ba = metaIndex db ctx (Browse.createReq $ source "s2")
    baToggles ba `shouldBe`
      [ (FieldSource, [ V.Toggle "s1" "/browse?source=s1&source=s2&archive=yes" False
                      , V.Toggle "s2" "/browse?archive=yes" True ]) ]
    baNextStep ba `shouldBe`
      [(FieldTag, [("maison", "/browse?source=s2&tag=maison&archive=yes")])]
    baListing ba `shouldBe` V.RecentPages
    uids ba `shouldBe` [("s2", "c")]


--------------------------------------------------------------------------------
-- DocIndex

-- | (id, tags, year)
type Doc = (Int, [T.Text], Int)

type DocQuery = DI.Query String DocIndexKey

docId :: Doc -> Int
docId (i, _, _) = i

-- | Keys of the fields "tag" and "year", none for the other fields
docKeys :: String -> Doc -> [DocIndexKey]
docKeys "tag" (_, ts, _) = map KeyText ts
docKeys "year" (_, _, y) = [KeyInt y]
docKeys _ _ = []

docsIndex :: [Doc] -> DI.DocIndex Doc Int String DocIndexKey
docsIndex = DI.insertMany $ DI.empty docId
  [ DI.Field "tag" (docKeys "tag"), DI.Field "year" (docKeys "year") ]

-- | Reference semantics of a query, on a single document
matches :: DocQuery -> Doc -> Bool
matches q d = case q of
  DI.And [] -> False
  DI.And qs -> all (`matches` d) qs
  DI.Or qs -> any (`matches` d) qs
  DI.Only f k -> k `elem` docKeys f d
  DI.All f ks -> not (S.null ks) && all (`elem` docKeys f d) ks
  DI.Any f ks -> any (`elem` docKeys f d) ks
  DI.HasField f -> not $ null $ docKeys f d
  DI.Range f k1 k2 -> any (\k -> k1 <= k && k <= k2) $ docKeys f d
  DI.GreaterThanOrEq f k -> any (>= k) $ docKeys f d
  DI.LowerThanOrEq f k -> any (<= k) $ docKeys f d

-- | Documents with few ids and keys, so that they often share them, and with
-- the same id sometimes
genDocs :: Gen [Doc]
genDocs = listOf $ (,,) <$> choose (1, 12) <*> listOf genTag <*> choose (2018, 2023)

genTag :: Gen T.Text
genTag = elements ["a", "b", "c", "d"]

-- | Queries on the fields of the documents, an unknown field, an unknown tag
-- ("z") and years without documents
genQuery :: Int -> Gen DocQuery
genQuery n = oneof $ leaves ++ if n > 0 then [ DI.And <$> subQueries
                                              , DI.Or <$> subQueries ] else []
  where
    subQueries = resize 3 $ listOf $ genQuery (n `div` 2)
    tag = KeyText <$> oneof [genTag, pure "z"]
    year = KeyInt <$> choose (2017, 2024)
    leaves = [ DI.Only "tag" <$> tag
             , DI.Only "year" <$> year
             , DI.All "tag" . S.fromList <$> resize 3 (listOf tag)
             , DI.Any "tag" . S.fromList <$> resize 3 (listOf tag)
             , DI.HasField <$> elements ["tag", "year", "none"]
             , DI.Range "year" <$> year <*> year
             , DI.GreaterThanOrEq "year" <$> year
             , DI.LowerThanOrEq "year" <$> year ]

-- | The documents of the index built from these ones: the last one with each id
storedDocs :: [Doc] -> [Doc]
storedDocs ds = M.elems $ M.fromList [ (docId d, d) | d <- ds ]

-- | Same documents and same index (the index only depends on the documents)
sameIndex :: DI.DocIndex Doc Int String DocIndexKey -> [Doc] -> Property
sameIndex idx ds = (DI.documentsList idx, DI.index idx)
                   === (ds, DI.index $ docsIndex ds)

sampleDocs :: [Doc]
sampleDocs = [ (1, ["a", "b"], 2019)
             , (2, ["a"], 2020)
             , (3, ["c"], 2021)
             , (4, [], 2022) ]

ids :: DI.DocIndex Doc Int String DocIndexKey -> DI.Query String DocIndexKey -> [Int]
ids idx q = S.toList $ DI.queryIdSet idx q

docIndexSpec :: Spec
docIndexSpec = do
  let idx = docsIndex sampleDocs
      tag t = DI.Only "tag" (KeyText t)
      year y = KeyInt y

  it "stores and finds documents" $ do
    DI.cardinal idx `shouldBe` 4
    DI.findDoc idx 2 `shouldBe` Just (2, ["a"], 2020)
    DI.findDoc idx 5 `shouldBe` Nothing

  it "indexes field keys" $
    DI.getFieldKeys idx "tag" `shouldBe` map KeyText ["a", "b", "c"]

  it "runs And / Or / All / Any queries" $ do
    ids idx (tag "a") `shouldBe` [1, 2]
    ids idx (DI.And [tag "a", tag "b"]) `shouldBe` [1]
    ids idx (DI.Or [tag "b", tag "c"]) `shouldBe` [1, 3]
    ids idx (DI.All "tag" $ S.fromList [KeyText "a", KeyText "b"]) `shouldBe` [1]
    ids idx (DI.Any "tag" $ S.fromList [KeyText "b", KeyText "c"]) `shouldBe` [1, 3]
    ids idx (DI.HasField "tag") `shouldBe` [1, 2, 3]
    ids idx (DI.And []) `shouldBe` []
    ids idx (DI.All "tag" S.empty) `shouldBe` []

  it "matches no document with All and a missing key" $ do
    ids idx (DI.All "tag" $ S.fromList [KeyText "a", KeyText "z"]) `shouldBe` []
    ids idx (DI.Any "tag" $ S.fromList [KeyText "a", KeyText "z"]) `shouldBe` [1, 2]

  it "returns the documents by id" $
    DI.query idx (DI.Or [tag "c", tag "a"]) `shouldBe`
      [(1, ["a", "b"], 2019), (2, ["a"], 2020), (3, ["c"], 2021)]

  it "runs range queries, bounds included" $ do
    ids idx (DI.Range "year" (year 2020) (year 2021)) `shouldBe` [2, 3]
    ids idx (DI.Range "year" (year 2019) (year 2019)) `shouldBe` [1]
    ids idx (DI.GreaterThanOrEq "year" (year 2021)) `shouldBe` [3, 4]
    ids idx (DI.LowerThanOrEq "year" (year 2020)) `shouldBe` [1, 2]
    ids idx (DI.Range "year" (year 2021) (year 2020)) `shouldBe` []
    ids idx (DI.Range "none" (year 2019) (year 2022)) `shouldBe` []

  it "removes documents and their keys" $ do
    let idx' = DI.removeMany idx [1, 3, 5]
    DI.cardinal idx' `shouldBe` 2
    ids idx' (tag "a") `shouldBe` [2]
    DI.getFieldKeys idx' "tag" `shouldBe` [KeyText "a"]

  it "removes the fields without keys" $ do
    DI.getFields (DI.removeMany idx [1, 2, 3]) `shouldBe` ["year"]
    DI.getFields (DI.narrow idx $ DI.Only "year" $ year 2022) `shouldBe` ["year"]

  it "replaces a document inserted twice" $ do
    let idx' = DI.insert idx (2, ["z"], 2020)
    DI.cardinal idx' `shouldBe` 4
    ids idx' (tag "a") `shouldBe` [1]
    ids idx' (tag "z") `shouldBe` [2]

  it "narrows an index to a query" $ do
    let idx' = DI.narrow idx (tag "a")
    DI.cardinal idx' `shouldBe` 2
    DI.getFieldKeys idx' "tag" `shouldBe` map KeyText ["a", "b"]
    ids idx' (DI.HasField "year") `shouldBe` [1, 2]

  prop "runs queries as the reference semantics" $
    forAll genDocs $ \ds -> forAll (genQuery 4) $ \q ->
      ids (docsIndex ds) q === [ docId d | d <- storedDocs ds, matches q d ]

  prop "narrows as if the matching documents were indexed again" $
    forAll genDocs $ \ds -> forAll (genQuery 4) $ \q ->
      sameIndex (DI.narrow (docsIndex ds) q) (filter (matches q) $ storedDocs ds)

  prop "removes as if the other documents were indexed again" $
    forAll genDocs $ \ds -> forAll (listOf $ choose (1, 12)) $ \removed ->
      sameIndex (DI.removeMany (docsIndex ds) removed)
                [ d | d <- storedDocs ds, docId d `notElem` removed ]


--------------------------------------------------------------------------------
-- Search

searchSpec :: Spec
searchSpec = do
  let page = pageIn French
      -- a page and the plain text of its content
      pageIn lang uid content =
        ( Page { pageUID = ("src", uid)
               , pageAbsoluteFSPath = "/dev/null"
               , pageMTime = Time.UTCTime (Time.fromGregorian 2026 1 1) 0
               , pageText = content
               , pageTitle = uid
               , pageTags = []
               , pageLang = lang
               , pageAccess = []
               , pageLinks = []
               , pageArchive = False
               , pageCustomMeta = []
               }
        , content )
      se = SE.insertDocs [ page "a" "Un développement en été"
                         , page "b" "Elle est aimée"
                         , page "c" "Lui et moi"
                         , pageIn Spanish "d" "Los niños corrían"
                         , page "e" "Le chat dort au soleil du jardin"
                         , page "f" "Le chat mange"
                         , page "g" "Le soleil brille" ]
                         initPageSearchEngine
      search q = map snd $ SE.query se (queryTerms French q)
      searchVisible visible = sort . searchPages se (visible . snd) . queryTerms French

  it "matches inflections of accented words" $
    search "développer" `shouldBe` ["a"]

  it "finds an accented word typed with its accents" $
    search "aimée" `shouldBe` ["b"]

  it "does not confuse accented words with short ones" $
    search "été" `shouldBe` ["a"]

  it "stems the query in the lang of its prefix" $ do
    search "corriendo" `shouldBe` []
    search "es:corriendo" `shouldBe` ["d"]
    search " ES: corriendo" `shouldBe` ["d"]

  it "keeps only the pages with a score close to the best one" $ do
    -- without threshold: e first, then f and g with one of the terms
    take 1 (search "chat soleil jardin") `shouldBe` ["e"]
    sort (search "chat soleil jardin") `shouldBe` ["e", "f", "g"]
    searchVisible (const True) "chat soleil jardin" `shouldBe` [("src", "e")]

  it "compares the scores of the visible pages only" $
    searchVisible (/= "e") "chat soleil jardin" `shouldBe` [("src", "f"), ("src", "g")]

  it "keeps unknown prefixes, and prefixes after the start, in the query" $ do
    queryTerms French "fr:chevaux" `shouldBe` queryTerms French "chevaux"
    queryTerms French "es:canciones" `shouldBe` queryTerms Spanish "canciones"
    queryTerms French "xx:chevaux" `shouldBe` ["xx:cheval"]
    queryTerms French "es:" `shouldBe` []
    queryTerms French "chevaux es:x" `shouldBe` queryTerms French "chevaux" ++ ["es:x"]


--------------------------------------------------------------------------------
-- PagesDB

-- | Runs the test on the content folder of a new temporary kiwi folder,
-- removed afterwards
withContentDir :: (FilePath -> IO a) -> IO a
withContentDir test = do
  tmp <- D.getTemporaryDirectory
  bracket (newDir tmp (0 :: Int)) D.removeDirectoryRecursive $ \d -> do
    let cd = d FP.</> "content"
    D.createDirectoryIfMissing True (cd FP.</> "s" FP.</> "pages")
    test cd
  where
    newDir tmp n = do
      let d = tmp FP.</> ("kiwi-test-" ++ show n)
      r <- try $ D.createDirectory d
      case r of
        Right () -> return d
        Left e | isAlreadyExistsError e -> newDir tmp (n + 1)
               | otherwise -> ioError e

-- | Path of a file of the pages of source "s"
pageFile :: FilePath -> FilePath -> FilePath
pageFile cd name = cd FP.</> "s" FP.</> "pages" FP.</> name

-- | Writes the file, with a modification time of the given second
writeFileAt :: FilePath -> Integer -> String -> IO ()
writeFileAt p sec c = do
  D.createDirectoryIfMissing True (FP.takeDirectory p)
  writeFile p c
  D.setModificationTime p (Time.UTCTime (Time.fromGregorian 2026 1 1)
                                        (Time.secondsToDiffTime sec))

-- | The database updated by 'DB.updatePagesDB', and the messages logged
reloadDB :: FilePath -> PagesDB -> IO (PagesDB, [(L.LogLevel, T.Text)])
reloadDB cd db = do
  logR <- newIORef []
  db' <- DB.updatePagesDB (\lvl msg -> modifyIORef logR ((lvl, msg) :)) cd "pages" db
  msgs <- reverse <$> readIORef logR
  return (db', msgs)

loadDB :: FilePath -> IO (PagesDB, [(L.LogLevel, T.Text)])
loadDB cd = reloadDB cd (DB.emptyPagesDB defMeta [] [])

-- | Ids of the pages of the index, which must be those of the search engine:
-- every test page has the word "kiwiword"
dbIds :: PagesDB -> IO [T.Text]
dbIds db = do
  let ids' = sort $ map (snd . pageUID) $ DI.documentsList $ pagesIndex db
  sort (map snd $ dbSearch db "kiwiword") `shouldBe` ids'
  return ids'

dbSearch :: PagesDB -> T.Text -> [PageUID]
dbSearch db w = SE.query (searchEngine db) (queryTerms English w)

-- | File path of the page
dbPagePath :: PagesDB -> T.Text -> Maybe FilePath
dbPagePath db pId = pageAbsoluteFSPath <$> DI.findDoc (pagesIndex db) ("s", pId)

warnings :: [(L.LogLevel, T.Text)] -> [T.Text]
warnings msgs = [ m | (L.Warn, m) <- msgs ]

pagesDBSpec :: Spec
pagesDBSpec = do
  it "loads the pages, with the id of their path or of their id field" $
    withContentDir $ \cd -> do
      writeFileAt (pageFile cd "a.md") 0 "title: A\n\nkiwiword"
      writeFileAt (pageFile cd "sub/b.md") 0 "title: B\n\nkiwiword"
      writeFileAt (pageFile cd "c.md") 0 "id: custom\n\nkiwiword"
      writeFileAt (pageFile cd "d.md") 0 "id:  \n\nkiwiword"
      (db, msgs) <- loadDB cd
      dbIds db `shouldReturn` ["a", "custom", "d", "sub:b"]
      warnings msgs `shouldBe` []

  it "does not reload unchanged files" $
    withContentDir $ \cd -> do
      writeFileAt (pageFile cd "a.md") 0 "title: A\n\nkiwiword"
      (db, _) <- loadDB cd
      (db', msgs) <- reloadDB cd db
      dbIds db' `shouldReturn` ["a"]
      msgs `shouldBe` []

  it "loads a renamed file, whose time is kept" $
    withContentDir $ \cd -> do
      writeFileAt (pageFile cd "a.md") 0 "title: A\n\nkiwiword"
      (db, _) <- loadDB cd
      D.renameFile (pageFile cd "a.md") (pageFile cd "b.md")
      (db', _) <- reloadDB cd db
      dbIds db' `shouldReturn` ["b"]

  it "loads a new file with an old time" $
    withContentDir $ \cd -> do
      writeFileAt (pageFile cd "a.md") 100 "title: A\n\nkiwiword"
      (db, _) <- loadDB cd
      writeFileAt (pageFile cd "b.md") 0 "title: B\n\nkiwiword"
      (db', _) <- reloadDB cd db
      dbIds db' `shouldReturn` ["a", "b"]

  it "reloads a file whose time goes back" $
    withContentDir $ \cd -> do
      writeFileAt (pageFile cd "a.md") 100 "title: A\n\nkiwiword oldword"
      (db, _) <- loadDB cd
      writeFileAt (pageFile cd "a.md") 50 "title: A\n\nkiwiword newword"
      (db', _) <- reloadDB cd db
      dbIds db' `shouldReturn` ["a"]
      dbSearch db' "newword" `shouldBe` [("s", "a")]
      dbSearch db' "oldword" `shouldBe` []

  it "removes the page of a file whose id changes" $
    withContentDir $ \cd -> do
      writeFileAt (pageFile cd "a.md") 0 "id: one\n\nkiwiword"
      (db, _) <- loadDB cd
      writeFileAt (pageFile cd "a.md") 1 "id: two\n\nkiwiword"
      (db', _) <- reloadDB cd db
      dbIds db' `shouldReturn` ["two"]

  it "removes the page of a file that can no longer be loaded" $
    withContentDir $ \cd -> do
      writeFileAt (pageFile cd "a.md") 0 "title: A\n\nkiwiword"
      (db, _) <- loadDB cd
      withBinaryFile (pageFile cd "a.md") WriteMode $ \h -> hPutStr h "\xff\xfe"
      D.setModificationTime (pageFile cd "a.md") (Time.UTCTime (Time.fromGregorian 2026 1 2) 0)
      (db', msgs) <- reloadDB cd db
      dbIds db' `shouldReturn` []
      warnings msgs `shouldSatisfy` any (T.isPrefixOf "page ignored, ")

  it "removes the page of a removed file" $
    withContentDir $ \cd -> do
      writeFileAt (pageFile cd "a.md") 0 "title: A\n\nkiwiword"
      writeFileAt (pageFile cd "b.md") 0 "title: B\n\nkiwiword"
      (db, _) <- loadDB cd
      D.removeFile (pageFile cd "a.md")
      (db', _) <- reloadDB cd db
      dbIds db' `shouldReturn` ["b"]

  it "uses the first file of a duplicate page id, at each reload, and warns" $
    withContentDir $ \cd -> do
      -- "a.markdown" comes before "a.md"
      writeFileAt (pageFile cd "a.md") 0 "title: A\n\nkiwiword"
      writeFileAt (pageFile cd "a.markdown") 0 "title: A\n\nkiwiword"
      (db, msgs) <- loadDB cd
      dbIds db `shouldReturn` ["a"]
      dbPagePath db "a" `shouldBe` Just (pageFile cd "a.markdown")
      warnings msgs `shouldBe` [ T.pack $ concat [ "duplicate page id s:a, "
                                                 , pageFile cd "a.md", " ignored, "
                                                 , pageFile cd "a.markdown", " used" ] ]
      (db', msgs') <- reloadDB cd db
      dbPagePath db' "a" `shouldBe` Just (pageFile cd "a.markdown")
      warnings msgs' `shouldBe` warnings msgs
      -- the other file is used once the first one is removed
      D.removeFile (pageFile cd "a.markdown")
      (db'', msgs'') <- reloadDB cd db'
      dbIds db'' `shouldReturn` ["a"]
      dbPagePath db'' "a" `shouldBe` Just (pageFile cd "a.md")
      warnings msgs'' `shouldBe` []

  it "replaces a loaded page by a new file of the same id, if it comes first" $
    withContentDir $ \cd -> do
      writeFileAt (pageFile cd "b.md") 0 "id: x\n\nkiwiword bword"
      (db, _) <- loadDB cd
      writeFileAt (pageFile cd "a.md") 0 "id: x\n\nkiwiword aword"
      (db', msgs) <- reloadDB cd db
      dbIds db' `shouldReturn` ["x"]
      dbPagePath db' "x" `shouldBe` Just (pageFile cd "a.md")
      dbSearch db' "aword" `shouldBe` [("s", "x")]
      dbSearch db' "bword" `shouldBe` []
      length (warnings msgs) `shouldBe` 1

  it "warns about an id with a '/'" $
    withContentDir $ \cd -> do
      writeFileAt (pageFile cd "a.md") 0 "id: x/y\n\nkiwiword"
      (db, msgs) <- loadDB cd
      dbIds db `shouldReturn` ["x/y"]
      warnings msgs `shouldBe` [T.pack $ "page id with a '/', " ++ pageFile cd "a.md" ++ " : x/y"]

  it "ignores the files whose name is not the one of a page, in debug" $
    withContentDir $ \cd -> do
      writeFileAt (pageFile cd "_draft.md") 0 "title: A\n\nkiwiword"
      writeFileAt (pageFile cd "notes.txt") 0 "title: A\n\nkiwiword"
      (db, msgs) <- loadDB cd
      dbIds db `shouldReturn` []
      length [ m | (L.Debug, m) <- msgs, T.isPrefixOf "file ignored, " m ] `shouldBe` 2


--------------------------------------------------------------------------------
-- HtmlWriter

htmlWriterSpec :: Spec
htmlWriterSpec = do
  let html = TL.toStrict . renderText . writeHtml5 . pandocDoc . load

  it "renders footnotes as numbered references and a list at the end" $
    html "t: t\n\nA[^x] and B[^y].\n\n[^x]: Note x.\n\n[^y]: Note y."
      `shouldBe` T.concat
        [ "<p>A<a href=\"#fn1\" id=\"fnref1\" class=\"footnote-ref\">1</a>"
        , " and B<a href=\"#fn2\" id=\"fnref2\" class=\"footnote-ref\">2</a>.</p>"
        , "<section class=\"footnotes\"><ol>"
        , "<li id=\"fn1\"><p>Note x. <a href=\"#fnref1\" class=\"footnote-back\">↑</a></p></li>"
        , "<li id=\"fn2\"><p>Note y. <a href=\"#fnref2\" class=\"footnote-back\">↑</a></p></li>"
        , "</ol></section>" ]

  it "renders no footnotes section without notes" $
    html "t: t\n\nNo note." `shouldBe` "<p>No note.</p>"

  it "renders a lone image as a figure, captioned by its description" $
    html "t: t\n\n![A *nice* `cat`](http://x/cat.png \"Cat\")"
      `shouldBe` T.concat
        [ "<figure><img src=\"http://x/cat.png\" alt=\"\" title=\"Cat\">"
        , "<figcaption>A <em>nice</em> <code>cat</code></figcaption></figure>" ]

  it "renders no caption for a lone image without description" $
    html "t: t\n\n![](http://x/cat.png)"
      `shouldBe` "<figure><img src=\"http://x/cat.png\" alt=\"\"></figure>"

  it "gives an inline image the full text of its description as alt" $
    html "t: t\n\nSee ![A *nice* `cat`](http://x/cat.png)."
      `shouldBe` "<p>See <img src=\"http://x/cat.png\" alt=\"A nice cat\">.</p>"

  it "aligns table cells with classes" $ do
    let t = html "t: t\n\n| a | b | c |\n|:--|:-:|--:|\n| 1 | 2 | 3 |\n"
    t `shouldSatisfy` T.isInfixOf "<th scope=\"col\" class=\"align-left\">a</th>"
    t `shouldSatisfy` T.isInfixOf "<td class=\"align-left\">1</td>"
    t `shouldSatisfy` T.isInfixOf "<td class=\"align-center\">2</td>"
    t `shouldSatisfy` T.isInfixOf "<td class=\"align-right\">3</td>"
    t `shouldSatisfy` (not . T.isInfixOf "style=")
