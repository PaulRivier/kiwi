module Main (main) where

import           Data.Char (isAscii, isAlphaNum)
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
import           Test.Hspec
import           Test.Hspec.QuickCheck (prop)
import           Test.QuickCheck
import qualified Text.Pandoc.Definition as PD
import qualified Text.Pandoc.Walk as PW

import           Kiwi.HtmlWriter (writeHtml5)
import qualified Kiwi.ConfigFile as Conf
import           Kiwi.LangDetect (detectLang, resolveLang, stopWords)
import           Kiwi.Pandoc (linksInContext, loadPage, pageDoc, pagePath, parseMetaData,
                              parseCustomMeta, sourceMeta)
import           Kiwi.Search (initPageSearchEngine, searchPages, queryTerms)
import           Kiwi.Types
import qualified Kiwi.Utils as U
import qualified Utils.DocIndex as DI


main :: IO ()
main = hspec $ do
  describe "Kiwi.Pandoc.loadPage" loadPageSpec
  describe "Kiwi.Pandoc metadata" metaDataSpec
  describe "Kiwi.Utils tags" tagsSpec
  describe "Kiwi.Utils browse requests" browseReqSpec
  describe "Kiwi.Utils passwords" passwordSpec
  describe "Utils.DocIndex" docIndexSpec
  describe "Kiwi.LangDetect" langDetectSpec
  describe "Kiwi.Search" searchSpec
  describe "Kiwi.HtmlWriter" htmlWriterSpec


--------------------------------------------------------------------------------
-- Pandoc

defMeta :: MetaData
defMeta = MetaData { metaId = Nothing
                   , metaTitle = "untitled"
                   , metaTags = ["untagged"]
                   , metaAccess = ["private"]
                   , metaLang = LangFixed English
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

  it "keeps the sources context in page links only" $ do
    let p = load "title: t\n\n[a](<page:Other#part>) [b](#section) [c](https://example.org)"
        p' = p { pandocDoc = linksInContext (S.fromList ["x"]) (pandocDoc p) }
    links p' `shouldBe` [ (["kiwi-link-page"], "/page/src/sub:Other?source=x#part")
                        , (["kiwi-link-anchor"], "#section")
                        , (["kiwi-link-external"], "https://example.org") ]

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
-- Utils

tagsSpec :: Spec
tagsSpec = do
  it "splits metadata on commas, with escaping" $
    U.splitMeta "a, b\\, c, ,d" `shouldBe` ["a", "b, c", "d"]

  it "splits on several escaped separators" $
    U.splitMeta "a\\,b\\,c, d" `shouldBe` ["a,b,c", "d"]

  it "keeps other backslashes" $
    U.splitMeta "a\\b, c\\" `shouldBe` ["a\\b", "c\\"]

  it "splits tags without groups as metadata" $
    U.splitTags "a, b > c\\, d, ,e \\> f" `shouldBe` ["a", "b > c, d", "e \\> f"]

  it "expands groups of tags" $ do
    U.splitTags "sport > {alpinisme, escalade,rando }"
      `shouldBe` ["sport > alpinisme", "sport > escalade", "sport > rando"]
    U.splitTags "{sport, loisir} > montagne, a"
      `shouldBe` ["sport > montagne", "loisir > montagne", "a"]

  it "expands nested groups" $
    U.splitTags "sport > {montagne > {alpinisme, rando}, natation}"
      `shouldBe` [ "sport > montagne > alpinisme", "sport > montagne > rando"
                 , "sport > natation" ]

  it "drops empty groups and blank tags of groups" $
    U.splitTags "sport > {}, a > {b, }, c" `shouldBe` ["a > b", "c"]

  it "keeps escaped braces in tags" $
    U.splitTags "a\\{b\\}, c > {d\\,e}" `shouldBe` ["a{b}", "c > d,e"]

  it "takes unbalanced braces literally" $ do
    U.splitTags "sport > {a, b" `shouldBe` ["sport > {a", "b"]
    U.splitTags "a}, b" `shouldBe` ["a}", "b"]

  it "splits tags in segments" $
    U.tagToSegments "loisir > sport>escalade" `shouldBe` ["loisir", "sport", "escalade"]

  it "computes tag sub segments" $
    U.tagSubSegments "loisir>sport>escalade"
      `shouldBe` [["loisir"], ["loisir", "sport"], ["loisir", "sport", "escalade"]]

  it "roundtrips segments with escaped >" $ do
    U.segmentsToTag ["a>b", "c"] `shouldBe` "a\\>b>c"
    U.tagToSegments (U.segmentsToTag ["a>b", "c"]) `shouldBe` ["a>b", "c"]

  it "prefixes and normalizes tags" $
    U.prefixNormalizeTag ["root"] "a > b" `shouldBe` "root>a>b"

  it "pretty prints tags" $
    U.prettyTag "a>b" `shouldBe` "a > b"

  it "keeps only root tags" $
    U.filterRootTags [["a"], ["a", "b"], ["c", "d"], ["c", "d", "e"]]
      `shouldBe` [["a"], ["c", "d"]]


browseReqSpec :: Spec
browseReqSpec = do
  let cmcs = [ CustomMetaConfig "author" CmtText 1
             , CustomMetaConfig "year" CmtInt 1
             , CustomMetaConfig "date" CmtDate 1
             , CustomMetaConfig "done" CmtBool 1 ]
      -- decode the query string of an URL, as scotty does
      decodeUrl url = U.decodeReq cmcs $ map (fmap $ fromMaybe "") $
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
    decodeUrl (U.browseUrl r) `shouldBe` Right r
    U.browseUrl (req [(FieldLang, [KeyLang French])]) `shouldBe` "/browse?lang=french"

  it "builds readable URLs, in a stable order" $ do
    U.browseUrl M.empty `shouldBe` "/browse"
    U.browseUrl (req [ (FieldCustom "done", [KeyBool False])
                     , (FieldTag, [KeyText "maison>produits"])
                     , (FieldSource, [KeyText "a-p"])
                     , (FieldCustom "date", [KeyDay (Time.fromGregorian 2020 2 9)]) ])
      `shouldBe` "/browse?source=a-p&tag=maison%3Eproduits&m.date=2020-02-09&m.done=false"

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
    U.addToReq (U.createReq (FieldTag, KeyText "a")) (FieldTag, KeyText "b")
      `shouldBe` req [(FieldTag, [KeyText "a", KeyText "b"])]
    U.addToReq (U.createReq (FieldTag, KeyText "a")) (FieldTag, KeyText "a>b")
      `shouldBe` req [(FieldTag, [KeyText "a>b"])]
    U.addToReq (U.createReq (FieldTag, KeyText "a\\>b")) (FieldTag, KeyText "a>b")
      `shouldBe` req [(FieldTag, [KeyText "a\\>b", KeyText "a>b"])]

  it "combines sources with an union, the other fields with an intersection" $
    U.reqToQuery (req [ (FieldSource, [KeyText "a", KeyText "b"])
                      , (FieldTag, [KeyText "x", KeyText "y"]) ])
      `shouldBe` DI.And [ DI.Any FieldSource (S.fromList [KeyText "a", KeyText "b"])
                        , DI.All FieldTag (S.fromList [KeyText "x", KeyText "y"]) ]

  it "adds the sources context to URLs, before the fragment" $ do
    let ss = S.fromList ["b c", "a"]
    U.inContext S.empty "/page/s/p#h" `shouldBe` "/page/s/p#h"
    U.inContext ss "/page/s/p#h" `shouldBe` "/page/s/p?source=a&source=b%20c#h"
    U.inContext ss "/browse" `shouldBe` U.browseUrl (U.sourcesReq ss)

  it "removes keys from a request" $ do
    let r = req [(FieldTag, [KeyText "a", KeyText "b"]), (FieldSource, [KeyText "s"])]
    U.removeFromReq r (FieldTag, KeyText "a")
      `shouldBe` req [(FieldTag, [KeyText "b"]), (FieldSource, [KeyText "s"])]
    U.removeFromReq r (FieldSource, KeyText "s")
      `shouldBe` req [(FieldTag, [KeyText "a", KeyText "b"])]


passwordSpec :: Spec
passwordSpec = do
  let salt = "0123456789abcdef"
      hash = fromMaybe (error "hashPassword failed") $ U.hashPassword salt "secret"
      accounts = [Account "paul" salt hash ["admin", "family"]]

  it "hashes passwords deterministically, as hex" $ do
    U.hashPassword salt "secret" `shouldBe` Just hash
    T.length hash `shouldBe` 64
    U.hashPassword salt "other" `shouldNotBe` Just hash
    U.hashPassword "fedcba9876543210" "secret" `shouldNotBe` Just hash

  it "refuses salts shorter than 8 bytes" $
    U.hashPassword "short" "secret" `shouldBe` Nothing

  it "verifies logins" $ do
    U.verifyLogin accounts "paul" "secret" `shouldBe` Just ("paul", ["admin", "family"])
    U.verifyLogin accounts "paul" "wrong" `shouldBe` Nothing
    U.verifyLogin accounts "nobody" "secret" `shouldBe` Nothing

  it "generates random alphanumeric texts" $ do
    t0 <- U.randomText 0
    t0 `shouldBe` ""
    t1 <- U.randomText 128
    t2 <- U.randomText 128
    T.length t1 `shouldBe` 128
    T.all (\c -> isAscii c && isAlphaNum c) t1 `shouldBe` True
    t1 `shouldNotBe` t2


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
