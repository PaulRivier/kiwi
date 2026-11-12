module Kiwi.SearchSpec (searchSpec) where

import           Data.List (sort)
import qualified Data.SearchEngine as SE
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Time as Time
import           Test.Hspec

import           Kiwi.Search (excerpt, initPageSearchEngine, searchPages, queryTerms, queryLang,
                              searchLang)
import           Kiwi.Types

searchSpec :: Spec
searchSpec = do
  let page = pageIn French
      -- a page, with the plain text of its content
      pageIn lang uid content =
        Page { pageUID = ("src", uid)
             , pageAbsoluteFSPath = "/dev/null"
             , pageMTime = Time.UTCTime (Time.fromGregorian 2026 1 1) 0
             , pageBody = content
             , pageImagesDir = ""
             , pageFilesDir = ""
             , pagePlainText = content
             , pageTitle = uid
             , pageTags = []
             , pageLang = lang
             , pageAccess = []
             , pageLinks = []
             , pageArchive = False
             , pageCustomMeta = []
             }
      se = SE.insertDocs [ page "a" "Un développement en été"
                         , page "b" "Elle est aimée"
                         , page "c" "Lui et moi"
                         , pageIn Spanish "d" "Los niños corrían"
                         , page "e" "Le chat dort au soleil du jardin"
                         , page "f" "Le chat mange"
                         , page "g" "Le soleil brille" ]
                         initPageSearchEngine
      search q = map snd $ SE.query se (queryTerms French q)
      searchVisible visible = sort . map fst . fst . searchPages se (visible . snd) . queryTerms French

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

  it "finds the visible pages among many pages with the query terms" $ do
    -- the pages that the user can not read do not count in any limit
    let hidden = [ page (T.pack $ "h" ++ show n) "Le soleil" | n <- [1 .. 30 :: Int] ]
                 ++ [ page (T.pack $ "i" ++ show n) "Un chat" | n <- [1 .. 40 :: Int] ]
        se' = SE.insertDocs (page "v1" "Un chat" : page "v2" "Un chat" : hidden)
                            initPageSearchEngine
        visible uid = T.isPrefixOf "v" (snd uid)
    sort (map fst $ fst $ searchPages se' visible $ queryTerms French "chat soleil")
      `shouldBe` [("src", "v1"), ("src", "v2")]

  it "gives the query terms found in the content of each page" $ do
    let se' = SE.insertDocs [ page "t" "Le soleil" `withTitle` "Le chat"
                            , page "u" "Un chat au soleil" `withTitle` "U" ]
                            initPageSearchEngine
        withTitle p t = p { pageTitle = t }
    sort (fst $ searchPages se' (const True) $ queryTerms French "chats soleil")
      `shouldBe` [ (("src", "t"), queryTerms French "soleil")
                 , (("src", "u"), queryTerms French "chat soleil") ]

  it "gives 12 pages at most, telling when there are more" $ do
    let se' = SE.insertDocs [ page (T.pack $ "p" ++ show n) "Un chat" | n <- [1 .. 13 :: Int] ]
                            initPageSearchEngine
        summary visible = let (ps, more) = searchPages se' visible (queryTerms French "chat")
                          in (length ps, more)
    summary (const True) `shouldBe` (12, True)
    summary ((/= "p13") . snd) `shouldBe` (12, False)

  it "gives an excerpt around the first word of the query, inflected or not" $ do
    let long = T.unwords $ replicate 30 "mot" ++ ["Les", "chevaux", "galopent,"]
                           ++ replicate 40 "fin" ++ ["cheval"]
        pg = page "x" long
        ex = excerpt (queryTerms French "cheval") pg
    ex `shouldBe` [ ("…", False), (T.unwords $ replicate 9 "mot" ++ ["Les"], False)
                  , ("chevaux", True)
                  , (T.unwords $ "galopent," : replicate 23 "fin", False), ("…", False) ]
    -- beyond the first words stemmed at once, words of several terms
    let far = T.unwords $ replicate 120 "mot" ++ ["gâteau", "chocolat-banane"]
    excerpt (queryTerms French "banane") (page "z" far)
      `shouldBe` [ ("…", False), (T.unwords $ replicate 9 "mot" ++ ["gâteau"], False)
                 , ("chocolat-banane", True) ]
    -- short content: whole, every matching word flagged
    excerpt (queryTerms French "chats") (page "y" "Le chat, et le Chat.")
      `shouldBe` [("Le", False), ("chat,", True), ("et le", False), ("Chat.", True)]

  it "gives the start of the content when the words of the query are not in it" $ do
    excerpt (queryTerms French "zèbre") (page "y" "Le chat dort")
      `shouldBe` [("Le chat dort", False)]
    excerpt (queryTerms French "zèbre") (page "y" "") `shouldBe` []
    excerpt [] (page "z" $ T.unwords $ replicate 40 "mot")
      `shouldBe` [(T.unwords $ replicate 35 "mot", False), ("…", False)]

  it "splits unknown prefixes, and prefixes after the start, as other words" $ do
    queryTerms French "fr:chevaux" `shouldBe` queryTerms French "chevaux"
    queryTerms French "es:canciones" `shouldBe` queryTerms Spanish "canciones"
    queryTerms French "xx:chevaux" `shouldBe` ["xx", "cheval"]
    queryTerms French "es:" `shouldBe` []
    queryTerms French "chevaux es:x" `shouldBe` queryTerms French "chevaux es x"

  it "gives the lang of the query, from its prefix if any" $ do
    queryLang French "es:canciones" `shouldBe` (Spanish, "canciones")
    queryLang French " EN: horses" `shouldBe` (English, " horses")
    queryLang French "xx:chevaux" `shouldBe` (French, "xx:chevaux")
    queryLang French "chevaux" `shouldBe` (French, "chevaux")

  it "splits the query on punctuation, symbols and all the spaces" $ do
    let same q q' = queryTerms French q `shouldBe` queryTerms French q'
    same "chocolat-banane" "chocolat banane"
    same "\"fondant chocolat\"" "fondant chocolat"
    same "«\x00A0\&fondant chocolat\x00A0»" "fondant chocolat"
    same "“fondant” ‘chocolat’" "fondant chocolat"
    same "(chocolat)" "chocolat"
    same "chocolat," "chocolat"
    same "moelleux/amandes" "moelleux amandes"
    same "l’eau" "l'eau"
    same "l'eau" "l eau"
    same "recette\x00A0:\x202Ftarte" "recette tarte"
    same "1914–1918" "1914 1918"
    same "été—hiver" "été hiver"
    same "etc…" "etc"
    same "a+b=c 5€ #x [y]" "a b c 5 x y"
    queryTerms French " ,;… " `shouldBe` []

  it "finds the words of the pages, split as those of the query" $ do
    let withMeta p title tags = p { pageTitle = title, pageTags = tags }
        se' = SE.insertDocs
                [ page "h" "Un gâteau chocolat-banane"
                , page "i" "Verser l’eau (froide)"
                , page "j" "Pendant la guerre 1914–1918, etc…"
                , withMeta (page "k" "") "Recette\x00A0: tarte" ["dessert > fruits"]
                , page "m" "Rien à voir" ]
                initPageSearchEngine
        search' q = sort $ map snd $ SE.query se' (queryTerms French q)
    search' "chocolat-banane" `shouldBe` ["h"]
    search' "\"banane\"" `shouldBe` ["h"]
    search' "l’eau" `shouldBe` ["i"]
    search' "l'eau" `shouldBe` ["i"]
    search' "eau" `shouldBe` ["i"]
    search' "(froide)" `shouldBe` ["i"]
    search' "1918" `shouldBe` ["j"]
    search' "etc" `shouldBe` ["j"]
    search' "recette" `shouldBe` ["k"]
    search' "fruits" `shouldBe` ["k"]
    -- no "[tag]" term for the tags
    SE.query se' ["[dessert > fruits]"] `shouldBe` []

  describe "searchLang" $ do
    let configs = [ SourceConfig "en" [] Nothing (Just (LangFixed English))
                  , SourceConfig "en-auto" [] Nothing (Just (LangAuto English))
                  , SourceConfig "es" [] Nothing (Just (LangAuto Spanish))
                  , SourceConfig "none" [] Nothing Nothing ]
        allSources = ["en", "en-auto", "es", "none", "unconfigured"]
        lang def ctx = searchLang def configs allSources (S.fromList ctx)
    it "takes the lang of the sources of the context" $ do
      lang (LangFixed French) ["en"] `shouldBe` English
      lang (LangFixed French) ["es"] `shouldBe` Spanish
    it "counts auto/X as X" $
      lang (LangFixed French) ["en", "en-auto"] `shouldBe` English
    it "takes the default lang for the sources without lang" $ do
      lang (LangFixed French) ["none", "unconfigured"] `shouldBe` French
      lang (LangAuto German) ["none"] `shouldBe` German
      lang (LangFixed English) ["en", "none"] `shouldBe` English
    it "takes the default lang when the sources do not agree" $ do
      lang (LangFixed French) ["en", "es"] `shouldBe` French
      lang (LangFixed French) ["en", "none"] `shouldBe` French
    it "takes all the sources when the context is empty" $ do
      lang (LangFixed French) [] `shouldBe` French
      searchLang (LangFixed French) configs ["en", "en-auto"] S.empty `shouldBe` English
      searchLang (LangFixed French) configs [] S.empty `shouldBe` French

