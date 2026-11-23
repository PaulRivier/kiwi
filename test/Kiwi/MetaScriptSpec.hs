module Kiwi.MetaScriptSpec (metaScriptSpec) where

import qualified Data.List as L
import qualified Data.Text as T
import           Test.Hspec

import           Kiwi.Meta (parseHeader, writeHeader)
import           Kiwi.MetaScript
import           Kiwi.Types


cfg :: [CustomMetaConfig]
cfg = [CustomMetaConfig "zone" CmtText 1 Nothing False, CustomMetaConfig "date" CmtDate 1 Nothing False]

metaScriptSpec :: Spec
metaScriptSpec = do
  let header = either error id . parseHeader cfg
      -- the header after a command (on any pages), as written; Nothing when
      -- unchanged
      run line h = case parseScript ("pages *\n" <> line) of
        Right [c] -> fmap (writeHeader cfg) <$> applyCommand cfg c (header h)
        other -> error (show other)
      page = "title: P\nuid: u\ntags: cuisine > matériel > poêle, cuisine, b\nzone: north, south\n\
             \date: 2020-1-5\nauthor: Ann, Bob\n"

  it "reads the commands, each one with its pages line, with quoted arguments and comments" $ do
    let all' = Pages 2 [AllSources]
        some' = Pages 4 [WholeSource "cuisine", OnePage "Mes notes" "0123", OnePage "b" "x_1-2"]
    parseScript "# matériel\npages *\n  meta tags move \"cuisine > matériel\" \"matériel > cuisine\"\n\
                \pages cuisine/*, \"Mes notes\"/0123 ,b/x_1-2\nmeta summary set \"About \\\"x\\\" \\\\ y\"\n\
                \meta lang unset\n"
      `shouldBe` Right
        [ Command 3 all' "tags" (Move "cuisine > matériel" "matériel > cuisine")
        , Command 5 some' "summary" (Set "About \"x\" \\ y")
        , Command 6 some' "lang" Unset ]

  it "tells where a line is wrong, and what was expected" $ do
    let err s = either id (const "") (parseScript s)
    err "pages *\nmeta tags move a" `shouldSatisfy` L.isInfixOf "line 2:1:17"
    err "pages *\nmeta tags set" `shouldSatisfy` L.isInfixOf "expecting"
    err "ok\npages *\nmeta x y" `shouldSatisfy` \e ->
      "line 1" `L.isInfixOf` e && "line 3" `L.isInfixOf` e
    err "pages *\nmeta tags delete \"open" `shouldSatisfy` L.isInfixOf "closing quote"
    -- a command needs a pages line before it
    err "meta tags delete x" `shouldSatisfy` L.isInfixOf "line 1: no \"pages\" line"
    -- an empty item, a source without uid nor *
    err "pages a/x,\nmeta tags delete x" `shouldSatisfy` L.isInfixOf "line 1"
    err "pages a\nmeta tags delete x" `shouldSatisfy` L.isInfixOf "line 1"
    err "pages a/\nmeta tags delete x" `shouldSatisfy` L.isInfixOf "uid, or *"

  it "writes the commands back, each pages line before its commands" $
    fmap scriptText (parseScript "pages a/u\nmeta summary set \"a b\"\nmeta lang unset\n\
                                 \pages \"x y\"/*,*\nmeta tags delete x")
      `shouldBe` Right [ "pages a/u", "meta summary set \"a b\"", "meta lang unset"
                       , "pages \"x y\"/*,*", "meta tags delete x" ]

  it "knows the pages lines with * or SOURCE/*" $ do
    hasWildcard (Pages 1 [OnePage "a" "b"]) `shouldBe` False
    hasWildcard (Pages 1 [OnePage "a" "b", WholeSource "c"]) `shouldBe` True
    hasWildcard (Pages 1 [AllSources]) `shouldBe` True

  it "moves and deletes a tag with its sub-tags, merging the duplicates" $ do
    run "meta tags move \"cuisine > matériel\" \"matériel > cuisine\"" page `shouldBe`
      Right (Just "title: P\nuid: u\ntags: b, cuisine, matériel > cuisine > poêle\n\
                  \zone: north, south\ndate: 2020-01-05\nauthor: Ann, Bob\n")
    run "meta tags move b cuisine" page `shouldSatisfy`
      either (const False) (maybe False (T.isInfixOf "tags: cuisine, cuisine > matériel > poêle\n"))
    run "meta tags delete cuisine" page `shouldSatisfy`
      either (const False) (maybe False (T.isInfixOf "tags: b\n"))
    run "meta tags delete nothing" page `shouldBe` Right Nothing

  it "moves, deletes and renames the values of the fields" $ do
    run "meta zone move north west" page `shouldSatisfy`
      either (const False) (maybe False (T.isInfixOf "zone: west, south\n"))
    -- a date is compared as a date
    run "meta date delete 2020-01-05" page `shouldSatisfy`
      either (const False) (maybe False (not . T.isInfixOf "date:"))
    run "meta author move Bob Robert" page `shouldSatisfy`
      either (const False) (maybe False (T.isInfixOf "author: Ann, Robert\n"))
    run "meta author rename-to zone" page `shouldSatisfy` either (L.isInfixOf "already has") (const False)
    run "meta author rename-to writer" page `shouldSatisfy`
      either (const False) (maybe False (\h -> T.isInfixOf "writer: Ann, Bob\n" h &&
                                                not (T.isInfixOf "author:" h)))
    run "meta title move P Q" page `shouldSatisfy` either (L.isInfixOf "use set or unset") (const False)

  it "sets, adds, removes and unsets the fields of a page" $ do
    run "meta summary set \"First aid, burns\"" page `shouldSatisfy`
      either (const False) (maybe False (T.isInfixOf "title: P\nsummary: First aid, burns\nuid: u\n"))
    run "meta tags add \"santé, b\"" page `shouldSatisfy`
      either (const False) (maybe False (T.isInfixOf "tags: b, cuisine, cuisine > matériel > poêle, santé\n"))
    run "meta tags remove \"cuisine > matériel > poêle\"" page `shouldSatisfy`
      either (const False) (maybe False (T.isInfixOf "tags: b, cuisine\n"))
    -- remove: the exact tag, not its sub-tags
    run "meta tags remove cuisine" page `shouldSatisfy`
      either (const False) (maybe False (T.isInfixOf "tags: b, cuisine > matériel > poêle\n"))
    run "meta zone add north" page `shouldBe` Right Nothing
    run "meta author unset" page `shouldSatisfy`
      either (const False) (maybe False (not . T.isInfixOf "author"))
    run "meta date set 2020-13-45" page `shouldSatisfy` either (L.isInfixOf "invalid value") (const False)
    run "meta lang set klingon" page `shouldSatisfy` either (L.isInfixOf "invalid value") (const False)
    run "meta title add x" page `shouldSatisfy` either (L.isInfixOf "one value") (const False)
    run "meta uid set x" page `shouldSatisfy` either (L.isInfixOf "can not be changed") (const False)
