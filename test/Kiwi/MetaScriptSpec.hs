module Kiwi.MetaScriptSpec (metaScriptSpec) where

import qualified Data.List as L
import qualified Data.Text as T
import           Test.Hspec

import           Kiwi.Meta (parseHeader, writeHeader)
import           Kiwi.MetaScript
import           Kiwi.Types


cfg :: [CustomMetaConfig]
cfg = [CustomMetaConfig "zone" CmtText 1, CustomMetaConfig "date" CmtDate 1]

metaScriptSpec :: Spec
metaScriptSpec = do
  let header = either error id . parseHeader cfg
      -- the header after a command, as written; Nothing when unchanged
      run line h = case parseScript line of
        Right [c] -> fmap (writeHeader cfg) <$> applyCommand cfg c (header h)
        other -> error (show other)
      page = "title: P\nuid: u\ntags: cuisine > matériel > poêle, cuisine, b\nzone: north, south\n\
             \date: 2020-1-5\nauthor: Ann, Bob\n"

  it "reads the commands, with quoted arguments and comments" $ do
    parseScript "# matériel\n\n  meta tags move \"cuisine > matériel\" \"matériel > cuisine\"\n\
                \page 0123 meta summary set \"About \\\"x\\\" \\\\ y\"\npage 0123 meta lang unset\n"
      `shouldBe` Right
        [ Command 3 AllPages "tags" (Move "cuisine > matériel" "matériel > cuisine")
        , Command 4 (OnePage "0123") "summary" (Set "About \"x\" \\ y")
        , Command 5 (OnePage "0123") "lang" Unset ]

  it "tells where a command is wrong, and what was expected" $ do
    let err s = either id (const "") (parseScript s)
    err "meta tags move a" `shouldSatisfy` L.isInfixOf "line 1:1:17"
    err "meta tags set a" `shouldSatisfy` L.isInfixOf "expecting"
    err "ok\npage x meta tags move a b" `shouldSatisfy` \e ->
      "line 1" `L.isInfixOf` e && "line 2" `L.isInfixOf` e
    err "meta tags delete \"open" `shouldSatisfy` L.isInfixOf "closing quote"

  it "writes the commands back" $
    fmap (map commandText) (parseScript "page u meta summary set \"a b\"\nmeta tags delete x")
      `shouldBe` Right ["page u meta summary set \"a b\"", "meta tags delete x"]

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
    run "meta title move P Q" page `shouldSatisfy` either (L.isInfixOf "page by page") (const False)

  it "sets, adds, removes and unsets the fields of a page" $ do
    run "page u meta summary set \"First aid, burns\"" page `shouldSatisfy`
      either (const False) (maybe False (T.isInfixOf "title: P\nsummary: First aid, burns\nuid: u\n"))
    run "page u meta tags add \"santé, b\"" page `shouldSatisfy`
      either (const False) (maybe False (T.isInfixOf "tags: b, cuisine, cuisine > matériel > poêle, santé\n"))
    run "page u meta tags remove \"cuisine > matériel > poêle\"" page `shouldSatisfy`
      either (const False) (maybe False (T.isInfixOf "tags: b, cuisine\n"))
    run "page u meta zone add north" page `shouldBe` Right Nothing
    run "page u meta author unset" page `shouldSatisfy`
      either (const False) (maybe False (not . T.isInfixOf "author"))
    run "page u meta date set 2020-13-45" page `shouldSatisfy` either (L.isInfixOf "invalid value") (const False)
    run "page u meta lang set klingon" page `shouldSatisfy` either (L.isInfixOf "invalid value") (const False)
    run "page u meta title add x" page `shouldSatisfy` either (L.isInfixOf "one value") (const False)
    run "page u meta uid set x" page `shouldSatisfy` either (L.isInfixOf "can not be changed") (const False)
