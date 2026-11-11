module Kiwi.ControllerSpec (metaIndexSpec) where

import qualified Data.Map as M
import qualified Data.Set as S
import           Test.Hspec

import qualified Kiwi.Browse as Browse
import           Kiwi.Controller (BrowseAll (..), metaIndex)
import           Kiwi.Types
import qualified Kiwi.View as V
import           TestUtils

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
      [ (FieldSource, [ V.Toggle "s1" "/browse?source=s1&tag=sport" False ]) ]
    baNextStep ba `shouldBe`
      [(FieldTag, [ ("> escalade", "/browse?tag=sport%3Eescalade")
                  , ("> ski", "/browse?tag=sport%3Eski")
                  , ("maison", "/browse?tag=maison&tag=sport") ])]
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

  it "proposes the sources with pages matching the other fields" $ do
    let ba = metaIndex db noContext (Browse.addToReq (Browse.createReq $ source "s2") (tag "maison"))
    baToggles ba `shouldBe`
      [ (FieldSource, [ V.Toggle "s1" "/browse?source=s1&source=s2&tag=maison" False
                      , V.Toggle "s2" "/browse?tag=maison" True ]) ]
    uids ba `shouldBe` [("s2", "c")]

  it "keeps the selected sources without matching pages, to remove them" $ do
    let ba = metaIndex db noContext (Browse.addToReq (Browse.createReq $ source "s2") (tag "sport"))
    baToggles ba `shouldBe`
      [ (FieldSource, [ V.Toggle "s1" "/browse?source=s1&source=s2&tag=sport" False
                      , V.Toggle "s2" "/browse?tag=sport" True ]) ]
    uids ba `shouldBe` []

  it "sorts the sources and the next tags without case nor accents" $ do
    let db' = mkPagesDB [ mkPage ("B", "a") ["zoo"] ["public"] False 1
                        , mkPage ("a", "b") ["\201cologie"] ["public"] False 2
                        , mkPage ("\201c", "c") ["abeille", "Zen"] ["public"] False 3 ]
        ba = metaIndex db' noContext M.empty
    map (map V.toggleLabel . snd) (baToggles ba) `shouldBe` [["a", "B", "\201c"]]
    map (map fst . snd) (baNextStep ba) `shouldBe` [["abeille", "\201cologie", "Zen", "zoo"]]
