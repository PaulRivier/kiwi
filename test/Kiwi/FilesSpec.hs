module Kiwi.FilesSpec (filesSpec) where

import qualified Data.Text as T
import           Test.Hspec

import           Kiwi.Files

filesSpec :: Spec
filesSpec = do
  it "tells which paths can be served" $ do
    let ok = [ ["a.png"], ["notes.mdx"], ["cmd.txt"], ["x.mdb"], ["d", "e.pdf"]
             , ["a.b", "md"], ["a__XJD54PM77T.png"], ["Makefile"], ["a.partition"]
             , ["x.sync.png"] ]
        refused = [ [], [""], ["."], [".."], ["a", ""], [".git", "config"], ["_x"]
                  , ["a", "_b", "c"], ["p.md"], ["P.Md"], ["p.markdown"], ["p.md~"]
                  , ["#p.md#"], ["p.md.bak"], ["p.md-20250101"], ["rapport.md.pdf"]
                  , ["a~"], ["#a"], ["a#"], ["a/b"], ["a\0"], ["notes.md", "a.png"]
                  , ["~$a.docx"], ["~syncthing~a.png.tmp"], ["a.png.PART"], ["a.crdownload"]
                  , ["a.tmp"], ["a.swp"], ["a.png.bak"], ["a.orig"], ["a.lock"]
                  , ["a.sync-conflict-20260101-120000-ABCDEFG.png"], ["Thumbs.db"]
                  , ["desktop.INI"], ["old.bak", "a.png"], ["a.kiwi-migration"] ]
    filter (not . servablePath) (map (map T.pack) ok) `shouldBe` []
    filter servablePath (map (map T.pack) refused) `shouldBe` []

  it "keeps the names of the files sent that kiwi can serve, without the spaces around" $ do
    map uploadName [" a b.png ", "sch\xe9ma.svg", "x__XJD54PM77T.pdf", "Makefile"]
      `shouldBe` map Just ["a b.png", "sch\xe9ma.svg", "x__XJD54PM77T.pdf", "Makefile"]
    map uploadName [ "", "  ", "p.md", "a/b.png", "a\\b.png", ".x", "_x", "x~", "x.bak"
                   , "a\nb", T.replicate 201 "a" ]
      `shouldBe` replicate 11 Nothing

  it "tells the conflict copies of syncthing" $ do
    isConflictCopy "p.sync-conflict-20260101-120000-ABCDEFG.md" `shouldBe` True
    isConflictCopy "a.SYNC-CONFLICT-20260101-120000-ABCDEFG.png" `shouldBe` True
    isConflictCopy "p.md" `shouldBe` False
    isConflictCopy "sync-conflict.md" `shouldBe` False

  it "reads the uid of a file name" $ do
    fileUid "pomme__XJD54PM77T.jpg" `shouldBe` Just "XJD54PM77T"
    fileUid "pomme__XJD54PM77T (copie).jpg" `shouldBe` Just "XJD54PM77T"
    fileUid "Makefile__XJD54PM77T" `shouldBe` Just "XJD54PM77T"
    fileUid "a___XJD54PM77T.png" `shouldBe` Just "XJD54PM77T"
    fileUid "a__b__XJD54PM77T.tar.gz" `shouldBe` Just "XJD54PM77T"
    -- not a uid: followed by a letter or a digit, too short, lower case,
    -- outside of the Crockford alphabet, a single '_'
    mapM_ (\n -> (n, fileUid n) `shouldBe` (n, Nothing))
      [ "pomme__XJD54PM77Ta.jpg", "pomme__XJD54PM77T0.jpg", "pomme__XJD54PM7.jpg"
      , "pomme__xjd54pm77t.jpg", "pomme__XJD54PM77I.jpg", "pomme_XJD54PM77T.jpg"
      , "__init__.py", "pomme.jpg" ]

  it "writes the uid in a file name" $ do
    withFileUid "K3M9QW2TZ1" "pomme.jpg" `shouldBe` "pomme__K3M9QW2TZ1.jpg"
    withFileUid "K3M9QW2TZ1" "archive.tar.gz" `shouldBe` "archive__K3M9QW2TZ1.tar.gz"
    withFileUid "K3M9QW2TZ1" "Makefile" `shouldBe` "Makefile__K3M9QW2TZ1"
    -- in place of the uid of the name
    withFileUid "K3M9QW2TZ1" "pomme__XJD54PM77T (copie).jpg"
      `shouldBe` "pomme__K3M9QW2TZ1 (copie).jpg"

  it "gives the name of a file without its uid" $ do
    servedName "pomme__XJD54PM77T.jpg" `shouldBe` "pomme.jpg"
    servedName "Makefile__XJD54PM77T" `shouldBe` "Makefile"
    servedName "pomme.jpg" `shouldBe` "pomme.jpg"
