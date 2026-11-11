module Kiwi.PagesDBSpec (pagesDBSpec) where

import           Data.IORef (modifyIORef, newIORef, readIORef)
import           Data.List (sort)
import qualified Data.SearchEngine as SE
import qualified Data.Text as T
import qualified Data.Time as Time
import qualified System.Directory as D
import qualified System.FilePath as FP
import           System.IO (IOMode (WriteMode), hPutStr, withBinaryFile)
import           Test.Hspec

import           Kiwi.Auth (readableQuery)
import qualified Kiwi.Log as L
import qualified Kiwi.PagesDB as DB
import           Kiwi.Search (queryTerms)
import           Kiwi.Types
import qualified Kiwi.Utils as U
import qualified Utils.DocIndex as DI
import           TestUtils

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

  it "finds the links to missing pages, without warning" $
    withContentDir $ \cd -> do
      writeFileAt (pageFile cd "a.md") 0
        "title: A\n\nkiwiword [b](<page:b.md#x>) [c](<page:c>) [c2](<page:c#y>)"
      writeFileAt (pageFile cd "b.md") 0 "title: B\n\nkiwiword"
      (db, msgs) <- loadDB cd
      let broken = map (\(pg, uid) -> (pageUID pg, uid)) . DB.brokenLinks . pagesIndex
      broken db `shouldBe` [(("s", "a"), ("s", "c"))]
      warnings msgs `shouldBe` []
      writeFileAt (pageFile cd "c.md") 0 "title: C\n\nkiwiword"
      (db', _) <- reloadDB cd db
      broken db' `shouldBe` []

  it "finds the links to pages that the user can not read" $
    withContentDir $ \cd -> do
      writeFileAt (pageFile cd "a.md") 0 "title: A\naccess: public\n\nkiwiword [b](<page:b>)"
      writeFileAt (pageFile cd "b.md") 0 "title: B\naccess: secret\n\nkiwiword"
      (db, _) <- loadDB cd
      let broken loginM = map (\(pg, uid) -> (pageUID pg, uid)) $ DB.brokenLinks $
                          DI.narrow (pagesIndex db) (readableQuery loginM False)
      broken Nothing `shouldBe` [(("s", "a"), ("s", "b"))]
      broken (Just ("someone", ["secret"], Time.UTCTime (Time.fromGregorian 2026 1 1) 0))
        `shouldBe` []

  it "finds the backlinks of a page linked with an anchor" $
    withContentDir $ \cd -> do
      writeFileAt (pageFile cd "a.md") 0 "title: A\n\nkiwiword [b](<page:sub/b#x>)"
      writeFileAt (pageFile cd "sub/b.md") 0 "title: B\n\nkiwiword [a](<page:../a.md>)"
      (db, _) <- loadDB cd
      let backlinks :: PageUID -> [PageUID]
          backlinks uid = map pageUID $ DI.query (pagesIndex db) (U.qOnly FieldLink uid)
      backlinks ("s", "sub:b") `shouldBe` [("s", "a")]
      backlinks ("s", "a") `shouldBe` [("s", "sub:b")]

  it "ignores the files whose name is not the one of a page, in debug" $
    withContentDir $ \cd -> do
      writeFileAt (pageFile cd "_draft.md") 0 "title: A\n\nkiwiword"
      writeFileAt (pageFile cd "notes.txt") 0 "title: A\n\nkiwiword"
      (db, msgs) <- loadDB cd
      dbIds db `shouldReturn` []
      length [ m | (L.Debug, m) <- msgs, T.isPrefixOf "file ignored, " m ] `shouldBe` 2

