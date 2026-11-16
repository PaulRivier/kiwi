module Kiwi.PagesDBSpec (pagesDBSpec) where

import           Control.Exception (IOException, finally, try)
import           Data.Either (isRight)
import qualified Data.ByteString as BS
import           Control.Monad (forM_, when)
import           Data.IORef (modifyIORef, newIORef, readIORef)
import           Data.List (find, sort)
import qualified Data.Map.Strict as M
import qualified Data.SearchEngine as SE
import qualified Data.Text as T
import qualified Data.Time as Time
import qualified System.Directory as D
import qualified System.FilePath as FP
import           System.IO (IOMode (WriteMode), hPutStr, withBinaryFile)
import qualified System.Posix.Files as PF
import           Test.Hspec

import           Kiwi.Auth (readableQuery)
import qualified Kiwi.DefaultPages as DP
import qualified Kiwi.Files as Files
import qualified Kiwi.Log as L
import qualified Kiwi.PagesDB as DB
import           Kiwi.Search (queryTerms)
import           Kiwi.Types
import qualified Kiwi.Utils as U
import qualified Utils.DocIndex as DI
import           TestUtils

-- | Path of a file of the pages of source "s"
pageFile :: FilePath -> FilePath -> FilePath
pageFile cd name = cd FP.</> "s" FP.</> name

-- | Writes the file, with a modification time of the given second
writeFileAt :: FilePath -> Integer -> String -> IO ()
writeFileAt p sec c = do
  D.createDirectoryIfMissing True (FP.takeDirectory p)
  writeFile p c
  D.setModificationTime p (Time.UTCTime (Time.fromGregorian 2026 1 1)
                                        (Time.secondsToDiffTime sec))

-- | The database updated by 'DB.updatePagesDB' (without git), and the
-- messages logged
reloadDB :: FilePath -> PagesDB -> IO (PagesDB, [(L.LogLevel, T.Text)])
reloadDB cd db = do
  logR <- newIORef []
  db' <- DB.updatePagesDB (\lvl msg -> modifyIORef logR ((lvl, msg) :)) False cd db
  msgs <- reverse <$> readIORef logR
  return (db', msgs)

loadDB :: FilePath -> IO (PagesDB, [(L.LogLevel, T.Text)])
loadDB cd = reloadDB cd (DB.emptyPagesDB defMeta [] [])

-- | Uids of the pages of the index, which must be those of the search engine:
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

-- | The page of a file
dbPageOf :: PagesDB -> FilePath -> Maybe Page
dbPageOf db path = find ((== path) . pageAbsoluteFSPath) $ DI.documentsList $ pagesIndex db

-- | Uid of the page of a file (fails without page)
dbUidOf :: PagesDB -> FilePath -> T.Text
dbUidOf db path = maybe (error $ "no page for " ++ path) (snd . pageUID) $ dbPageOf db path

-- | Whether the uid is one drawn by kiwi: 10 characters of Crockford base32
isDrawn :: T.Text -> Bool
isDrawn u = T.length u == 10 && T.all (`elem` ("0123456789ABCDEFGHJKMNPQRSTVWXYZ" :: String)) u

-- | The files of a folder and its subfolders, by path from the folder
traverseFiles :: FilePath -> IO [FilePath]
traverseFiles top = map (FP.makeRelative top) <$> U.traverseDir top (const True)

warnings :: [(L.LogLevel, T.Text)] -> [T.Text]
warnings msgs = [ m | (L.Warn, m) <- msgs ]

pagesDBSpec :: Spec
pagesDBSpec = do
  it "loads the pages, with the uid of their file" $
    withContentDir $ \cd -> do
      writeFileAt (pageFile cd "a.md") 0 "uid: a\ntitle: A\n\nkiwiword"
      writeFileAt (pageFile cd "sub/b.md") 0 "title: B\nuid: Sub_b-2\n\nkiwiword"
      (db, msgs) <- loadDB cd
      dbIds db `shouldReturn` ["Sub_b-2", "a"]
      warnings msgs `shouldBe` []
      -- the files are not written
      readFile (pageFile cd "sub/b.md") `shouldReturn` "title: B\nuid: Sub_b-2\n\nkiwiword"

  it "gives a uid to the pages without one" $
    withContentDir $ \cd -> do
      writeFileAt (pageFile cd "a.md") 0 "tags: x\n---\ntitle: A\n\nkiwiword\n\n"
      writeFileAt (pageFile cd "sub/b.md") 0 "\n\nkiwiword"
      (db, msgs) <- loadDB cd
      warnings msgs `shouldBe` []
      ids <- dbIds db
      length ids `shouldBe` 2
      ids `shouldSatisfy` all isDrawn
      let uidOf = dbUidOf db . pageFile cd
          previous = fmap pagePreviousUids . dbPageOf db . pageFile cd
      -- the header of the file alone is written, normalized, the content kept
      readFile (pageFile cd "a.md") `shouldReturn` T.unpack (T.concat
        [ "title: A\nuid: ", uidOf "a.md", "\ntags: x\n\nkiwiword\n" ])
      readFile (pageFile cd "sub/b.md") `shouldReturn` T.unpack (T.concat
        [ "uid: ", uidOf "sub/b.md", "\n\nkiwiword\n" ])
      previous "a.md" `shouldBe` Just []
      -- the written files are not loaded again
      (db', msgs') <- reloadDB cd db
      msgs' `shouldBe` []
      dbIds db' `shouldReturn` ids

  it "loads the pages of kiwi init with their uid, without writing them" $
    withContentDir $ \cd -> do
      D.createDirectoryIfMissing True (FP.takeDirectory $ pageFile cd "Home.md")
      BS.writeFile (pageFile cd "Home.md") DP.defaultHomePage
      BS.writeFile (pageFile cd "CommonMark.md") DP.commonmarkDocPage
      (db, msgs) <- loadDB cd
      sort (map pageUID $ DI.documentsList $ pagesIndex db)
        `shouldBe` [("s", "CommonMark"), ("s", "Home")]
      warnings msgs `shouldBe` []
      DB.brokenLinks (pagesIndex db) `shouldSatisfy` null
      fmap pageLinks (DI.findDoc (pagesIndex db) ("s", "Home")) `shouldBe` Just [("s", "CommonMark")]
      BS.readFile (pageFile cd "Home.md") `shouldReturn` DP.defaultHomePage
      BS.readFile (pageFile cd "CommonMark.md") `shouldReturn` DP.commonmarkDocPage

  it "does not reload unchanged files" $
    withContentDir $ \cd -> do
      writeFileAt (pageFile cd "a.md") 0 "uid: a\ntitle: A\n\nkiwiword"
      (db, _) <- loadDB cd
      (db', msgs) <- reloadDB cd db
      dbIds db' `shouldReturn` ["a"]
      msgs `shouldBe` []

  it "loads a renamed file, whose time is kept" $
    withContentDir $ \cd -> do
      writeFileAt (pageFile cd "a.md") 0 "uid: a\ntitle: A\n\nkiwiword"
      (db, _) <- loadDB cd
      D.renameFile (pageFile cd "a.md") (pageFile cd "b.md")
      (db', _) <- reloadDB cd db
      dbIds db' `shouldReturn` ["a"]
      dbPagePath db' "a" `shouldBe` Just (pageFile cd "b.md")

  it "loads a new file with an old time" $
    withContentDir $ \cd -> do
      writeFileAt (pageFile cd "a.md") 100 "uid: a\ntitle: A\n\nkiwiword"
      (db, _) <- loadDB cd
      writeFileAt (pageFile cd "b.md") 0 "uid: b\ntitle: B\n\nkiwiword"
      (db', _) <- reloadDB cd db
      dbIds db' `shouldReturn` ["a", "b"]

  it "reloads a file whose time goes back" $
    withContentDir $ \cd -> do
      writeFileAt (pageFile cd "a.md") 100 "uid: a\ntitle: A\n\nkiwiword oldword"
      (db, _) <- loadDB cd
      writeFileAt (pageFile cd "a.md") 50 "uid: a\ntitle: A\n\nkiwiword newword"
      (db', _) <- reloadDB cd db
      dbIds db' `shouldReturn` ["a"]
      dbSearch db' "newword" `shouldBe` [("s", "a")]
      dbSearch db' "oldword" `shouldBe` []

  it "removes the page of a file whose uid changes" $
    withContentDir $ \cd -> do
      writeFileAt (pageFile cd "a.md") 0 "uid: one\n\nkiwiword"
      (db, _) <- loadDB cd
      writeFileAt (pageFile cd "a.md") 1 "uid: two\n\nkiwiword"
      (db', _) <- reloadDB cd db
      dbIds db' `shouldReturn` ["two"]

  it "removes the page of a file that can no longer be loaded" $
    withContentDir $ \cd -> do
      writeFileAt (pageFile cd "a.md") 0 "uid: a\ntitle: A\n\nkiwiword"
      (db, _) <- loadDB cd
      withBinaryFile (pageFile cd "a.md") WriteMode $ \h -> hPutStr h "\xff\xfe"
      D.setModificationTime (pageFile cd "a.md") (Time.UTCTime (Time.fromGregorian 2026 1 2) 0)
      (db', msgs) <- reloadDB cd db
      dbIds db' `shouldReturn` []
      warnings msgs `shouldSatisfy` any (T.isPrefixOf "page ignored, ")

  it "removes the page of a removed file" $
    withContentDir $ \cd -> do
      writeFileAt (pageFile cd "a.md") 0 "uid: a\ntitle: A\n\nkiwiword"
      writeFileAt (pageFile cd "b.md") 0 "uid: b\ntitle: B\n\nkiwiword"
      (db, _) <- loadDB cd
      D.removeFile (pageFile cd "a.md")
      (db', _) <- reloadDB cd db
      dbIds db' `shouldReturn` ["b"]

  it "gives a new uid to the second file of a uid, in path order, and warns" $
    withContentDir $ \cd -> do
      -- "a.markdown" comes before "a.md"
      writeFileAt (pageFile cd "a.md") 0 "uid: a\nprevious-uids: old\ntitle: A\n\nkiwiword"
      writeFileAt (pageFile cd "a.markdown") 0 "uid: a\ntitle: A\n\nkiwiword"
      (db, msgs) <- loadDB cd
      dbPagePath db "a" `shouldBe` Just (pageFile cd "a.markdown")
      let new = dbUidOf db (pageFile cd "a.md")
      new `shouldSatisfy` isDrawn
      dbIds db `shouldReturn` sort ["a", new]
      fmap pagePreviousUids (dbPageOf db $ pageFile cd "a.md") `shouldBe` Just ["old", "a"]
      readFile (pageFile cd "a.md") `shouldReturn` T.unpack (T.concat
        ["title: A\nuid: ", new, "\nprevious-uids: old, a\n\nkiwiword\n"])
      warnings msgs `shouldBe` [ T.concat [ "uid a already used, ", T.pack (pageFile cd "a.md")
                                          , " : new uid ", new ] ]
      (db', msgs') <- reloadDB cd db
      dbIds db' `shouldReturn` sort ["a", new]
      warnings msgs' `shouldBe` []

  it "keeps the uid of a loaded page, a new file of this uid gets another one" $
    withContentDir $ \cd -> do
      writeFileAt (pageFile cd "b.md") 0 "uid: x\n\nkiwiword bword"
      (db, _) <- loadDB cd
      writeFileAt (pageFile cd "a.md") 0 "uid: x\n\nkiwiword aword"
      (db', msgs) <- reloadDB cd db
      dbPagePath db' "x" `shouldBe` Just (pageFile cd "b.md")
      dbSearch db' "bword" `shouldBe` [("s", "x")]
      dbSearch db' "aword" `shouldBe` [("s", dbUidOf db' (pageFile cd "a.md"))]
      length (warnings msgs) `shouldBe` 1

  it "keeps the uid of a modified file" $
    withContentDir $ \cd -> do
      writeFileAt (pageFile cd "a.md") 0 "uid: x\n\nkiwiword"
      (db, _) <- loadDB cd
      writeFileAt (pageFile cd "a.md") 1 "uid: x\ntitle: New\n\nkiwiword"
      (db', msgs) <- reloadDB cd db
      dbIds db' `shouldReturn` ["x"]
      warnings msgs `shouldBe` []

  it "gives the same uid in two sources" $
    withContentDir $ \cd -> do
      writeFileAt (pageFile cd "a.md") 0 "uid: x\n\nkiwiword"
      writeFileAt (cd FP.</> "t" FP.</> "a.md") 0 "uid: x\n\nkiwiword"
      (db, msgs) <- loadDB cd
      sort (map pageUID $ DI.documentsList $ pagesIndex db) `shouldBe` [("s", "x"), ("t", "x")]
      warnings msgs `shouldBe` []

  it "replaces an invalid uid" $
    withContentDir $ \cd -> do
      writeFileAt (pageFile cd "a.md") 0 "uid: x/y\n\nkiwiword"
      (db, msgs) <- loadDB cd
      let new = dbUidOf db (pageFile cd "a.md")
      new `shouldSatisfy` isDrawn
      fmap pagePreviousUids (dbPageOf db $ pageFile cd "a.md") `shouldBe` Just ["x/y"]
      warnings msgs `shouldBe` [ T.concat [ "uid x/y invalid, ", T.pack (pageFile cd "a.md")
                                          , " : new uid ", new ] ]

  it "does not load a page whose uid can not be written, until it can" $
    withContentDir $ \cd -> do
      writeFileAt (pageFile cd "ro/a.md") 0 "title: A\n\nkiwiword"
      writeFileAt (pageFile cd "b.md") 0 "uid: b\n\nkiwiword"
      let roDir = pageFile cd "ro"
      PF.setFileMode roDir 0o555
      -- root can write anyway
      probe <- try (writeFile (roDir FP.</> ".probe") "") :: IO (Either IOException ())
      when (isRight probe) $ do
        PF.setFileMode roDir 0o755
        pendingWith "the folder can be written (root)"
      (db, msgs) <- loadDB cd `finally` PF.setFileMode roDir 0o755
      dbIds db `shouldReturn` ["b"]
      map (\(src, p, _) -> (src, p)) (loadErrors db) `shouldBe` [("s", "ro/a.md")]
      length [ m | (L.Error, m) <- msgs ] `shouldBe` 1
      (db', _) <- reloadDB cd db
      length <$> dbIds db' `shouldReturn` 2
      loadErrors db' `shouldBe` []

  it "finds the links to missing pages, without warning" $
    withContentDir $ \cd -> do
      writeFileAt (pageFile cd "a.md") 0
        "uid: a\n\nkiwiword [b](<page:b#x>) [c](<page: c>) [c2](page:c#y)"
      writeFileAt (pageFile cd "b.md") 0 "uid: b\n\nkiwiword"
      (db, msgs) <- loadDB cd
      let broken = map (\(pg, uid) -> (pageUID pg, uid)) . DB.brokenLinks . pagesIndex
      broken db `shouldBe` [(("s", "a"), ("s", "c"))]
      warnings msgs `shouldBe` []
      writeFileAt (pageFile cd "c.md") 0 "uid: c\n\nkiwiword"
      (db', _) <- reloadDB cd db
      broken db' `shouldBe` []

  it "finds the links to pages that the user can not read" $
    withContentDir $ \cd -> do
      writeFileAt (pageFile cd "a.md") 0 "uid: a\naccess: public\n\nkiwiword [b](page:b)"
      writeFileAt (pageFile cd "b.md") 0 "uid: b\naccess: secret\n\nkiwiword"
      (db, _) <- loadDB cd
      let broken loginM = map (\(pg, uid) -> (pageUID pg, uid)) $ DB.brokenLinks $
                          DI.narrow (pagesIndex db) (readableQuery loginM False)
      broken Nothing `shouldBe` [(("s", "a"), ("s", "b"))]
      broken (Just $ Login "someone" ["secret"] $ Time.UTCTime (Time.fromGregorian 2026 1 1) 0)
        `shouldBe` []

  it "finds the backlinks of a page linked with an anchor" $
    withContentDir $ \cd -> do
      writeFileAt (pageFile cd "a.md") 0 "uid: a\n\nkiwiword [b](<page:b#x>)"
      writeFileAt (pageFile cd "sub/b.md") 0 "uid: b\n\nkiwiword [a](page:a)"
      (db, _) <- loadDB cd
      let backlinks :: PageUID -> [PageUID]
          backlinks uid = map pageUID $ DI.query (pagesIndex db) (U.qOnly FieldLink uid)
      backlinks ("s", "b") `shouldBe` [("s", "a")]
      backlinks ("s", "a") `shouldBe` [("s", "b")]

  it "ignores the files whose name is not the one of a page, and the hidden and _ ones" $
    withContentDir $ \cd -> do
      D.createDirectoryIfMissing True (cd FP.</> "s" FP.</> "_drafts")
      D.createDirectoryIfMissing True (cd FP.</> "s" FP.</> ".git")
      D.createDirectoryIfMissing True (cd FP.</> "s" FP.</> ".stversions")
      forM_ ["_draft.md", "notes.txt", "-draft.md", "_drafts/a.md", ".git/b.md"
            , ".stversions/c.md", "page.md~"] $ \name ->
        writeFileAt (pageFile cd name) 0 "title: A\n\nkiwiword"
      (db, msgs) <- loadDB cd
      dbIds db `shouldReturn` []
      -- only a page file whose name is refused is logged (in debug)
      [ m | (L.Debug, m) <- msgs, T.isPrefixOf "file ignored, " m ]
        `shouldBe` [ T.pack $ "file ignored, " ++ pageFile cd "-draft.md" ++
                     " : a page file name starts with a letter or a digit, and ends with \
                     \.md or .markdown"
                   , T.pack $ "file ignored, " ++ pageFile cd "page.md~" ++
                     " : temporary or backup file (see the README)" ]
      -- the other files get a uid
      M.keys (sourceFiles db) `shouldSatisfy` ((== 1) . length)

  it "gives a uid to the files, renamed, the links written with a path unchanged" $
    withContentDir $ \cd -> do
      writeFileAt (pageFile cd "p.md") 0
        "uid: p\n\n![a](file:img/a.png) [b](<file:/b c.pdf#page=2>) \
        \[k](file:k__XJD54PM77T.txt) [g](file:gone.png) kiwiword"
      forM_ [ "img/a.png", "b c.pdf", "k__XJD54PM77T.txt", "z/k__XJD54PM77T (copie).txt"
            , "x.tmp", "_y.png", "p.md~", ".hidden.png" ] $ \name ->
        writeFileAt (pageFile cd name) 0 name
      (db, msgs) <- loadDB cd
      files <- sort <$> traverseFiles (cd FP.</> "s")
      let named prefix = [ f | f <- files, prefix `T.isPrefixOf` T.pack f ]
          uidOf f = maybe (error f) T.pack $ Files.fileUid (FP.takeFileName f)
      [a] <- return $ named "img/a__"
      [b] <- return $ named "b c__"
      [c] <- return $ named "z/k__"
      uidOf c `shouldNotBe` "XJD54PM77T"
      FP.takeFileName c `shouldBe` ("k__" ++ T.unpack (uidOf c) ++ " (copie).txt")
      -- the excluded files are not renamed
      files `shouldBe` sort [a, b, c, "k__XJD54PM77T.txt", "p.md", "x.tmp", "_y.png", "p.md~"
                            , ".hidden.png"]
      sourceFiles db `shouldBe` M.fromList [ (("s", uidOf f), f)
                                           | f <- [a, b, c, "k__XJD54PM77T.txt"] ]
      -- the links are kept as written (broken, listed on /doctor)
      readFile (pageFile cd "p.md") `shouldReturn`
        "uid: p\n\n![a](file:img/a.png) [b](<file:/b c.pdf#page=2>) \
        \[k](file:k__XJD54PM77T.txt) [g](file:gone.png) kiwiword"
      fmap pageFiles (dbPageOf db (pageFile cd "p.md"))
        `shouldBe` Just ["/b c.pdf", "gone.png", "img/a.png", "k__XJD54PM77T.txt"]
      warnings msgs `shouldBe` []
      -- nothing to do at the next load
      (db', msgs') <- reloadDB cd db
      sourceFiles db' `shouldBe` sourceFiles db
      warnings msgs' `shouldBe` []
      sort <$> traverseFiles (cd FP.</> "s") `shouldReturn` files

  it "keeps the uid of a known file, a new copy of it gets another one" $
    withContentDir $ \cd -> do
      writeFileAt (pageFile cd "k__XJD54PM77T.txt") 0 "k"
      (db, _) <- loadDB cd
      writeFileAt (pageFile cd "a/k__XJD54PM77T.txt") 0 "copy"
      (db', _) <- reloadDB cd db
      M.lookup ("s", "XJD54PM77T") (sourceFiles db') `shouldBe` Just "k__XJD54PM77T.txt"
      M.size (sourceFiles db') `shouldBe` 2
      D.doesFileExist (pageFile cd "a/k__XJD54PM77T.txt") `shouldReturn` False

  it "commits the files renamed to get their uid" $ withGit $
    withContentDir $ \cd -> do
      git cd ["init", "-q"]
      writeFileAt (pageFile cd "p.md") 0 "uid: p\n\n![a](file:a.png) kiwiword"
      writeFileAt (pageFile cd "a.png") 0 "PNG"
      git cd ["add", "."]
      git cd ["commit", "-q", "-m", "init"]
      writeFileAt (pageFile cd "new.png") 0 "NEW"
      db <- DB.updatePagesDB (\_ _ -> return ()) True cd (DB.emptyPagesDB defMeta [] [])
      gitOut cd ["log", "--format=%an|%s"] `shouldReturn`
        "Kiwi|Kiwi: uid assigned to 2 files\nKiwi|Kiwi: pre-update commit of 2 files\n\
        \test|init\n"
      [a] <- return [ p | p <- M.elems (sourceFiles db), "a__" `T.isPrefixOf` T.pack p ]
      gitOut cd ["show", "-M", "--name-status", "--format=", "HEAD", "--", "s/a.png"
                , "s/" ++ a ]
        `shouldReturn` ("R100\ts/a.png\ts/" ++ a ++ "\n")
      gitOut cd ["status", "--porcelain"] `shouldReturn` ""

  it "lists the files that are not loaded, until they are fixed" $
    withContentDir $ \cd -> do
      writeFileAt (pageFile cd "a.md") 0 "uid: a\ntitle: A\n\nkiwiword"
      writeFileAt (pageFile cd "sub/bad.md") 0 "# No header\n\nkiwiword"
      (db, msgs) <- loadDB cd
      dbIds db `shouldReturn` ["a"]
      loadErrors db `shouldBe`
        [ ("s", "sub/bad.md", "unable to load the page : invalid metadata header, line 1") ]
      length (warnings msgs) `shouldBe` 1
      -- the same errors at each reload, without any change
      (db', _) <- reloadDB cd db
      loadErrors db' `shouldBe` loadErrors db
      writeFileAt (pageFile cd "sub/bad.md") 1 "uid: bad\ntitle: Fixed\n\nkiwiword"
      (db'', _) <- reloadDB cd db'
      dbIds db'' `shouldReturn` ["a", "bad"]
      loadErrors db'' `shouldBe` []

  it "commits the files whose uid is written, after their changes" $ withGit $
    withContentDir $ \cd -> do
      git cd ["init", "-q"]
      writeFileAt (pageFile cd "a.md") 0 "title: A\n\nkiwiword"
      writeFileAt (pageFile cd "b.md") 0 "uid: b\n\nkiwiword"
      git cd ["add", "."]
      git cd ["commit", "-q", "-m", "init"]
      writeFileAt (pageFile cd "a.md") 0 "title: A2\n\nkiwiword"
      writeFileAt (pageFile cd "c.md") 0 "title: C\n\nkiwiword"
      _ <- DB.updatePagesDB (\_ _ -> return ()) True cd (DB.emptyPagesDB defMeta [] [])
      gitOut cd ["log", "--format=%an|%s"] `shouldReturn`
        "Kiwi|Kiwi: uid assigned to 2 pages\nKiwi|Kiwi: pre-update commit of 2 pages\ntest|init\n"
      gitOut cd ["show", "--name-only", "--format=", "HEAD~1"]
        `shouldReturn` "s/a.md\ns/c.md\n"
      gitOut cd ["status", "--porcelain"] `shouldReturn` ""
