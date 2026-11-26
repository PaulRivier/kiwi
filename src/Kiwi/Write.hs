{-# LANGUAGE LambdaCase #-}

-- | Writing of the page files changed by a user: the browser editor, and
-- the API. A change (see 'Kiwi.Edit.Change') is checked and written under the
-- lock of the reloads, with git commits, then the pages are reloaded.
module Kiwi.Write
  ( readPageFile
  , updatePage
  , createPage
  , creatableSources
  , canCreateIn
  , deletePage
  , orphanFiles
  , ScriptRun (..)
  , runMetaScript
  , metaScriptResult
  ) where

import           Control.Concurrent.MVar (withMVar)
import           Control.Exception (IOException, try)
import           Control.Monad (foldM, forM, forM_, unless, when)
import           Control.Monad.Except (ExceptT (..), liftEither, runExceptT, throwError, withExceptT)
import qualified Crypto.Random as CR
import           Data.Bifunctor (first)
import qualified Data.List as L
import           Data.Maybe (fromMaybe, isJust)
import           Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString as BS
import           Data.IORef (readIORef)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified System.Directory as D
import qualified System.FilePath as FP

import qualified Kiwi.Auth as Auth
import qualified Kiwi.Edit as Edit
import qualified Kiwi.Git as Git
import qualified Kiwi.Log as L
import qualified Kiwi.PagesDB as DB
import           Kiwi.Meta (clearMeta, sourceMeta, splitPage, uidFromBytes, writeHeader, writePage)
import qualified Kiwi.MetaScript as MS
import           Kiwi.Pandoc (checkPageText, loadPage)
import qualified Kiwi.Tags as Tags
import qualified Kiwi.Utils as U
import           Kiwi.Types
import qualified Utils.DocIndex.Restricted as RDI


-- | Text and bytes of a page file
readPageFile :: FilePath -> IO (Either String (BS.ByteString, T.Text))
readPageFile path = do
  r <- try $ BS.readFile path
  return $ case r of
    Left e -> Left $ "unable to read " ++ path ++ ": " ++ show (e :: IOException)
    Right bytes -> either (const $ Left $ path ++ " is not a valid UTF-8 file")
                          (\t -> Right (bytes, t)) (TE.decodeUtf8' bytes)

-- | Writes the file of a page changed by a user (login), when the change
-- applies to the current file and the page of the new text loads (see
-- 'Edit.checkSave'), in a source where kiwi writes (see 'SourceMode'): its
-- changes made outside of kiwi, if any, are first committed by Kiwi (a
-- failure stops the save, see 'preCommit'), and the new file is committed by
-- the user (a failure is only logged). The pages are then reloaded. The
-- check, the commits, the write and the reload hold the lock of the reloads.
-- The message, if any, is added to the one of the commit (see
-- 'commitMessage'). The result is the hash of the written file, and the keys
-- of the change that are ignored.
updatePage :: ServerState -> T.Text -> Maybe T.Text -> Page -> Edit.Change ->
              IO (Either Edit.SaveFailure (T.Text, [T.Text]))
updatePage ss user messageM p change = withMVar (pagesDBLock ss) $ \_ -> runExceptT $ do
  let path = pageAbsoluteFSPath p
      uid = pageUID p
  db <- liftIO $ readIORef (pagesDB ss)
  writableSource db (fst uid)
  file <- withExceptT Edit.SaveFailed $ ExceptT $ readPageFile path
  (text, ignored) <- liftEither $ Edit.checkSave (contentDir ss) db change path file
  preCommit ss (fst uid) (pageMessage ss "Pre-update commit of " uid Nothing) [path]
  withExceptT (\e -> Edit.SaveFailed $ show (e :: IOException)) $ ExceptT $ try $
    Edit.writePageFile path text
  liftIO $ do
    logger ss L.Info $ T.pack $ concat [ "page saved by ", T.unpack user, ": ", path ]
    commitPaths ss (fst uid) (user <> " via Kiwi") (pageMessage ss "Edit " uid messageM) [path]
    DB.reloadPages ss
  return (Edit.fileHash $ TE.encodeUtf8 text, ignored)

-- | Creates a page in a source (its folder must exist) for a user, from its
-- text as written by an editor (header and content; the keys that the user
-- can not set are ignored, see 'Edit.protectedKeys'): Kiwi gives it a new
-- uid, and writes it at the given path (from the top of the source, without
-- extension, see 'Edit.validPagePath'; its folders are made), or else in a
-- file named after its title (see 'Edit.titleFileName') at the top of the
-- source. When the name is taken, " (2)", " (3)"... is added to it. The user
-- must have the write right on the page, by its access (the one of the
-- source, or of the header for an admin), and kiwi must write in the source
-- (see 'SourceMode'). The page is committed (a failure is only logged) and
-- the pages are reloaded, under the lock of the reloads. The message, if
-- any, is added to the one of the commit. The result is the uid of the page
-- and the path of its file, from the top of the source.
createPage :: ServerState -> Maybe Login -> Maybe T.Text -> T.Text -> Maybe FilePath -> T.Text ->
              IO (Either Edit.SaveFailure (PageUID, FilePath))
createPage ss loginM messageM source pathM sent = withMVar (pagesDBLock ss) $ \_ -> runExceptT $ do
  db <- liftIO $ readIORef (pagesDB ss)
  let cmc = customMetaConfig db
      top = contentDir ss FP.</> T.unpack source
      admin = maybe False (elem "admin" . loginGroups) loginM
      user = maybe "" loginUser loginM
      scM = L.find ((== source) . scSourceName) (sourcesConfig db)
  exists <- liftIO $ D.doesDirectoryExist top
  unless exists $ throwError $ Edit.SaveInvalid $ "no source " ++ T.unpack source
  writableSource db source
  (sentHeader, content) <- liftEither $ first Edit.SaveInvalid $ splitPage cmc sent
  uid <- liftIO $ newUid db
  let level = if admin then AccessAdmin else AccessWrite
      header = (clearMeta (Edit.protectedKeys level) sentHeader) { metaUid = Just uid }
      text = writePage cmc header content
      title = fromMaybe "" (metaTitle header)
  base <- case pathM of
    Just p | not admin -> throwError $ Edit.SaveForbidden
               "only the admins choose the path of a new page"
           | not (Edit.validPagePath p) -> throwError $ Edit.SaveInvalid $ "invalid path " ++ p
           | otherwise -> return (top FP.</> p)
    Nothing -> return (top FP.</> Edit.titleFileName title)
  path <- liftIO $ freePath base
  p <- liftEither $ first Edit.SaveInvalid $
         loadPage path text source (sourceMeta scM (defaultMeta db)) cmc
  unless (maybe False (>= AccessWrite) $ Auth.accessLevel loginM $
            fromMaybe [] $ metaAccess $ pandocMeta p) $
    throwError $ Edit.SaveForbidden $ "the account can not write a page of this access in "
                                      ++ T.unpack source
  withExceptT (\e -> Edit.SaveFailed $ show (e :: IOException)) $ ExceptT $ try $ do
    D.createDirectoryIfMissing True (FP.takeDirectory path)
    Edit.writeNewPageFile path text
  liftIO $ do
    logger ss L.Info $ T.pack $ concat [ "page created by ", T.unpack user, ": ", path ]
    commitPaths ss source (user <> " via Kiwi") (pageMessage ss "Create " (source, uid) messageM) [path]
    DB.reloadPages ss
  return ((source, uid), FP.makeRelative top path)
  where
    newUid db = do
      u <- uidFromBytes <$> (CR.getRandomBytes 10 :: IO BS.ByteString)
      if isJust (RDI.findDoc (pagesIndex db) (source, u)) then newUid db else return u
    -- the first name not taken: NAME.md, NAME (2).md...
    freePath base = go (1 :: Int)
      where go n = do
              let p = (if n == 1 then base else base ++ " (" ++ show n ++ ")") FP.<.> "md"
              taken <- (||) <$> D.doesPathExist p <*> D.doesPathExist (p FP.-<.> "markdown")
              if taken then go (n + 1) else return p

-- | The sources where the user can create a page (see 'canCreateIn')
creatableSources :: PagesDB -> Maybe Login -> [T.Text]
creatableSources db loginM = filter (canCreateIn db loginM) (M.keys $ dbSources db)

-- | Whether the user can create a page in this source: when kiwi writes in
-- the source (see 'SourceMode'), and a page without header would get there
-- an access (default of the config and of the source) that gives the user
-- the write right; every source where kiwi writes for the admins (see
-- 'createPage', which checks the access of the page created)
canCreateIn :: PagesDB -> Maybe Login -> T.Text -> Bool
canCreateIn db loginM source =
  U.isWritableSource db source &&
  maybe False (>= AccessWrite) (Auth.accessLevel loginM $ fromMaybe [] $ metaAccess $
                                  sourceMeta scM (defaultMeta db))
  where scM = L.find ((== source) . scSourceName) (sourcesConfig db)

-- | Deletes the file of a page for a user with the admin right on the page,
-- when it has not changed since it was read (this hash), with the files of
-- these uids, which only this page must link (see 'orphanFiles'), in a
-- source where kiwi writes (see 'SourceMode'): their changes made outside of
-- kiwi, if any, and those not tracked yet are first committed by Kiwi (a
-- failure stops the deletion, see 'preCommit'), so that they stay in its
-- history; the deletion is then committed by the user
-- (a failure is only logged), with the message, if any (see
-- 'commitMessage'), and the files deleted with the page. The pages are then
-- reloaded. All of it holds the lock of the reloads. A page or a file that
-- changed meanwhile (other hash, page gone, file linked by another page) is
-- a conflict.
deletePage :: ServerState -> Maybe Login -> Maybe T.Text -> PageUID -> T.Text -> [T.Text] ->
              IO (Either Edit.SaveFailure ())
deletePage ss loginM messageM uid@(source, pId) hash fileUids =
  withMVar (pagesDBLock ss) $ \_ -> runExceptT $ do
    db <- liftIO $ readIORef (pagesDB ss)
    writableSource db source
    p <- maybe (throwError Edit.SaveConflict) return $ RDI.findDoc (pagesIndex db) uid
    unless (maybe False (>= AccessAdmin) $ Auth.pageAccessLevel db loginM p) $
      throwError $ Edit.SaveForbidden "the account can not delete this page"
    let path = pageAbsoluteFSPath p
        top = contentDir ss FP.</> T.unpack source
        orphans = M.fromList (orphanFiles db p)
    (bytes, _) <- withExceptT Edit.SaveFailed $ ExceptT $ readPageFile path
    when (Edit.fileHash bytes /= hash) $ throwError Edit.SaveConflict
    files <- forM (U.nodup fileUids) $ \u ->
      maybe (throwError Edit.SaveConflict) (return . (top FP.</>)) (M.lookup u orphans)
    let paths = path : files
        ref = T.concat [source, ":", pId]
        user = maybe "" loginUser loginM
        rel = FP.makeRelative top
    preCommit ss source (pageMessage ss "Pre-delete commit of " uid Nothing) paths
    -- the files first: a failure keeps the page, which links them
    (removed, failure) <- liftIO $ removeAll (files ++ [path])
    liftIO $ do
      forM_ removed $ \f -> logger ss L.Info $ T.pack $ concat [ "deleted by ", T.unpack user, ": ", f ]
      let message = commitMessage (T.concat ["Delete ", ref, " ", pageTitle p, " (kiwi ", kiwiName ss, ")"])
                                  messageM <>
                    T.concat [ "\n\nFiles deleted:\n" <> T.unlines [ "- " <> T.pack (rel f) | f <- deletedFiles ]
                             | let deletedFiles = filter (/= path) removed, not (null deletedFiles) ]
      unless (null removed) $ commitPaths ss source (user <> " via Kiwi") message removed
      DB.reloadPages ss
    maybe (return ()) (throwError . Edit.SaveFailed) failure
  where
    -- the files removed, until a failure
    removeAll [] = return ([], Nothing)
    removeAll (f : fs) = try (D.removeFile f) >>= \case
      Left e -> return ([], Just $ "unable to delete " ++ f ++ ": " ++ show (e :: IOException))
      Right () -> (\(done, failure) -> (f : done, failure)) <$> removeAll fs

-- | The files that this page links, and that no other page links (whatever
-- their access, archives included): uid, and path from the top of the source
orphanFiles :: PagesDB -> Page -> [(T.Text, FilePath)]
orphanFiles db p =
  [ (u, path) | u <- U.nodup (pageFiles p)
              , Just path <- [M.lookup (source, u) (sourceFiles db)]
              , all ((== pageUID p) . pageUID) $
                  RDI.query (pagesIndex db) (U.qOnly FieldFile (source, u)) ]
  where source = fst (pageUID p)

-- | Applies the commands of a metadata script (see "Kiwi.MetaScript") to the
-- headers of the pages of their "pages" lines that the user can write,
-- archives included, in their order, all of them or none. A source of a
-- "pages" line that the user can not read, a SOURCE/* where the user can
-- write no page, a SOURCE/UID that the user can not write, a command that can
-- not apply to a page, a command on "*" or SOURCE/* pages that changes none
-- (a typo, most likely), or a change of "access" without the admin right on
-- the page stops everything. Unless only checked, the changed files are
-- written, committed in one commit whose message holds the script (after a
-- commit of their former changes, see 'updatePage'), and the pages are
-- reloaded, under the lock of the reloads. When only checked, the script
-- runs on the pages that the user can read, and the result tells those that
-- the user could not change (to prepare a script that an admin applies).
runMetaScript :: ServerState -> Maybe Login -> Maybe T.Text -> Bool -> [MS.Command] ->
                 IO (Either Edit.SaveFailure ScriptRun)
runMetaScript ss loginM messageM check cmds = withMVar (pagesDBLock ss) $ \_ -> runExceptT $ do
  db <- liftIO $ readIORef (pagesDB ss)
  let cmc = customMetaConfig db
      readable = RDI.query (pagesIndex db) (Auth.readableAnyArchiveQuery loginM)
      byUid = M.fromList [ (pageUID p, p) | p <- readable ]
      canWrite p = maybe False (>= AccessWrite) (Auth.pageAccessLevel db loginM p)
      writable = filter canWrite readable
      -- the pages of the "pages" lines: those that the account can change,
      -- or, when checked, those that it can read
      pool = if check then readable else writable
      sources = L.sort $ L.nub $ map (fst . pageUID) readable
      user = maybe "" loginUser loginM
      ref (s, u) = T.unpack s ++ "/" ++ T.unpack u
      lineAt n = "line " ++ show n ++ ": "
      at c = lineAt (MS.commandLine c) ++ T.unpack (MS.commandText c) ++ ": "
      -- the pages of a "pages" line
      resolve ps = fmap (M.fromList . concat) $ forM (MS.pagesTargets ps) $ \t -> case t of
        MS.AllSources
          | null pool -> Left $ Edit.SaveForbidden $ lineAt n ++ "the account can change no page"
          | otherwise -> Right [ (pageUID p, p) | p <- pool ]
        MS.WholeSource s
          | s `notElem` sources -> Left $ Edit.SaveInvalid $ concat
              [ lineAt n, "no source ", T.unpack s, "; sources: "
              , T.unpack (T.intercalate ", " sources) ]
          | otherwise -> case [ (pageUID p, p) | p <- pool, fst (pageUID p) == s ] of
              [] -> Left $ Edit.SaveForbidden $ lineAt n ++ "the account can change no page of " ++ T.unpack s
              found -> Right found
        MS.OnePage s u -> case M.lookup (s, u) byUid of
          Nothing -> Left $ Edit.SaveInvalid $ lineAt n ++ "no page " ++ ref (s, u)
          Just p | check || canWrite p -> Right [((s, u), p)]
                 | otherwise -> Left $ Edit.SaveForbidden $
                     lineAt n ++ "the account can not change " ++ ref (s, u)
        where n = MS.pagesLine ps
  selections <- liftEither $ fmap M.fromList $
    forM (L.nubBy (\a b -> MS.pagesLine a == MS.pagesLine b) $ map MS.commandPages cmds) $ \ps ->
      (,) (MS.pagesLine ps) <$> resolve ps
  let selected = M.unions (M.elems selections)
  files <- fmap M.fromList $ forM (M.elems selected) $ \p -> do
    (_, text) <- withExceptT Edit.SaveFailed $ ExceptT $ readPageFile (pageAbsoluteFSPath p)
    (header, content) <- liftEither $ first (\e -> Edit.SaveFailed $ concat
                           [pageAbsoluteFSPath p, ": ", e]) $ splitPage cmc text
    return (pageUID p, (p, header, content))
  let -- the tags of these sources close to a tag that no page has
      closeTags ss' t = take 5 $ L.nub
        [ Tags.prettyTag x | s <- ss'
        , KeyText x <- RDI.getFieldKeys (RDI.narrow (pagesIndex db) (U.qOnly FieldSource s)) FieldTag
        , let l = T.toLower (last' $ Tags.tagToSegments t)
        , not (T.null l), l `T.isInfixOf` T.toLower x ]
      last' xs = if null xs then "" else last xs
      step (headers, done, admins) c = do
        let ps = MS.commandPages c
            sel = M.findWithDefault M.empty (MS.pagesLine ps) selections
        results <- forM (M.keys sel) $ \k -> case M.lookup k headers of
          Nothing -> Right (k, Nothing)
          Just h -> (,) k <$> first (\e -> at c ++ ref k ++ ": " ++ e) (MS.applyCommand cmc c h)
        let changed = [ (k, h') | (k, Just h') <- results ]
            hint ts = if null ts then "" else "; tags close to it: " ++ T.unpack (T.intercalate ", " ts)
            selSources = L.nub $ map fst (M.keys sel)
        when (MS.hasWildcard ps && null changed) $ Left $ at c ++ "it changes no page" ++
          case (MS.commandField c, MS.commandOp c) of
            ("tags", MS.Move a _) -> hint (closeTags selSources a)
            ("tags", MS.Delete a) -> hint (closeTags selSources a)
            _ -> ""
        return ( M.union (M.fromList changed) headers
               , (c, [ p | (k, _) <- changed, Just p <- [M.lookup k sel] ]) : done
               , [ k | MS.commandField c == "access", (k, _) <- changed ] ++ admins )
  (final, done, admins) <- liftEither $ first Edit.SaveInvalid $
    foldM step (M.map (\(_, h, _) -> h) files, [], []) cmds
  let changed = [ (p, writePage cmc h content)
                | (k, (p, original, content)) <- M.toList files
                , Just h <- [M.lookup k final]
                , writeHeader cmc h /= writeHeader cmc original ]
      refused = [ ref (pageUID p) | (p, _) <- changed, pageUID p `elem` admins
                , not (maybe False (>= AccessAdmin) $ Auth.pageAccessLevel db loginM p) ]
  unless (check || null refused) $ throwError $ Edit.SaveForbidden $
    "the account can not change the access of these pages: " ++ L.intercalate ", " refused
  forM_ changed $ \(p, text) ->
    liftEither $ first (\e -> Edit.SaveInvalid $ ref (pageUID p) ++ ": " ++ e) $
      checkPageText (contentDir ss) db (pageAbsoluteFSPath p) text
  unless (check || null changed) $ do
    let changedSources = T.intercalate ", " $ L.sort $ L.nub $ map (fst . pageUID . fst) changed
        count = T.pack (show $ length changed) <> " pages"
        message = commitMessage (T.concat [ "Metadata of ", changedSources, ": ", count
                                          , " (kiwi ", kiwiName ss, ")" ]) messageM
                  <> "\n\n" <> T.unlines (MS.scriptText cmds)
        bySource = M.toList $ M.fromListWith (flip (++))
                     [ (fst (pageUID p), [pageAbsoluteFSPath p]) | (p, _) <- changed ]
    forM_ bySource $ writableSource db . fst
    forM_ bySource $ \(source, ps) ->
      preCommit ss source (T.concat [ "Pre-update commit of ", T.pack (show $ length ps)
                                    , " pages (kiwi ", kiwiName ss, ")" ]) ps
    forM_ changed $ \(p, text) ->
      withExceptT (\e -> Edit.SaveFailed $ show (e :: IOException)) $ ExceptT $ try $
        Edit.writePageFile (pageAbsoluteFSPath p) text
    liftIO $ do
      logger ss L.Info $ T.concat [ "metadata of ", count, " changed by ", user, " in ", changedSources ]
      forM_ bySource $ \(source, ps) -> commitPaths ss source (user <> " via Kiwi") message ps
      DB.reloadPages ss
  return ScriptRun { runChanged = length changed
                   , runCommands = reverse done
                   , runReadOnly = L.nub $ [ pageUID p | (p, _) <- changed, not (canWrite p) ]
                                           ++ [ pageUID p | (p, _) <- changed, ref (pageUID p) `elem` refused ] }

-- | What a metadata script changed, or would change when checked
data ScriptRun = ScriptRun
  { runChanged :: Int                     -- ^ number of pages changed
  , runCommands :: [(MS.Command, [Page])] -- ^ the pages changed by each command
  , runReadOnly :: [PageUID]              -- ^ when checked, the pages changed that the
                                          -- account could not change (write right, or
                                          -- admin right for "access")
  }

-- | What a metadata script did (see 'runMetaScript'), or would do when only
-- checked: the number of pages changed, then the number of pages changed by
-- each command, with these pages when checked (at most 20 by command), those
-- that the account can not change marked "(read only)"
metaScriptResult :: Bool -> ScriptRun -> [T.Text]
metaScriptResult check r =
  (if check then T.concat ["check only, nothing written: ", pages (runChanged r), " would change"]
   else pages (runChanged r) <> " changed") :
  [ T.concat [ tshow (length ro), " of them read only for this account: an account that can \
               \change them must apply the script" ]
  | check, let ro = runReadOnly r, not (null ro) ] ++
  concat [ T.concat [ "line ", tshow (MS.commandLine c), ": ", MS.commandText c, ": "
                    , pages (length ps) ]
           : (if check then listed ps else [])
         | (c, ps) <- runCommands r ]
  where
    tshow = T.pack . show
    pages n = tshow n <> if n == 1 then " page" else " pages"
    listed ps = [ T.concat [ "  ", s, "/", u, " ", pageTitle p
                           , if pageUID p `elem` runReadOnly r then " (read only)" else "" ]
                | p <- take 20 ps, let (s, u) = pageUID p ]
                ++ [ T.concat ["  ... and ", tshow (length ps - 20), " more"] | length ps > 20 ]

-- | Stops a write in a read-only source (see 'SourceMode'): the rights of
-- the users already forbid it (see 'Auth.pageAccessLevel'), this is the last
-- check
writableSource :: PagesDB -> T.Text -> ExceptT Edit.SaveFailure IO ()
writableSource db source = unless (U.isWritableSource db source) $ throwError $
  Edit.SaveForbidden $ "the source " ++ T.unpack source ++ " is read-only"

-- | Commits by Kiwi these files of a source before their write: their
-- changes made outside of kiwi, if any, and those not tracked yet (see
-- 'Git.commitFiles'), so that they stay in its history. A failure stops the
-- write.
preCommit :: ServerState -> T.Text -> T.Text -> [FilePath] -> ExceptT Edit.SaveFailure IO ()
preCommit ss source message paths =
  liftIO (Git.commitFiles (contentDir ss FP.</> T.unpack source) paths "Kiwi" message) >>= \case
    Left e -> do
      liftIO $ logger ss L.Warn $ T.pack $ "files not committed before their write: " ++ e
      throwError $ Edit.SaveFailed $ "the files could not be committed before their write: " ++ e
    Right committed -> liftIO $ forM_ committed $ \f ->
      logger ss L.Info $ T.pack $ "pre-update commit: " ++ f

-- | Commits these files of a source, written by kiwi, with this author and
-- message (see 'Git.commitFiles'); the commit and the failure are logged
commitPaths :: ServerState -> T.Text -> T.Text -> T.Text -> [FilePath] -> IO ()
commitPaths ss source author message paths =
  Git.commitFiles (contentDir ss FP.</> T.unpack source) paths author message >>= \case
    Left e -> logger ss L.Warn $ T.pack $ "files not committed: " ++ e
    Right committed -> forM_ committed $ \f -> logger ss L.Info $ T.pack $ "committed: " ++ f

-- | Message of a commit: the line of Kiwi, then the message of the user (if
-- any, its CR removed, at most 2000 characters) after a blank line
commitMessage :: T.Text -> Maybe T.Text -> T.Text
commitMessage line messageM = case T.strip . T.take 2000 . T.filter (/= '\r') <$> messageM of
  Just m | not (T.null m) -> line <> "\n\n" <> m
  _ -> line

-- | Message of the commit of a page: "PREFIX SOURCE:ID (kiwi NAME)",
-- followed by the message of the user, if any (see 'commitMessage')
pageMessage :: ServerState -> T.Text -> PageUID -> Maybe T.Text -> T.Text
pageMessage ss prefix (source, pid) =
  commitMessage (T.concat [ prefix, source, ":", pid, " (kiwi ", kiwiName ss, ")" ])
