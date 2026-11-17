-- | Writing of the page files changed by a user: the browser editor, and
-- the API. A change (see 'Kiwi.Edit.Change') is checked and written under the
-- lock of the reloads, with git commits, then the pages are reloaded.
module Kiwi.Write
  ( readPageFile
  , updatePage
  , createPage
  , runMetaScript
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
import qualified Utils.DocIndex as DI


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
-- 'Edit.checkSave'). When the file is in a git repository (unless git-commit
-- is no; a failure of git is only logged), its changes made outside of kiwi,
-- if any, are first committed by Kiwi, and the new file is committed by the
-- user. The pages are then reloaded. The check, the commits, the write and
-- the reload hold the lock of the reloads. The result is the hash of the
-- written file, and the keys of the change that are ignored.
updatePage :: ServerState -> T.Text -> Page -> Edit.Change ->
              IO (Either Edit.SaveFailure (T.Text, [T.Text]))
updatePage ss user p change = withMVar (pagesDBLock ss) $ \_ -> runExceptT $ do
  let path = pageAbsoluteFSPath p
      uid = pageUID p
  db <- liftIO $ readIORef (pagesDB ss)
  file <- withExceptT Edit.SaveFailed $ ExceptT $ readPageFile path
  (text, ignored) <- liftEither $ Edit.checkSave (contentDir ss) db change path file
  liftIO $ when (gitCommit ss) $
    commitPage ss "Kiwi" "pre-update commit" "Pre-update commit of " path uid
  withExceptT (\e -> Edit.SaveFailed $ show (e :: IOException)) $ ExceptT $ try $
    Edit.writePageFile path text
  liftIO $ do
    logger ss L.Info $ T.pack $ concat [ "page saved by ", T.unpack user, ": ", path ]
    when (gitCommit ss) $
      commitPage ss (user <> " via Kiwi") "page" "Edit " path uid
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
-- source, or of the header for an admin). The page is committed (unless
-- git-commit is no) and the pages are reloaded, under the lock of the
-- reloads. The result is the uid of the page and the path of its file, from
-- the top of the source.
createPage :: ServerState -> Maybe Login -> T.Text -> Maybe FilePath -> T.Text ->
              IO (Either Edit.SaveFailure (PageUID, FilePath))
createPage ss loginM source pathM sent = withMVar (pagesDBLock ss) $ \_ -> runExceptT $ do
  db <- liftIO $ readIORef (pagesDB ss)
  let cmc = customMetaConfig db
      top = contentDir ss FP.</> T.unpack source
      admin = maybe False (elem "admin" . loginGroups) loginM
      user = maybe "" loginUser loginM
      scM = L.find ((== source) . scSourceName) (sourcesConfig db)
  exists <- liftIO $ D.doesDirectoryExist top
  unless exists $ throwError $ Edit.SaveInvalid $ "no source " ++ T.unpack source
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
    when (gitCommit ss) $
      commitPage ss (user <> " via Kiwi") "page" "Create " path (source, uid)
    DB.reloadPages ss
  return ((source, uid), FP.makeRelative top path)
  where
    newUid db = do
      u <- uidFromBytes <$> (CR.getRandomBytes 10 :: IO BS.ByteString)
      if isJust (DI.findDoc (pagesIndex db) (source, u)) then newUid db else return u
    -- the first name not taken: NAME.md, NAME (2).md...
    freePath base = go (1 :: Int)
      where go n = do
              let p = (if n == 1 then base else base ++ " (" ++ show n ++ ")") FP.<.> "md"
              taken <- (||) <$> D.doesPathExist p <*> D.doesPathExist (p FP.-<.> "markdown")
              if taken then go (n + 1) else return p

-- | Applies the commands of a metadata script (see "Kiwi.MetaScript") to the
-- headers of the pages of a source that the user can read, archives
-- included, in their order, all of them or none: a command that can not
-- apply to a page, a command on all the pages that changes none (a typo,
-- most likely), a page of a command that does not exist, or a changed page
-- that the user can not write (admin right for "access") stops everything.
-- The changed files are written, committed in one commit whose message
-- holds the commands (after a commit of their former changes, see
-- 'updatePage'), and the pages are reloaded, under the lock of the reloads.
-- The result is the number of pages changed by each command.
runMetaScript :: ServerState -> Maybe Login -> T.Text -> [MS.Command] ->
                 IO (Either Edit.SaveFailure [(MS.Command, Int)])
runMetaScript ss loginM source cmds = withMVar (pagesDBLock ss) $ \_ -> runExceptT $ do
  db <- liftIO $ readIORef (pagesDB ss)
  let cmc = customMetaConfig db
      pages = DI.query (pagesIndex db) $
                DI.And [U.qOnly FieldSource source, Auth.readableAnyArchiveQuery loginM]
      user = maybe "" loginUser loginM
  when (null pages) $ throwError $ Edit.SaveInvalid $ "no page in source " ++ T.unpack source
  files <- fmap M.fromList $ forM pages $ \p -> do
    (_, text) <- withExceptT Edit.SaveFailed $ ExceptT $ readPageFile (pageAbsoluteFSPath p)
    (header, content) <- liftEither $ first (\e -> Edit.SaveFailed $ concat
                           [pageAbsoluteFSPath p, ": ", e]) $ splitPage cmc text
    return (snd $ pageUID p, (p, header, content))
  let ref uid = T.unpack source ++ "/" ++ T.unpack uid
      at c = "line " ++ show (MS.commandLine c) ++ " (" ++ T.unpack (MS.commandText c) ++ "): "
      tagsOfSource = [ t | KeyText t <- DI.getFieldKeys (DI.narrow (pagesIndex db)
                                          (U.qOnly FieldSource source)) FieldTag ]
      -- the tags of the source close to a tag that no page has
      closeTags t = take 5 [ Tags.prettyTag x | x <- tagsOfSource
                           , let l = T.toLower (last' $ Tags.tagToSegments t)
                           , not (T.null l), l `T.isInfixOf` T.toLower x ]
      last' xs = if null xs then "" else last xs
      step (headers, counts, admins) c = case MS.commandScope c of
        MS.OnePage uid -> case M.lookup uid headers of
          Nothing -> Left $ at c ++ "no page " ++ ref uid
          Just h -> do
            r <- first (at c ++) $ MS.applyCommand cmc c h
            let n = maybe 0 (const 1) r
            return ( maybe headers (\h' -> M.insert uid h' headers) r
                   , (c, n) : counts
                   , [ uid | MS.commandField c == "access", n > 0 ] ++ admins )
        MS.AllPages -> do
          results <- forM (M.toList headers) $ \(uid, h) ->
            (,) uid <$> first (\e -> at c ++ ref uid ++ ": " ++ e) (MS.applyCommand cmc c h)
          let changed = [ (uid, h') | (uid, Just h') <- results ]
          when (null changed) $ Left $ at c ++ "it changes no page" ++
            case (MS.commandField c, MS.commandOp c) of
              ("tags", MS.Move a _) -> hint (closeTags a)
              ("tags", MS.Delete a) -> hint (closeTags a)
              _ -> ""
          return ( M.union (M.fromList changed) headers
                 , (c, length changed) : counts
                 , [ uid | MS.commandField c == "access", (uid, _) <- changed ] ++ admins )
      hint [] = ""
      hint ts = "; tags close to it: " ++ T.unpack (T.intercalate ", " ts)
  (final, counts, admins) <- liftEither $ first Edit.SaveInvalid $
    foldM step (M.map (\(_, h, _) -> h) files, [], []) cmds
  let changed = [ (p, writePage cmc h content)
                | (uid, (p, original, content)) <- M.toList files
                , Just h <- [M.lookup uid final]
                , writeHeader cmc h /= writeHeader cmc original ]
      refused = [ ref (snd $ pageUID p) | (p, _) <- changed
                , let needed = if snd (pageUID p) `elem` admins then AccessAdmin else AccessWrite
                , not (maybe False (>= needed) $ Auth.pageAccessLevel loginM p) ]
  unless (null refused) $ throwError $ Edit.SaveForbidden $
    "the account can not change these pages: " ++ L.intercalate ", " refused
  forM_ changed $ \(p, text) ->
    liftEither $ first (\e -> Edit.SaveInvalid $ ref (snd $ pageUID p) ++ ": " ++ e) $
      checkPageText (contentDir ss) db (pageAbsoluteFSPath p) text
  unless (null changed) $ do
    let paths = map (pageAbsoluteFSPath . fst) changed
        count = T.pack (show $ length changed) <> " pages"
        message = T.concat [ "Metadata of ", source, ": ", count, " (kiwi ", kiwiName ss, ")\n\n"
                           , T.unlines (map MS.commandText cmds) ]
    liftIO $ when (gitCommit ss) $
      commitPaths ss "Kiwi" ("Pre-update commit of " <> count <> " (kiwi " <> kiwiName ss <> ")") paths
    forM_ changed $ \(p, text) ->
      withExceptT (\e -> Edit.SaveFailed $ show (e :: IOException)) $ ExceptT $ try $
        Edit.writePageFile (pageAbsoluteFSPath p) text
    liftIO $ do
      logger ss L.Info $ T.concat [ "metadata of ", count, " changed by ", user, " in ", source ]
      when (gitCommit ss) $ commitPaths ss (user <> " via Kiwi") message paths
      DB.reloadPages ss
  return (reverse counts)

-- | Commits these files (see 'Git.commitFiles'); the failures are logged
commitPaths :: ServerState -> T.Text -> T.Text -> [FilePath] -> IO ()
commitPaths ss author message paths = do
  rs <- Git.commitFiles paths author message
  forM_ (L.nub [ e | (_, Left e) <- rs ]) $ \e ->
    logger ss L.Warn $ T.pack $ "files not committed: " ++ e

-- | Commits the file of a page, when it is in a git repository and differs
-- from its last commit, with this author and the message "PREFIX SOURCE:ID
-- (kiwi NAME)"; the commit (named by what in the log) or its failure is logged
commitPage :: ServerState -> T.Text -> String -> T.Text -> FilePath -> PageUID -> IO ()
commitPage ss author what prefix path (source, pid) = do
  let message = T.concat [ prefix, source, ":", pid, " (kiwi ", kiwiName ss, ")" ]
  r <- Git.commitFile path author message
  case r of
    Right Git.Committed -> logger ss L.Info $ T.pack $ concat [ what, " committed: ", path ]
    Right _ -> return ()
    Left e -> logger ss L.Warn $ T.pack $ concat [ what, " not committed: ", path, ": ", e ]
