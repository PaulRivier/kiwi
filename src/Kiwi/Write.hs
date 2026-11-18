-- | Writing of the page files changed by a user: the browser editor, and
-- the API. A change (see 'Kiwi.Edit.Change') is checked and written under the
-- lock of the reloads, with git commits, then the pages are reloaded.
module Kiwi.Write
  ( readPageFile
  , updatePage
  , createPage
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
runMetaScript :: ServerState -> Maybe Login -> Bool -> [MS.Command] ->
                 IO (Either Edit.SaveFailure ScriptRun)
runMetaScript ss loginM check cmds = withMVar (pagesDBLock ss) $ \_ -> runExceptT $ do
  db <- liftIO $ readIORef (pagesDB ss)
  let cmc = customMetaConfig db
      readable = DI.query (pagesIndex db) (Auth.readableAnyArchiveQuery loginM)
      byUid = M.fromList [ (pageUID p, p) | p <- readable ]
      canWrite p = maybe False (>= AccessWrite) (Auth.pageAccessLevel loginM p)
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
        , KeyText x <- DI.getFieldKeys (DI.narrow (pagesIndex db) (U.qOnly FieldSource s)) FieldTag
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
                , not (maybe False (>= AccessAdmin) $ Auth.pageAccessLevel loginM p) ]
  unless (check || null refused) $ throwError $ Edit.SaveForbidden $
    "the account can not change the access of these pages: " ++ L.intercalate ", " refused
  forM_ changed $ \(p, text) ->
    liftEither $ first (\e -> Edit.SaveInvalid $ ref (pageUID p) ++ ": " ++ e) $
      checkPageText (contentDir ss) db (pageAbsoluteFSPath p) text
  unless (check || null changed) $ do
    let paths = map (pageAbsoluteFSPath . fst) changed
        changedSources = T.intercalate ", " $ L.sort $ L.nub $ map (fst . pageUID . fst) changed
        count = T.pack (show $ length changed) <> " pages"
        message = T.concat [ "Metadata of ", changedSources, ": ", count, " (kiwi ", kiwiName ss, ")\n\n"
                           , T.unlines (MS.scriptText cmds) ]
    liftIO $ when (gitCommit ss) $
      commitPaths ss "Kiwi" ("Pre-update commit of " <> count <> " (kiwi " <> kiwiName ss <> ")") paths
    forM_ changed $ \(p, text) ->
      withExceptT (\e -> Edit.SaveFailed $ show (e :: IOException)) $ ExceptT $ try $
        Edit.writePageFile (pageAbsoluteFSPath p) text
    liftIO $ do
      logger ss L.Info $ T.concat [ "metadata of ", count, " changed by ", user, " in ", changedSources ]
      when (gitCommit ss) $ commitPaths ss (user <> " via Kiwi") message paths
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
