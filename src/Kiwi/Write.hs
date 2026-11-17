-- | Writing of the page files changed by a user: the browser editor, and
-- the API. A change (see 'Kiwi.Edit.Change') is checked and written under the
-- lock of the reloads, with git commits, then the pages are reloaded.
module Kiwi.Write
  ( readPageFile
  , updatePage
  , createPage
  ) where

import           Control.Concurrent.MVar (withMVar)
import           Control.Exception (IOException, try)
import           Control.Monad (unless, when)
import           Control.Monad.Except (ExceptT (..), liftEither, runExceptT, throwError, withExceptT)
import qualified Crypto.Random as CR
import           Data.Bifunctor (first)
import qualified Data.List as L
import           Data.Maybe (fromMaybe, isJust)
import           Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString as BS
import           Data.IORef (readIORef)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified System.Directory as D
import qualified System.FilePath as FP

import qualified Kiwi.Auth as Auth
import qualified Kiwi.Edit as Edit
import qualified Kiwi.Git as Git
import qualified Kiwi.Log as L
import qualified Kiwi.PagesDB as DB
import           Kiwi.Meta (clearMeta, sourceMeta, splitPage, uidFromBytes, writePage)
import           Kiwi.Pandoc (loadPage)
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
