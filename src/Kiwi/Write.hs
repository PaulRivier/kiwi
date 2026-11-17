-- | Writing of the page files changed by a user: the browser editor, and
-- the API. A change (see 'Kiwi.Edit.Change') is checked and written under the
-- lock of the reloads, with git commits, then the pages are reloaded.
module Kiwi.Write
  ( readPageFile
  , updatePage
  ) where

import           Control.Concurrent.MVar (withMVar)
import           Control.Exception (IOException, try)
import           Control.Monad (when)
import           Control.Monad.Except (ExceptT (..), liftEither, runExceptT, withExceptT)
import           Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString as BS
import           Data.IORef (readIORef)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import qualified Kiwi.Edit as Edit
import qualified Kiwi.Git as Git
import qualified Kiwi.Log as L
import qualified Kiwi.PagesDB as DB
import           Kiwi.Types


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
