{-# LANGUAGE LambdaCase #-}

-- | Commit of the files written by kiwi, when they are in a git repository.
-- The git program is called (no shell, arguments as a list), with a time
-- limit, the standard input closed, and without the GIT_DIR-like variables of
-- the environment, so that git finds the repository of the file.
module Kiwi.Git
  ( CommitResult (..)
  , commitFile
  ) where

import           Control.Exception (IOException, try)
import           Control.Monad.Except (runExceptT, throwError)
import           Control.Monad.IO.Class (liftIO)
import           Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified System.Directory as D
import           System.Environment (getEnvironment)
import           System.Exit (ExitCode (..))
import qualified System.FilePath as FP
import qualified System.Process as Proc
import           System.Timeout (timeout)

-- | What happened to a file to commit
data CommitResult = Committed        -- ^ the file was committed
                  | NotInRepository  -- ^ not in a git repository (or no git)
                  | Ignored          -- ^ ignored by the repository (.gitignore)
                  | Unchanged        -- ^ nothing to commit for this file
  deriving (Eq, Show)

-- | Commits a file alone, with this author (name, without email) and this
-- message, when it is in a git repository and not ignored there; a file that
-- is not tracked yet is added. What is already staged for other files stays
-- staged, out of the commit. The committer is "Kiwi". Fails with the error
-- of git, or when git takes more than 'gitTimeout' (it is then stopped).
commitFile :: FilePath -> T.Text -> T.Text -> IO (Either String CommitResult)
commitFile path author message = do
  r <- timeout gitTimeout $ runExceptT $ do
    file <- liftIO $ D.canonicalizePath path
    env <- liftIO $ gitEnvironment author
    let git dir args = liftIO $ gitIn env dir args
        run dir args = git dir args >>= \case
          Left e -> throwError $ "git: " ++ show e
          Right (ExitSuccess, out, _) -> return out
          Right (ExitFailure _, _, err) -> throwError $ "git " ++ unwords (take 1 args)
                                                        ++ ": " ++ short err
    -- no repository, or no git
    top <- git (FP.takeDirectory file) ["rev-parse", "--show-toplevel"]
    case top of
      Right (ExitSuccess, out, _) | root@(_:_) <- trimEnd out -> do
        ignored <- git root ["check-ignore", "--quiet", "--", file]
        case ignored of
          Right (ExitSuccess, _, _) -> return Ignored
          _ -> do
            _ <- run root ["add", "--", file]
            changed <- git root ["diff", "--cached", "--quiet", "--", file]
            case changed of
              Right (ExitSuccess, _, _) -> return Unchanged
              _ -> do
                _ <- run root ["commit", "--quiet", "-m", T.unpack message, "--", file]
                return Committed
      _ -> return NotInRepository
  return $ fromMaybe (Left "git: time limit reached") r
  where
    trimEnd = reverse . dropWhile (`elem` ("\r\n" :: String)) . reverse
    short = take 500 . trimEnd

-- | Runs git in this folder, with this environment: its exit code and
-- outputs, or the error when it can not be run
gitIn :: [(String, String)] -> FilePath -> [String] ->
         IO (Either IOException (ExitCode, String, String))
gitIn env dir args = try $ Proc.readCreateProcessWithExitCode
  (Proc.proc "git" args) { Proc.cwd = Just dir, Proc.env = Just env } ""

-- | Time limit of the git commands of a commit, in microseconds: a slow hook
-- or a signature that waits for a passphrase must not block the saves
gitTimeout :: Int
gitTimeout = 10 * 1000000

-- | The environment of kiwi, without the variables that would make git use
-- another repository, without prompt, with this author and Kiwi as
-- committer (without email)
gitEnvironment :: T.Text -> IO [(String, String)]
gitEnvironment author = do
  env <- getEnvironment
  return $ [ (k, v) | (k, v) <- env, k `notElem` removed ] ++
           [ ("GIT_TERMINAL_PROMPT", "0")
           , ("GIT_AUTHOR_NAME", T.unpack author)
           , ("GIT_AUTHOR_EMAIL", "")
           , ("GIT_COMMITTER_NAME", "Kiwi")
           , ("GIT_COMMITTER_EMAIL", "") ]
  where removed = [ "GIT_DIR", "GIT_WORK_TREE", "GIT_INDEX_FILE", "GIT_OBJECT_DIRECTORY"
                  , "GIT_ALTERNATE_OBJECT_DIRECTORIES", "GIT_TERMINAL_PROMPT"
                  , "GIT_AUTHOR_NAME", "GIT_AUTHOR_EMAIL", "GIT_AUTHOR_DATE"
                  , "GIT_COMMITTER_NAME", "GIT_COMMITTER_EMAIL", "GIT_COMMITTER_DATE" ]
