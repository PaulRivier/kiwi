{-# LANGUAGE LambdaCase #-}

-- | Commit of the files written by kiwi in a source, whose folder is the top
-- of a git repository (see 'isRepositoryRoot'). The git program is called
-- (no shell, arguments as a list), with a time limit, the standard input
-- closed, and without the GIT_DIR-like variables of the environment, so
-- that git finds the repository of the folder.
module Kiwi.Git
  ( isRepositoryRoot
  , commitFiles
  ) where

import           Control.Exception (IOException, try)
import           Control.Monad (void)
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

-- | Whether this folder is the top of a git repository: git runs there, and
-- the top of its repository is this folder (not one of its parents). False
-- without git, or when git refuses the repository (owned by another user…).
isRepositoryRoot :: FilePath -> IO Bool
isRepositoryRoot dir = do
  env <- gitEnvironment "Kiwi"
  r <- timeout gitTimeout $ gitIn env dir ["rev-parse", "--show-toplevel"] "" >>= \case
    Right (ExitSuccess, out, _) | top@(_:_) <- trimEnd out -> do
      same <- try $ (==) <$> D.canonicalizePath top <*> D.canonicalizePath dir
      return $ either (\(_ :: IOException) -> False) id same
    _ -> return False
  return $ fromMaybe False r

-- | Commits these files of the repository whose top folder is given (paths
-- in this folder), with this author (name, without email) and this message:
-- one commit of the files that changed, deleted ones included. The files
-- that are not tracked yet are added. What is already staged for other
-- files stays staged, out of the commit. The committer is "Kiwi". The
-- result is the files committed (those that changed), or the error of git
-- (an ignored file, for example), or the time limit ('gitTimeout', git is
-- then stopped).
commitFiles :: FilePath -> [FilePath] -> T.Text -> T.Text -> IO (Either String [FilePath])
commitFiles _ [] _ _ = return $ Right []
commitFiles root paths author message = do
  env <- gitEnvironment author
  r <- timeout gitTimeout $ runExceptT $ do
    let git args input = liftIO (gitIn env root args input) >>= \case
          Left e -> throwError $ "git: " ++ show e
          Right (ExitSuccess, out, _) -> return out
          Right (ExitFailure _, _, err) -> throwError $ "git " ++ unwords (take 1 args)
                                                        ++ ": " ++ short err
        -- paths from the top of the repository, as git gives them
        files = [ (p, FP.normalise $ FP.makeRelative root p) | p <- paths ]
        nulList = concatMap (++ "\0")
    _ <- git ["add", "--pathspec-from-file=-", "--pathspec-file-nul"] (nulList $ map snd files)
    -- every staged file, those of the admin too; without rename detection,
    -- which would hide the former path of a renamed file
    staged <- splitNul <$> git ["diff", "--cached", "--name-only", "--no-renames", "-z"] ""
    let changed = [ f | f@(_, rel) <- files, rel `elem` staged ]
    if null changed then return [] else do
      void $ git [ "commit", "--quiet", "-m", T.unpack message
                 , "--pathspec-from-file=-", "--pathspec-file-nul" ]
                 (nulList $ map snd changed)
      return $ map fst changed
  return $ fromMaybe (Left "git: time limit reached") r
  where
    short = take 500 . trimEnd
    splitNul = filter (not . null) . splitOn
    splitOn str = case break (== '\0') str of
      (a, [])     -> [a]
      (a, _ : bs) -> a : splitOn bs

trimEnd :: String -> String
trimEnd = reverse . dropWhile (`elem` ("\r\n" :: String)) . reverse

-- | Runs git in this folder, with this environment and this input: its exit
-- code and outputs, or the error when it can not be run
gitIn :: [(String, String)] -> FilePath -> [String] -> String ->
         IO (Either IOException (ExitCode, String, String))
gitIn env dir args input = try $ Proc.readCreateProcessWithExitCode
  (Proc.proc "git" args) { Proc.cwd = Just dir, Proc.env = Just env } input

-- | Time limit of the git commands of a commit, in microseconds: a slow hook
-- or a signature that waits for a passphrase must not block the saves
gitTimeout :: Int
gitTimeout = 60 * 1000000

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
