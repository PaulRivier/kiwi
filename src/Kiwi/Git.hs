{-# LANGUAGE LambdaCase #-}

-- | Commit of the files written by kiwi, when they are in a git repository.
-- The git program is called (no shell, arguments as a list), with a time
-- limit, the standard input closed, and without the GIT_DIR-like variables of
-- the environment, so that git finds the repository of the file.
module Kiwi.Git
  ( CommitResult (..)
  , commitFile
  , commitFiles
  ) where

import           Control.Exception (IOException, try)
import           Control.Monad (unless, void)
import           Control.Monad.Except (runExceptT, throwError)
import           Control.Monad.IO.Class (liftIO)
import qualified Data.Map.Strict as M
import           Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified System.Directory as D
import           System.Environment (getEnvironment)
import           System.Exit (ExitCode (..))
import qualified System.FilePath as FP
import qualified System.Process as Proc
import           System.Timeout (timeout)

import           Kiwi.Utils (nodup)

-- | What happened to a file to commit
data CommitResult = Committed        -- ^ the file was committed
                  | NotInRepository  -- ^ not in a git repository (or no git)
                  | Ignored          -- ^ ignored by the repository (.gitignore)
                  | Unchanged        -- ^ nothing to commit for this file
  deriving (Eq, Show)

-- | Commits a file alone (see 'commitFiles')
commitFile :: FilePath -> T.Text -> T.Text -> IO (Either String CommitResult)
commitFile path author message = commitFiles [path] author message >>= \case
  [(_, r)] -> return r
  _        -> return $ Left "git: no result"

-- | Commits these files, with this author (name, without email) and this
-- message, when they are in a git repository and not ignored there: one
-- commit by repository, of the files that changed. The files that are not
-- tracked yet are added. What is already staged for other files stays
-- staged, out of the commit. The committer is "Kiwi". The result of each
-- file: a failure is the error of git (for the files of its repository), or
-- the time limit ('gitTimeout', git is then stopped).
commitFiles :: [FilePath] -> T.Text -> T.Text -> IO [(FilePath, Either String CommitResult)]
commitFiles paths author message = do
  env <- gitEnvironment author
  r <- timeout gitTimeout $ do
    files <- mapM (\p -> (p,) <$> D.canonicalizePath p) paths
    -- the repository of each folder, if any
    let dirs = nodup $ map (FP.takeDirectory . snd) files
    roots <- M.fromList <$> mapM (\d -> (d,) <$> repositoryRoot env d) dirs
    let byRoot = M.fromListWith (flip (++))
                   [ (M.findWithDefault Nothing (FP.takeDirectory c) roots, [f])
                   | f@(_, c) <- files ]
    concat <$> mapM (uncurry $ commitIn env) (M.toList byRoot)
  return $ fromMaybe [ (p, Left "git: time limit reached") | p <- paths ] r
  where
    commitIn _ Nothing fs = return [ (p, Right NotInRepository) | (p, _) <- fs ]
    commitIn env (Just root) fs = do
      results <- runExceptT $ do
        let git args input = liftIO (gitIn env root args input) >>= \case
              Left e -> throwError $ "git: " ++ show e
              Right x -> return x
            run args input = git args input >>= \case
              (ExitSuccess, out, _) -> return out
              (ExitFailure _, _, err) -> throwError $ "git " ++ unwords (take 1 args)
                                                      ++ ": " ++ short err
            nulList = concatMap (++ "\0")
        -- exit code 1 when no file is ignored
        ignoredOut <- git ["check-ignore", "-z", "--stdin"] (nulList $ map snd fs) >>= \case
          (ExitFailure 1, _, _) -> return ""
          (ExitSuccess, out, _) -> return out
          (ExitFailure _, _, err) -> throwError $ "git check-ignore: " ++ short err
        let ignored = splitNul ignoredOut
            kept = [ f | f@(_, c) <- fs, c `notElem` ignored ]
            keptPaths = nulList $ map snd kept
        if null kept then return [] else do
          _ <- run ["add", "--pathspec-from-file=-", "--pathspec-file-nul"] keptPaths
          -- every staged file, those of the admin too; without rename
          -- detection, which would hide the former path of a renamed file
          staged <- splitNul <$> run ["diff", "--cached", "--name-only", "--no-renames", "-z"] ""
          let changed = [ f | f@(_, c) <- kept, FP.makeRelative root c `elem` staged ]
          unless (null changed) $ void $
            run [ "commit", "--quiet", "-m", T.unpack message
                , "--pathspec-from-file=-", "--pathspec-file-nul" ]
                (nulList $ map snd changed)
          return [ (p, Right $ if f `elem` changed then Committed else Unchanged)
                 | f@(p, _) <- kept ]
      return $ case results of
        Left e -> [ (p, Left e) | (p, _) <- fs ]
        Right rs -> rs ++ [ (p, Right Ignored) | (p, _) <- fs, p `notElem` map fst rs ]
    short = take 500 . trimEnd
    splitNul = filter (not . null) . splitOn
    splitOn str = case break (== '\0') str of
      (a, [])     -> [a]
      (a, _ : bs) -> a : splitOn bs

-- | Top folder of the git repository of this folder, if any (or no git)
repositoryRoot :: [(String, String)] -> FilePath -> IO (Maybe FilePath)
repositoryRoot env dir = gitIn env dir ["rev-parse", "--show-toplevel"] "" >>= \case
  Right (ExitSuccess, out, _) | root@(_:_) <- trimEnd out -> return $ Just root
  _ -> return Nothing

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
