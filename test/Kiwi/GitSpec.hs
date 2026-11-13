{-# LANGUAGE LambdaCase #-}

module Kiwi.GitSpec (gitSpec) where

import           Control.Monad (when)
import qualified System.Directory as D
import qualified System.FilePath as FP
import qualified System.Process as Proc
import           Test.Hspec

import           Kiwi.Git
import           TestUtils (withContentDir)

gitSpec :: Spec
gitSpec = do
  it "does nothing outside of a repository" $ withRepo False $ \dir -> do
    writeFile (dir FP.</> "p.md") "a\n"
    commitFile (dir FP.</> "p.md") "bob" "Edit s:p" `shouldReturn` Right NotInRepository

  it "commits a file alone, with its author, Kiwi as committer and the message" $
    withRepo True $ \dir -> do
      writeFile (dir FP.</> "p.md") "a\n"
      writeFile (dir FP.</> "other.md") "o\n"
      git dir ["add", "p.md", "other.md"]
      git dir ["commit", "-q", "-m", "init"]
      writeFile (dir FP.</> "p.md") "b\n"
      -- staged by the admin, out of the commit of kiwi
      writeFile (dir FP.</> "other.md") "o2\n"
      git dir ["add", "other.md"]
      commitFile (dir FP.</> "p.md") "bob" "Edit s:p (kiwi K)" `shouldReturn` Right Committed
      gitOut dir ["log", "-1", "--format=%an <%ae>|%cn <%ce>|%s"]
        `shouldReturn` "bob <>|Kiwi <>|Edit s:p (kiwi K)\n"
      gitOut dir ["show", "--name-only", "--format=", "HEAD"] `shouldReturn` "p.md\n"
      gitOut dir ["status", "--porcelain"] `shouldReturn` "M  other.md\n"

  it "adds a file that is not tracked yet" $ withRepo True $ \dir -> do
    D.createDirectory (dir FP.</> "sub")
    writeFile (dir FP.</> "sub" FP.</> "new.md") "n\n"
    commitFile (dir FP.</> "sub" FP.</> "new.md") "bob" "Edit s:sub:new" `shouldReturn` Right Committed
    gitOut dir ["ls-files"] `shouldReturn` "sub/new.md\n"

  it "does not commit an ignored file" $ withRepo True $ \dir -> do
    writeFile (dir FP.</> ".gitignore") "drafts/\n"
    D.createDirectory (dir FP.</> "drafts")
    writeFile (dir FP.</> "drafts" FP.</> "d.md") "d\n"
    commitFile (dir FP.</> "drafts" FP.</> "d.md") "bob" "Edit s:d" `shouldReturn` Right Ignored
    gitOut dir ["ls-files"] `shouldReturn` ""

  it "does not commit a file without change" $ withRepo True $ \dir -> do
    writeFile (dir FP.</> "p.md") "a\n"
    git dir ["add", "p.md"]
    git dir ["commit", "-q", "-m", "init"]
    commitFile (dir FP.</> "p.md") "bob" "Edit s:p" `shouldReturn` Right Unchanged
    gitOut dir ["rev-list", "--count", "HEAD"] `shouldReturn` "1\n"

-- | Runs the test in a new temporary folder, a git repository or not
-- (pending when git is not installed)
withRepo :: Bool -> (FilePath -> IO ()) -> IO ()
withRepo repo test = D.findExecutable "git" >>= \case
  Nothing -> pendingWith "git is not installed"
  Just _ -> withContentDir $ \cd -> do
    let dir = cd FP.</> "repo"
    D.createDirectory dir
    when repo $ git dir ["init", "-q"]
    test dir

git :: FilePath -> [String] -> IO ()
git dir args = () <$ gitOut dir args

-- | Output of git in this folder, with an identity for the commits of the
-- tests
gitOut :: FilePath -> [String] -> IO String
gitOut dir args = Proc.readCreateProcess
  (Proc.proc "git" (["-c", "user.name=test", "-c", "user.email=test@test"] ++ args))
    { Proc.cwd = Just dir } ""
