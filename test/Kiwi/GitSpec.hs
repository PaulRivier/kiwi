module Kiwi.GitSpec (gitSpec) where

import           Control.Monad (when)
import qualified System.Directory as D
import qualified System.FilePath as FP
import           Test.Hspec

import           Kiwi.Git
import           TestUtils (git, gitOut, withContentDir, withGit)

gitSpec :: Spec
gitSpec = do
  describe "isRepositoryRoot" $ do
    it "is true for the top of a repository" $ withRepo True $ \dir ->
      isRepositoryRoot dir `shouldReturn` True

    it "is false outside of a repository" $ withRepo False $ \dir ->
      isRepositoryRoot dir `shouldReturn` False

    it "is false for a folder inside a repository" $ withRepo True $ \dir -> do
      D.createDirectory (dir FP.</> "sub")
      isRepositoryRoot (dir FP.</> "sub") `shouldReturn` False

    it "is true for a worktree, whose .git is a file" $ withRepo True $ \dir -> do
      writeFile (dir FP.</> "p.md") "a\n"
      git dir ["add", "p.md"]
      git dir ["commit", "-q", "-m", "init"]
      let wt = FP.takeDirectory dir FP.</> "wt"
      git dir ["worktree", "add", "-q", wt]
      isRepositoryRoot wt `shouldReturn` True

  describe "commitFiles" $ do
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
        commitFiles dir [dir FP.</> "p.md"] "bob" "Edit s:p (kiwi K)"
          `shouldReturn` Right [dir FP.</> "p.md"]
        gitOut dir ["log", "-1", "--format=%an <%ae>|%cn <%ce>|%s"]
          `shouldReturn` "bob <>|Kiwi <>|Edit s:p (kiwi K)\n"
        gitOut dir ["show", "--name-only", "--format=", "HEAD"] `shouldReturn` "p.md\n"
        gitOut dir ["status", "--porcelain"] `shouldReturn` "M  other.md\n"

    it "adds a file that is not tracked yet" $ withRepo True $ \dir -> do
      D.createDirectory (dir FP.</> "sub")
      writeFile (dir FP.</> "sub" FP.</> "new.md") "n\n"
      commitFiles dir [dir FP.</> "sub" FP.</> "new.md"] "bob" "Edit s:sub:new"
        `shouldReturn` Right [dir FP.</> "sub" FP.</> "new.md"]
      gitOut dir ["ls-files"] `shouldReturn` "sub/new.md\n"

    it "commits a deleted file" $ withRepo True $ \dir -> do
      writeFile (dir FP.</> "p.md") "a\n"
      git dir ["add", "p.md"]
      git dir ["commit", "-q", "-m", "init"]
      D.removeFile (dir FP.</> "p.md")
      commitFiles dir [dir FP.</> "p.md"] "bob" "Delete s:p"
        `shouldReturn` Right [dir FP.</> "p.md"]
      gitOut dir ["ls-files"] `shouldReturn` ""

    it "fails on an ignored file" $ withRepo True $ \dir -> do
      writeFile (dir FP.</> ".gitignore") "drafts/\n"
      D.createDirectory (dir FP.</> "drafts")
      writeFile (dir FP.</> "drafts" FP.</> "d.md") "d\n"
      r <- commitFiles dir [dir FP.</> "drafts" FP.</> "d.md"] "bob" "Edit s:d"
      either (const True) (const False) r `shouldBe` True
      gitOut dir ["ls-files"] `shouldReturn` ""

    it "does not commit a file without change" $ withRepo True $ \dir -> do
      writeFile (dir FP.</> "p.md") "a\n"
      git dir ["add", "p.md"]
      git dir ["commit", "-q", "-m", "init"]
      commitFiles dir [dir FP.</> "p.md"] "bob" "Edit s:p" `shouldReturn` Right []
      gitOut dir ["rev-list", "--count", "HEAD"] `shouldReturn` "1\n"

    it "commits several files in one commit" $ withRepo True $ \dir -> do
      D.createDirectory (dir FP.</> "sub")
      writeFile (dir FP.</> "a.md") "a\n"
      writeFile (dir FP.</> "same.md") "s\n"
      git dir ["add", "same.md"]
      git dir ["commit", "-q", "-m", "init"]
      writeFile (dir FP.</> "sub" FP.</> "b.md") "b\n"
      let files = map (dir FP.</>) ["a.md", "sub/b.md", "same.md"]
      commitFiles dir files "Kiwi" "Kiwi: 3 pages"
        `shouldReturn` Right (map (dir FP.</>) ["a.md", "sub/b.md"])
      gitOut dir ["show", "--name-only", "--format=%s", "HEAD"]
        `shouldReturn` "Kiwi: 3 pages\n\na.md\nsub/b.md\n"

    it "fails outside of a repository" $ withRepo False $ \dir -> do
      writeFile (dir FP.</> "p.md") "a\n"
      r <- commitFiles dir [dir FP.</> "p.md"] "bob" "Edit s:p"
      either (const True) (const False) r `shouldBe` True

-- | Runs the test in a new temporary folder, a git repository or not
-- (pending when git is not installed)
withRepo :: Bool -> (FilePath -> IO ()) -> IO ()
withRepo repo test = withGit $ withContentDir $ \cd -> do
  let dir = cd FP.</> "repo"
  D.createDirectory dir
  when repo $ git dir ["init", "-q"]
  test dir
