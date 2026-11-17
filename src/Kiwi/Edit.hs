-- | Edition of a page file in the browser: the text shown in the editor, the
-- text saved from the one sent back and its checks, and its writing. The
-- users with the write right on a page do not see nor change the metadata of
-- 'adminKeys', and no one sees nor changes those of 'kiwiKeys' (uid); the
-- headers are written normalized (see 'writeHeader').
module Kiwi.Edit
  ( fileHash
  , editorText
  , editedText
  , SaveFailure (..)
  , Change
  , sentText
  , checkSave
  , writePageFile
  ) where

import           Control.Exception (IOException, bracketOnError, try)
import           Control.Monad (void, when)
import           Data.Bifunctor (first)
import qualified Crypto.Hash as H
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified System.Directory as D
import qualified System.FilePath as FP
import           System.IO (hClose, openTempFile)
import qualified System.Posix.Files as PF

import           Kiwi.Meta
import           Kiwi.Pandoc (checkPageText)
import           Kiwi.Types


-- | Hash of the content of a file (SHA-256, hexadecimal), to detect that it
-- changed between its edition and its save
fileHash :: BS.ByteString -> T.Text
fileHash bytes = T.pack $ show (H.hash bytes :: H.Digest H.SHA256)

-- | Text of a page file to edit with this right, and the number of lines
-- before its content: the line n of the content is the line (n + offset) of
-- the text
editorText :: [CustomMetaConfig] -> AccessLevel -> T.Text -> Either String (T.Text, Int)
editorText cmc level fileText = do
  (header, content) <- splitPage cmc fileText
  let shown = restrict level header
      text = writePage cmc shown content
      headerLines = T.count "\n" (writeHeader cmc shown)
  -- a page without metadata starts with a blank line (see 'writePage')
  return (text, max 1 headerLines + 1)

-- | Text of a page file saved by an editor with this right, from the text of
-- the file and the text sent, and the keys sent that the editor can not
-- change (ignored): their values are those of the file. Fails when the
-- header sent is invalid.
editedText :: [CustomMetaConfig] -> AccessLevel -> T.Text -> T.Text ->
              Either String (T.Text, [T.Text])
editedText cmc level fileText sent = do
  (original, _) <- splitPage cmc fileText
  (edited, content) <- splitPage cmc sent
  let protected = protectedKeys level
      ignored = filter (`elem` protected) (metaKeys edited)
      header = mergeMeta (keepOnlyMeta protected original) (clearMeta protected edited)
  return (writePage cmc header content, ignored)

-- | Why a page could not be saved
data SaveFailure = SaveConflict         -- ^ the file changed since its edition
                 | SaveInvalid String   -- ^ the text can not be saved
                 | SaveFailed String    -- ^ error of the server
  deriving (Eq, Show)

-- | A change of a page file, from the custom metadata of the config and the
-- file (its bytes and its text): the text to write, with the keys of the
-- change that are ignored (see 'editedText'), or why it can not be made
type Change = [CustomMetaConfig] -> (BS.ByteString, T.Text) ->
              Either SaveFailure (T.Text, [T.Text])

-- | The text sent by an editor with this right (see 'editedText'), for the
-- file read with this hash: the file must not have changed since
sentText :: AccessLevel -> T.Text -> T.Text -> Change
sentText level hash sent cmc (bytes, fileText) = do
  when (fileHash bytes /= hash) $ Left SaveConflict
  first SaveInvalid $ editedText cmc level fileText sent

-- | Text of a page file to save, by this change of the file (its bytes and
-- text, read in the content folder at this path), with the ignored keys:
-- the page of the new text must load. The uid of the page can not change.
checkSave :: FilePath -> PagesDB -> Change -> FilePath -> (BS.ByteString, T.Text) ->
             Either SaveFailure (T.Text, [T.Text])
checkSave contentDir' db change path file = do
  (text, ignored) <- change (customMetaConfig db) file
  first SaveInvalid $ checkPageText contentDir' db path text
  return (text, ignored)

-- | Keys of the header that an editor with this right can not see nor change
protectedKeys :: AccessLevel -> [T.Text]
protectedKeys level = kiwiKeys ++ if level >= AccessAdmin then [] else adminKeys

-- | The header without the keys an editor with this right can not see
restrict :: AccessLevel -> MetaData -> MetaData
restrict level = clearMeta (protectedKeys level)


-- | Writes the text of a page file atomically: the text goes to a temporary
-- file of the same folder (a dot file, which is never loaded as a page),
-- which then replaces the file. The new file keeps the mode of the former
-- one, and its owner and group when possible. A symbolic link is followed:
-- its target is replaced.
writePageFile :: FP.FilePath -> T.Text -> IO ()
writePageFile path text = do
  p <- D.canonicalizePath path
  st <- PF.getFileStatus p
  bracketOnError (openTempFile (FP.takeDirectory p) ("." ++ FP.takeFileName p))
                 (\(tmp, h) -> hClose h >> void (try (D.removeFile tmp) :: IO (Either IOException ())))
                 (\(tmp, h) -> do
                     BS.hPut h (TE.encodeUtf8 text)
                     hClose h
                     PF.setFileMode tmp (PF.fileMode st)
                     -- only root can give a file to another user
                     void (try (PF.setOwnerAndGroup tmp (PF.fileOwner st) (PF.fileGroup st))
                             :: IO (Either IOException ()))
                     D.renameFile tmp p)
