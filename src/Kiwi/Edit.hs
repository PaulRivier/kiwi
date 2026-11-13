-- | Edition of a page file in the browser: the text shown in the editor, the
-- text saved from the one sent back and its checks, and its writing. The
-- users with the write right on a page do not see nor change the metadata of
-- 'adminKeys'; the headers are written normalized (see 'writeHeader').
module Kiwi.Edit
  ( fileHash
  , editorText
  , editedText
  , SaveFailure (..)
  , checkSave
  , writePageFile
  ) where

import           Control.Exception (IOException, bracketOnError, try)
import           Control.Monad (void, when)
import           Data.Bifunctor (first)
import           Data.Maybe (isJust)
import qualified Crypto.Hash as H
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified System.Directory as D
import qualified System.FilePath as FP
import           System.IO (hClose, openTempFile)
import qualified System.Posix.Files as PF

import           Kiwi.Meta
import           Kiwi.Pandoc (textPageUID)
import           Kiwi.Types
import qualified Utils.DocIndex as DI


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

-- | Text of a page file to save, from the text sent by an editor with this
-- right (see 'editedText'), with the keys sent that the editor can not
-- change, and the id of the page: the file (its bytes and text, read in the
-- content folder at this path) must not have changed since its edition
-- (same hash), and the page of the new text must load, with an id that is
-- not the one of another page of the pages.
checkSave :: FilePath -> PagesDB -> PageUID -> AccessLevel -> T.Text -> FilePath ->
             (BS.ByteString, T.Text) -> T.Text ->
             Either SaveFailure (T.Text, [T.Text], PageUID)
checkSave contentDir' db uid level hash path (bytes, fileText) sent = do
  when (fileHash bytes /= hash) $ Left SaveConflict
  (text, ignored) <- invalid $ editedText (customMetaConfig db) level fileText sent
  newUid <- invalid $ textPageUID contentDir' db path text
  when (newUid /= uid && isJust (DI.findDoc (pagesIndex db) newUid)) $
    Left $ SaveInvalid "the id is the one of another page"
  return (text, ignored, newUid)
  where invalid = first SaveInvalid

-- | Keys of the header that an editor with this right can not see nor change
protectedKeys :: AccessLevel -> [T.Text]
protectedKeys level = if level >= AccessAdmin then [] else adminKeys

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
