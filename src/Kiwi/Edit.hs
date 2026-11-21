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
  , saveFailureText
  , Change
  , sentText
  , applyEdits
  , editsChange
  , checkSave
  , writePageFile
  , writeNewPageFile
  , protectedKeys
  , titleFileName
  , validPagePath
  ) where

import           Control.Exception (IOException, bracketOnError, try)
import           Control.Monad (void, when)
import           Data.Bits (complement)
import           Data.Char (isAlphaNum, isControl, isSpace)
import           Data.Bifunctor (first)
import qualified Crypto.Hash as H
import qualified Data.ByteString as BS
import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified System.Directory as D
import qualified System.FilePath as FP
import           System.IO (hClose, openTempFile)
import qualified System.Posix.Files as PF

import           Kiwi.Meta
import           Kiwi.Pandoc (checkPageText)
import           Kiwi.Types


-- | Hash of the content of a file (the first 16 hexadecimal digits of its
-- SHA-256: short for the agents, enough to detect that it
-- changed between its edition and its save)
fileHash :: BS.ByteString -> T.Text
fileHash bytes = T.take 16 $ T.pack $ show (H.hash bytes :: H.Digest H.SHA256)

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
                 | SaveNotApplied String  -- ^ the change does not apply to the
                                          -- current text (see 'applyEdits')
                 | SaveInvalid String   -- ^ the text can not be saved
                 | SaveForbidden String -- ^ the user can not write this page
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

-- | Message of a failure
saveFailureText :: SaveFailure -> T.Text
saveFailureText f = T.pack $ case f of
  SaveConflict -> "the page changed since it was read"
  SaveNotApplied e -> e
  SaveInvalid e -> e
  SaveForbidden e -> e
  SaveFailed e -> e

-- | Replacements of exact passages of a text (old, new), in their order:
-- each old passage must be found exactly once in the text, as it is after
-- the previous replacements. The result is the new text, and the lines of
-- each new passage in it (first and last line, from 1; an empty new
-- passage gives the line where the old one was), or why the edits do not
-- apply (an old passage found several times: its lines in the content, the
-- header apart, at most 10).
applyEdits :: [(T.Text, T.Text)] -> T.Text -> Either String (T.Text, [(Int, Int)])
applyEdits edits text0 = go (1 :: Int) edits text0 []
  where
    go _ [] t ranges = Right (t, reverse ranges)
    go n ((old, new) : rest) t ranges
      | T.null old = Left $ "edit " ++ show n ++ ": the old text is empty"
      | otherwise = case T.breakOnAll old t of
          [] -> Left $ "edit " ++ show n ++ ": the old text is not in the page \
                       \(read the page again: it must be copied exactly, spaces included)"
          [(before, after)] ->
            let start = T.count "\n" before + 1
                oldLines = T.count "\n" old
                newLines = T.count "\n" new
                delta = newLines - oldLines
                -- the ranges after this edit move with its lines
                shift (a, b) | a > start = (a + delta, b + delta)
                             | otherwise = (a, b)
                t' = before <> new <> T.drop (T.length old) after
                -- the lines the new text is on
                end = start + max 1 (length $ T.lines new) - 1
            in go (n + 1) rest t' ((start, end) : map shift ranges)
          found ->
            let -- the lines of the content, after the header and its blank
                -- line (none without a blank line), as everywhere in the API
                off = maybe 0 (+ 1) (L.findIndex T.null (T.lines t))
                lineOf (b, _) = let l = T.count "\n" b + 1
                                in if l <= off then "header" else show (l - off)
                shown = L.nub (map lineOf found)
                more = length shown - maxLines
            in Left $ concat
                 [ "edit ", show n, ": the old text is in the page ", show (length found)
                 , " times (lines ", L.intercalate ", " (take maxLines shown)
                 , if more > 0 then ", and " ++ show more ++ " more" else ""
                 , "), give more of the text around it" ]
    -- lines given when an old text is found several times
    maxLines = 10

-- | The edits (see 'applyEdits') of the text of a page as shown to an
-- editor with this right (see 'editorText'), saved as the text sent by this
-- editor (see 'editedText')
editsChange :: AccessLevel -> [(T.Text, T.Text)] -> Change
editsChange level edits cmc (_, fileText) = do
  (shown, _) <- first SaveInvalid $ editorText cmc level fileText
  (text, _) <- first SaveNotApplied $ applyEdits edits shown
  first SaveInvalid $ editedText cmc level fileText text

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

-- | Writes the text of a new page file, which must not exist, atomically (see
-- 'writePageFile'), with the mode given by the umask
writeNewPageFile :: FP.FilePath -> T.Text -> IO ()
writeNewPageFile path text = do
  let dir = FP.takeDirectory path
  bracketOnError (openTempFile dir ("." ++ FP.takeFileName path))
                 (\(tmp, h) -> hClose h >> void (try (D.removeFile tmp) :: IO (Either IOException ())))
                 (\(tmp, h) -> do
                     BS.hPut h (TE.encodeUtf8 text)
                     hClose h
                     -- openTempFile makes a 0600 file: the mode of a new file instead
                     mask <- PF.setFileCreationMask 0
                     _ <- PF.setFileCreationMask mask
                     PF.setFileMode tmp (0o666 `PF.intersectFileModes` complement mask)
                     D.renameFile tmp path)

-- | Name of the file of a new page (without extension) made from its title:
-- without the characters that are forbidden or troublesome in a file name
-- ('/', '\\', ':', control characters), starting with a letter or a digit
-- (see 'Kiwi.Utils.isValidMdFilename'), without '#' and '~' at its end, at
-- most 100 characters; "page" when nothing is left
titleFileName :: T.Text -> FP.FilePath
titleFileName title =
  let kept = T.filter (\c -> c `notElem` ("/\\:" :: String) && not (isControl c)) title
      name = T.dropWhileEnd (\c -> isSpace c || c `elem` ("#~." :: String)) $ T.take 100 $
             T.dropWhile (not . isAlphaNum) kept
  in if T.null name then "page" else T.unpack name

-- | Whether a path of a new page, from the top of its source and without
-- extension, can be used: relative, without "..", no part starting with "."
-- or "_" (they are not loaded), a name starting with a letter or a digit
validPagePath :: FP.FilePath -> Bool
validPagePath p =
  let parts = FP.splitDirectories p
  in not (null parts) && FP.isRelative p &&
     all (\s -> s `notElem` [".", ".."] && not (null s) && head' s `notElem` ['.', '_', '/']) parts &&
     maybe False isAlphaNum (headMaybe (last parts))
  where
    head' s = maybe ' ' id (headMaybe s)
    headMaybe s = case s of
      c : _ -> Just c
      []    -> Nothing
