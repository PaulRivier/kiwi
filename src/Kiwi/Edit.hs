-- | Edition of a page file: the text shown in an editor, the text saved
-- from the one sent back and its checks, and its writing. The browser editor
-- changes the file whole (header and body); the API changes the header and
-- the body apart, each one keeping the other as written. The users with the
-- write right on a page do not see nor change the metadata of 'adminKeys',
-- and no one sees nor changes those of 'kiwiKeys' (uid); the headers are
-- written normalized (see 'writeHeader').
module Kiwi.Edit
  ( fileHash
  , textHash
  , splitFile
  , joinFile
  , editorText
  , editedText
  , editorMeta
  , metaText
  , SaveFailure (..)
  , saveFailureText
  , Change
  , sentText
  , metaChange
  , bodyText
  , editsChange
  , applyEdits        -- exported for tests
  , LineEdit (..)
  , LineResult (..)
  , applyLineEdits    -- exported for tests
  , linesChange
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
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import           Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified System.Directory as D
import qualified System.FilePath as FP
import           System.IO (hClose, openTempFile)
import qualified System.Posix.Files as PF

import           Kiwi.Meta
import           Kiwi.Pandoc (checkPageText)
import           Kiwi.Types
import           Kiwi.Utils (splitOnFirst)


-- | Hash of the content of a file (the first 16 hexadecimal digits of its
-- SHA-256: short for the agents, enough to detect that it
-- changed between its edition and its save)
fileHash :: BS.ByteString -> T.Text
fileHash bytes = T.take 16 $ T.pack $ show (H.hash bytes :: H.Digest H.SHA256)

-- | Hash of a text (see 'fileHash'): the version of the body or of the
-- header of a page, for the API
textHash :: T.Text -> T.Text
textHash = fileHash . TE.encodeUtf8

-- | The header and the body of the text of a page file, as written: the
-- header is the text before its first blank line, the body the text after
-- it (see 'splitPage'); a text without blank line is a header alone, with an
-- empty body
splitFile :: T.Text -> (T.Text, T.Text)
splitFile t = fromMaybe (T.dropWhileEnd (== '\n') t, T.empty) (splitOnFirst "\n\n" t)

-- | The text of a page file of this header and body (see 'splitFile')
joinFile :: T.Text -> T.Text -> T.Text
joinFile header body = T.concat [header, "\n\n", body]

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
  let (header, ignored) = editedHeader level original edited
  return (writePage cmc header content, ignored)

-- | The header saved by an editor with this right, from the header of the
-- file and the header sent, and the keys sent that the editor can not
-- change (ignored): their values are those of the file
editedHeader :: AccessLevel -> MetaData -> MetaData -> (MetaData, [T.Text])
editedHeader level original edited =
  let protected = protectedKeys level
  in ( mergeMeta (keepOnlyMeta protected original) (clearMeta protected edited)
     , filter (`elem` protected) (metaKeys edited) )

-- | The lines of the header of a page file shown to an editor with this
-- right (without the keys of 'protectedKeys'), normalized
editorMeta :: [CustomMetaConfig] -> AccessLevel -> T.Text -> Either String T.Text
editorMeta cmc level fileText = writeHeader cmc . restrict level . fst <$> splitPage cmc fileText

-- | The lines of a header sent apart from the body, without the blank
-- lines at its start and end; fails on a blank line inside it, which would
-- end the header
metaText :: T.Text -> Either String T.Text
metaText sent =
  let ls = L.dropWhileEnd blank $ dropWhile blank $ T.lines sent
      blank = T.all isSpace
  in if any blank ls then Left "the metadata can not have a blank line (the body is written apart)"
     else Right (T.intercalate "\n" ls)

-- | Why a page could not be saved
data SaveFailure = SaveConflict         -- ^ the file changed since its edition
                 | SaveNotApplied String  -- ^ the change does not apply to the
                                          -- current text (see 'applyEdits')
                 | SaveInvalid String   -- ^ the text can not be saved
                 | SaveForbidden String -- ^ the user can not write this page
                 | SaveFailed String    -- ^ error of the server
  deriving (Eq, Show)

-- | A change of a page file, from the custom metadata of the config and the
-- file (its bytes and its text): the text to write, with what the change
-- tells (the keys that are ignored, see 'editedText'; the lines of the new
-- passages, see 'applyEdits'), or why it can not be made
type Change a = [CustomMetaConfig] -> (BS.ByteString, T.Text) ->
                Either SaveFailure (T.Text, a)

-- | The text sent by an editor with this right (see 'editedText'), for the
-- file read with this hash: the file must not have changed since
sentText :: AccessLevel -> T.Text -> T.Text -> Change [T.Text]
sentText level hash sent cmc (bytes, fileText) = do
  when (fileHash bytes /= hash) $ Left SaveConflict
  first SaveInvalid $ editedText cmc level fileText sent

-- | The header sent by an editor with this right (see 'metaText' and
-- 'editedHeader'), for the header read with this hash (see 'textHash'): the
-- header must not have changed since. The body is kept as written.
metaChange :: AccessLevel -> T.Text -> T.Text -> Change [T.Text]
metaChange level hash sent cmc (_, fileText) = do
  let (rawHeader, body) = splitFile fileText
  when (textHash rawHeader /= hash) $ Left SaveConflict
  first SaveInvalid $ do
    lines' <- metaText sent
    edited <- parseHeader cmc lines'
    (original, _) <- splitPage cmc fileText
    let (header, ignored) = editedHeader level original edited
    return (joinFile (T.dropWhileEnd (== '\n') $ writeHeader cmc header) body, ignored)

-- | A change of the body of a page file: the header is kept as written
bodyChange :: (T.Text -> Either SaveFailure (T.Text, a)) -> Change a
bodyChange change _ (_, fileText) = do
  let (header, body) = splitFile fileText
  (body', r) <- change body
  return (joinFile header body', r)

-- | The body sent, for the body read with this hash (see 'textHash'): the
-- body must not have changed since. It ends with a line end.
bodyText :: T.Text -> T.Text -> Change ()
bodyText hash sent = bodyChange $ \body -> do
  when (textHash body /= hash) $ Left SaveConflict
  return (if T.null sent || "\n" `T.isSuffixOf` sent then sent else sent <> "\n", ())

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
-- apply (an old passage found several times: its lines, at most 10).
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
            let lineOf (b, _) = show (T.count "\n" b + 1)
                shown = L.nub (map lineOf found)
                more = length shown - maxLines
            in Left $ concat
                 [ "edit ", show n, ": the old text is in the page ", show (length found)
                 , " times (lines ", L.intercalate ", " (take maxLines shown)
                 , if more > 0 then ", and " ++ show more ++ " more" else ""
                 , "), give more of the text around it" ]
    -- lines given when an old text is found several times
    maxLines = 10

-- | The edits (see 'applyEdits') of the body of a page, with the lines of
-- the new passages in it; the header is kept as written
editsChange :: [(T.Text, T.Text)] -> Change [(Int, Int)]
editsChange edits = bodyChange (first SaveNotApplied . applyEdits edits)

-- | An edit of the lines of a body, by the numbers of its lines in the
-- version read (from 1), whatever the other edits
data LineEdit = ReplaceLines Int Int T.Text  -- ^ lines from-to replaced by the text (empty: deleted)
              | InsertAfter Int T.Text       -- ^ text inserted after a line (0: at the start)
              | MoveAfter Int Int Int        -- ^ lines from-to moved after a line
  deriving (Eq, Show)

-- | What an edit of lines did: the lines of its new passage in the new
-- version (first and last; for a deletion, the last is the first minus
-- one: the passage is empty, between these two lines), and its lines in
-- the version read (for an insertion: the line it follows, twice)
data LineResult = LineResult { lineNew :: (Int, Int), lineBefore :: (Int, Int) }
  deriving (Eq, Show)

-- | Edits of the lines of a text (see 'LineEdit'), all of them or none.
-- Each edit is a deletion of lines and an insertion after a line, all of
-- them in the version read: replacing deletes the lines and inserts the
-- text after the line before them, moving deletes the lines and inserts
-- them after the line given. The new version is written in one pass over
-- the lines of the version read: at each place, the insertions made after
-- it, in the order of the list, then the next line if it is not deleted.
-- Fails (with the number of the edit) on a line out of the text, deleted
-- lines of two edits that overlap, an insertion between two deleted lines
-- of an edit, a move that does not move its lines, an empty insertion. The
-- new text ends with a line end.
applyLineEdits :: [LineEdit] -> T.Text -> Either String (T.Text, [LineResult])
applyLineEdits edits text = do
  mapM_ check numbered
  let deletions = [ (k, a, b) | (k, e) <- numbered, Just (a, b) <- [deleted e] ]
  sequence_ [ Left $ concat [ "edit ", show j, ": its lines ", range a' b', " overlap those of edit "
                            , show k, " (", range a b, ")" ]
            | (k, a, b) : rest <- L.tails deletions, (j, a', b') <- rest, a' <= b, a <= b' ]
  sequence_ [ Left $ concat [ "edit ", show k, ": it inserts after line ", show p
                            , ", between the lines ", range a b, " of edit ", show j ]
            | (k, p, _) <- insertions, (j, a, b) <- deletions, j /= k, a <= p, p < b ]
  let gone = S.fromList (concat [ [a .. b] | (_, a, b) <- deletions ])
      at = M.fromListWith (flip (++)) [ (p, [(k, ls)]) | (k, p, ls) <- insertions ]
      -- at each place (from 0), with the lines of the version read after
      -- it: the lines written so far (reversed), their number, and the new
      -- lines of each edit
      step (out, count, sp) (p, next) =
        let (out', count', sp') =
              L.foldl' (\(o, c, m) (k, ls) -> ( reverse ls ++ o, c + length ls
                                              , M.insert k (c + 1, c + length ls) m ))
                       (out, count, sp) (M.findWithDefault [] p at)
        in case next of
             Just l | not (S.member (p + 1) gone) -> (l : out', count' + 1, sp')
             _ -> (out', count', sp')
      (written, _, spans) = L.foldl' step ([], 0 :: Int, M.empty)
                                     (zip [0 ..] (map Just old ++ [Nothing]))
      results = [ LineResult (M.findWithDefault (0, -1) k spans) (before e) | (k, e) <- numbered ]
  return (T.unlines (reverse written), results)
  where
    old = T.lines text
    total = length old
    numbered = zip [1 :: Int ..] edits
    range a b = show a ++ "-" ++ show b
    deleted e = case e of
      ReplaceLines a b _ -> Just (a, b)
      MoveAfter a b _ -> Just (a, b)
      InsertAfter _ _ -> Nothing
    before e = case e of
      ReplaceLines a b _ -> (a, b)
      MoveAfter a b _ -> (a, b)
      InsertAfter p _ -> (p, p)
    insertions = [ (k, p, ls) | (k, e) <- numbered, let (p, ls) = inserted e ]
    inserted e = case e of
      ReplaceLines a _ t -> (a - 1, T.lines t)
      InsertAfter p t -> (p, T.lines t)
      MoveAfter a b p -> (p, take (b - a + 1) (drop (a - 1) old))
    line l = 1 <= l && l <= total
    place p = 0 <= p && p <= total
    check (k, e) = let failWith m = Left ("edit " ++ show k ++ ": " ++ m) in case e of
      ReplaceLines a b _
        | not (line a && line b && a <= b) -> failWith $ linesOut a b
        | otherwise -> Right ()
      InsertAfter p t
        | not (place p) -> failWith $ placeOut p
        | T.null t -> failWith "the new text is empty"
        | otherwise -> Right ()
      MoveAfter a b p
        | not (line a && line b && a <= b) -> failWith $ linesOut a b
        | not (place p) -> failWith $ placeOut p
        | a - 1 <= p && p <= b -> failWith $ "the lines " ++ range a b ++ " would not move"
        | otherwise -> Right ()
    linesOut a b = concat [ "the lines ", range a b, " are not lines of the body (from 1 to "
                          , show total, ", the first one before the last one)" ]
    placeOut p = concat [ "no line ", show p, " (from 0, the start of the body, to ", show total, ")" ]

-- | The edits of lines (see 'applyLineEdits') of the body of a page, for
-- the body read with this hash (see 'textHash'): the body must not have
-- changed since. The header is kept as written.
linesChange :: T.Text -> [LineEdit] -> Change [LineResult]
linesChange hash edits = bodyChange $ \body -> do
  when (textHash body /= hash) $ Left SaveConflict
  first SaveInvalid $ applyLineEdits edits body

-- | Text of a page file to save, by this change of the file (its bytes and
-- text, read in the content folder at this path), with what the change
-- tells: the page of the new text must load. The uid of the page can not change.
checkSave :: FilePath -> PagesDB -> Change a -> FilePath -> (BS.ByteString, T.Text) ->
             Either SaveFailure (T.Text, a)
checkSave contentDir' db change path file = do
  (text, r) <- change (customMetaConfig db) file
  first SaveInvalid $ checkPageText contentDir' db path text
  return (text, r)

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
