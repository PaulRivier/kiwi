-- | Temporary: migration of the pages to uids (see CLAUDE/projects/normalisation.md).
-- When kiwi gives a uid to a page without one, the former id of the page
-- (its "id" field, or the id of its path) goes to its previous-uids
-- ('formerIds'). The "page:" links are then converted to "uid:" links, found
-- by these former ids ('convertLinks'). This module, and its two calls in
-- "Kiwi.PagesDB", are to be removed once the pages are migrated.
--
-- Temporary too: migration of the files to uids (see
-- CLAUDE/projects/file-move-helper.md). The "file:" links written with the
-- path of the file are converted to links with its uid ("file:XJD54PM77T",
-- see "Kiwi.Files"), found by the path of the file, or by its path before
-- kiwi renamed it to give it its uid ('convertLinks').
module Kiwi.Migration
  ( formerIds
  , convertLinks
  , convertBody      -- exported for tests
  , convertFileBody  -- exported for tests
  , fileTarget       -- exported for tests
  ) where

import           Control.Exception (IOException, try)
import qualified Data.ByteString as BS
import           Data.Char (isSpace)
import qualified Data.Map.Strict as M
import           Data.Maybe (catMaybes)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import           Data.Text.Normalize (normalize, NormalizationMode(NFC))
import qualified System.FilePath as FP

import           Kiwi.Files (isFileUid, servablePath)
import           Kiwi.Pandoc (pagePath, pageSubDir, pageTarget)
import qualified Kiwi.Url as Url
import           Kiwi.Types
import           Kiwi.Utils (pathToPageId, splitOnFirst)
import qualified Utils.DocIndex as DI


-- | The former id of a page without uid, from the header of its file and its
-- path in the pages folder, without extension: its "id" field if not empty,
-- else the id of its path ("a/b" gives "a:b"). The header is returned
-- without its "id" field.
formerIds :: FP.FilePath -> MetaData -> ([T.Text], MetaData)
formerIds relPath header =
  let other = metaOther header
      ids = case T.strip . T.concat <$> M.lookup "id" other of
              Just i | not (T.null i) -> [i]
              _ -> [pathToPageId relPath]
  in (ids, header { metaOther = M.delete "id" other })

-- | The new texts of the page files whose "page:" links are converted (see
-- 'convertBody'), or whose "file:" links are (see 'convertFileBody'), and
-- the targets that are not found ("page:x", "file:x"), by file. A page
-- target is found by the uid or the previous-uids of a page of the same
-- source, the uids first. A file target is found by the path of a file of
-- the source (see 'sourceFiles'), or by one of the given paths, from the
-- top of the source, of the files renamed by kiwi before it renamed them.
-- Only the content of a file changes, not its header.
convertLinks :: FP.FilePath -> M.Map (T.Text, T.Text) T.Text -> PagesDB ->
                IO [(FP.FilePath, Maybe T.Text, [T.Text])]
convertLinks contentDir' renamed db = catMaybes <$> mapM convert candidates
  where
    pages = DI.documentsList (pagesIndex db)
    candidates = filter (\p -> T.isInfixOf "page:" (pageBody p) ||
                               any (not . isFileUid . T.unpack) (pageFiles p)) pages
    -- the uids before the previous ones (left-biased union)
    table = M.union (M.fromList [ (pageUID p, snd $ pageUID p) | p <- pages ])
                    (M.fromList [ ((fst $ pageUID p, i), snd $ pageUID p)
                                | p <- pages, i <- pagePreviousUids p ])
    -- the files by path, in NFC (as 'fileTarget' gives them)
    files = M.union (M.fromList [ ((s, normalize NFC $ T.pack $ FP.joinPath $
                                       FP.splitDirectories path), u)
                                | ((s, u), path) <- M.toList (sourceFiles db) ])
                    renamed
    convert p = do
      let path = pageAbsoluteFSPath p
          source = fst $ pageUID p
          dir = T.pack $ pageSubDir $ snd $ pagePath contentDir' path
      bytesE <- try $ BS.readFile path
      return $ case either (\(_ :: IOException) -> Nothing) Just bytesE of
        Nothing -> Nothing
        Just bytes -> case TE.decodeUtf8' bytes of
          Left _ -> Nothing
          Right text -> do
            (header, body) <- splitOnFirst "\n\n" text
            let (body1, missingPages) = convertBody (\i -> M.lookup (source, i) table) dir body
                (body', missingFiles) =
                  convertFileBody (\f -> M.lookup (source, f) files) dir body1
                missing = map ("page:" <>) missingPages ++ map ("file:" <>) missingFiles
                newText = if body' == body then Nothing
                          else Just $ T.concat [header, "\n\n", body']
            if body' == body && null missing then Nothing
              else Just (path, newText, missing)

-- | The content of a page of this folder with its "page:" links converted to
-- "uid:" links, keeping their fragment ("#anchor"), and the targets that are
-- not found (left as they are). The target is resolved as in a page
-- ('pageTarget'): relative to the folder, or absolute when it starts with
-- '/'. See 'convertTargets'.
convertBody :: (T.Text -> Maybe T.Text) -> T.Text -> T.Text -> (T.Text, [T.Text])
convertBody find dir = convertTargets "page:" $ \target ->
  let (pId, fragment) = pageTarget dir target
  in (\u -> T.concat ["uid:", u, fragment]) <$> find pId

-- | The content of a page of this folder with its "file:" links written with
-- a path converted to links with the uid of the file, found by its path from
-- the top of the source (see 'fileTarget'), keeping their fragment
-- ("#page=2"), and the targets that are not found (left as they are). The
-- links with a uid are kept. See 'convertTargets'.
convertFileBody :: (T.Text -> Maybe T.Text) -> T.Text -> T.Text -> (T.Text, [T.Text])
convertFileBody find dir = convertTargets "file:" $ \target ->
  let (u, _) = T.breakOn "#" target
  in if isFileUid $ T.unpack $ T.strip $ Url.urlDecodeText u then Just ("file:" <> target)
     else do
       (segments, fragment) <- fileTarget dir target
       f <- find (T.intercalate "/" segments)
       return $ T.concat ["file:", f, fragment]

-- | The content of a page with its links of this prefix ("page:") replaced
-- by the given function (with the prefix: "uid:XYZ#a"), and the targets
-- that it does not find (left as they are). The links are found as written
-- in the text: "(page:", "(<page:", "]: page:" (reference definitions) and
-- "]: <page:", with or without spaces before "page:"; the target ends at a
-- space or ")", or at ">" after "<". A link written in a code block is
-- converted too.
convertTargets :: T.Text -> (T.Text -> Maybe T.Text) -> T.Text -> (T.Text, [T.Text])
convertTargets prefix replace = go
  where
    go t = case T.breakOn prefix t of
      (before, "") -> (before, [])
      (before, rest) ->
        let after = T.drop (T.length prefix) rest
            angle = "<" `T.isSuffixOf` before
            lead = T.stripEnd $ if angle then T.dropEnd 1 before else before
            isLink = "(" `T.isSuffixOf` lead || "]:" `T.isSuffixOf` lead
            (target, rest') = if angle then T.break (== '>') after
                              else T.break (\c -> isSpace c || c == ')') after
        in if not isLink || T.null target
           then prepend (before <> prefix) [] (go after)
           else case replace target of
             Just new -> prepend (before <> new) [] (go rest')
             Nothing  -> prepend (T.concat [before, prefix, target]) [target] (go rest')
    prepend x m (r, ms) = (x <> r, m ++ ms)

-- | Path of the file targeted by a "file:" link written with a path in a
-- page of the given folder, as segments from the top of the source, and the
-- fragment of the link ("#page=2"), if any. The target may be
-- percent-encoded; it is relative to the folder, or absolute ("/a/b.png")
-- from the top of the source. Its "." and empty segments are ignored.
-- 'Nothing' when the target is empty or a folder (it ends with '/' or '.'),
-- or when the file can not be served (see 'servablePath'), in particular
-- when the target has a ".." segment: a link never goes up. The path is in
-- NFC.
fileTarget :: T.Text -> T.Text -> Maybe ([T.Text], T.Text)
fileTarget pageDir l =
  let (path, fragment) = T.breakOn "#" l
      decoded = Url.urlDecodeText path
      full = case T.uncons decoded of
        Just ('/', p) -> p
        _             -> T.concat [pageDir, "/", decoded]
      segments = filter (`notElem` ["", "."]) $ T.splitOn "/" $ normalize NFC full
      folder = T.null decoded || "/" `T.isSuffixOf` decoded || decoded == "." ||
               "/." `T.isSuffixOf` decoded
  in if not folder && servablePath segments then Just (segments, fragment) else Nothing
