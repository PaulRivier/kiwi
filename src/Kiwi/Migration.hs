-- | Temporary: migration of the pages to uids (see CLAUDE/projects/normalisation.md).
-- When kiwi gives a uid to a page without one, the former id of the page
-- (its "id" field, or the id of its path) goes to its previous-uids
-- ('formerIds'). The "page:" links are then converted to "uid:" links, found
-- by these former ids ('convertLinks'). This module, and its two calls in
-- "Kiwi.PagesDB", are to be removed once the pages are migrated.
module Kiwi.Migration
  ( formerIds
  , convertLinks
  , convertBody  -- exported for tests
  ) where

import           Control.Exception (IOException, try)
import qualified Data.ByteString as BS
import           Data.Char (isSpace)
import qualified Data.Map.Strict as M
import           Data.Maybe (catMaybes)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified System.FilePath as FP

import           Kiwi.Pandoc (pagePath, pageSubDir, pageTarget)
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
-- 'convertBody'), and the targets that are not found, by file. A target is
-- found by the uid or the previous-uids of a page of the same source, the
-- uids first. Only the content of a file changes, not its header.
convertLinks :: FP.FilePath -> PagesDB -> IO [(FP.FilePath, Maybe T.Text, [T.Text])]
convertLinks contentDir' db = catMaybes <$> mapM convert candidates
  where
    pages = DI.documentsList (pagesIndex db)
    candidates = filter (T.isInfixOf "page:" . pageBody) pages
    -- the uids before the previous ones (left-biased union)
    table = M.union (M.fromList [ (pageUID p, snd $ pageUID p) | p <- pages ])
                    (M.fromList [ ((fst $ pageUID p, i), snd $ pageUID p)
                                | p <- pages, i <- pagePreviousUids p ])
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
            let (body', missing) = convertBody (\i -> M.lookup (source, i) table) dir body
                newText = if body' == body then Nothing
                          else Just $ T.concat [header, "\n\n", body']
            if body' == body && null missing then Nothing
              else Just (path, newText, missing)

-- | The content of a page of this folder with its "page:" links converted to
-- "uid:" links, keeping their fragment ("#anchor"), and the targets that are
-- not found (left as they are). The links are found as written in the text:
-- "(page:", "(<page:", "]: page:" (reference definitions) and "]: <page:",
-- with or without spaces before "page:"; the target ends at a space or ")",
-- or at ">" after "<". The target is resolved as in a page ('pageTarget'):
-- relative to the folder, or absolute when it starts with '/'. A link
-- written in a code block is converted too.
convertBody :: (T.Text -> Maybe T.Text) -> T.Text -> T.Text -> (T.Text, [T.Text])
convertBody find dir = go
  where
    go t = case T.breakOn "page:" t of
      (before, "") -> (before, [])
      (before, rest) ->
        let after = T.drop 5 rest
            angle = "<" `T.isSuffixOf` before
            lead = T.stripEnd $ if angle then T.dropEnd 1 before else before
            isLink = "(" `T.isSuffixOf` lead || "]:" `T.isSuffixOf` lead
            (target, rest') = if angle then T.break (== '>') after
                              else T.break (\c -> isSpace c || c == ')') after
            (pId, fragment) = pageTarget dir target
        in if not isLink || T.null target
           then prepend (before <> "page:") [] (go after)
           else case find pId of
             Just u  -> prepend (T.concat [before, "uid:", u, fragment]) [] (go rest')
             Nothing -> prepend (T.concat [before, "page:", target]) [target] (go rest')
    prepend x m (r, ms) = (x <> r, m ++ ms)
