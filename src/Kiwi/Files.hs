-- | The files of a source that are not pages (images, documents…): which
-- ones kiwi serves, and their uid, written in their name
-- ("pomme__XJD54PM77T.jpg"). A file is linked by its uid ("file:XJD54PM77T"),
-- so that it can be moved or renamed in its source without breaking the
-- links (see 'Kiwi.PagesDB.updatePagesDB', which gives a uid to the files
-- without one).
module Kiwi.Files
  ( servablePath
  , isConflictCopy
  , isFileUid
  , fileUid
  , withFileUid
  , servedName
  , isImagePath
  , uploadName
  ) where

import           Data.Char (isAlphaNum, isControl, toLower)
import           Data.List (stripPrefix, tails)
import           Data.Maybe (listToMaybe)
import qualified Data.Text as T
import qualified System.FilePath as FP


-- | Whether kiwi may give a uid to the file of this path and serve it, the
-- path being given as segments from the top of its source. Not served: an
-- empty path, a path with an empty segment, or a segment that:
--
-- * starts with '.' (hidden files, ".", ".."), '_' (ignored by the loader),
--   '#' or '~', or ends with '#' or '~' (copies and temporary files of the
--   editors, of syncthing on Windows, of Office…);
-- * holds ".md" or ".markdown" (in any case) followed by the end of the name
--   or by a character that is neither a letter nor a digit (pages and their
--   copies: "page.md.orig"…);
-- * holds ".sync-conflict-" (conflict copies of syncthing);
-- * has the extension of a temporary or backup file ("x.part", "x.bak"…,
--   see 'temporaryExtensions'), in any case;
-- * is "Thumbs.db" or "desktop.ini", in any case.
servablePath :: [T.Text] -> Bool
servablePath segments = not (null segments) && all servable segments
  where
    servable s = case T.uncons s of
      Nothing -> False
      Just (c, _) -> c `notElem` ['.', '_', '#', '~'] && T.last s `notElem` ['#', '~'] &&
                     T.all (`notElem` ['/', '\0']) s &&
                     let l = T.toLower s
                     in not (pageName l) && not (isConflictCopy l) &&
                        extension l `notElem` temporaryExtensions &&
                        l `notElem` ["thumbs.db", "desktop.ini"]
    pageName n = any (ends n) [".md", ".markdown"]
    ends n ext = any (\(_, after) -> case T.uncons (T.drop (T.length ext) after) of
                                       Nothing     -> True
                                       Just (c, _) -> not (isAlphaNum c))
                     (T.breakOnAll ext n)
    extension n = case T.breakOnEnd "." n of
      ("", _) -> ""
      (_, e)  -> e

-- | Whether a file name is a conflict copy of syncthing: it holds
-- ".sync-conflict-", in any case ("page.sync-conflict-20260101-120000-ABCDEFG.md").
-- Neither a page nor a served file.
isConflictCopy :: T.Text -> Bool
isConflictCopy n = ".sync-conflict-" `T.isInfixOf` T.toLower n

-- | Extensions (lower case, without the dot) of the files being written or
-- downloaded, and of the backup copies: kiwi does not rename them
temporaryExtensions :: [T.Text]
temporaryExtensions = [ "tmp", "temp", "part", "partial", "crdownload", "download"
                      , "swp", "swo", "bak", "orig", "rej", "lock", "lck"
                      , "kiwi-migration" ]


-- | Whether a text is a file uid: 10 characters of the Crockford base32
-- alphabet, in upper case (see 'Kiwi.Meta.uidFromBytes')
isFileUid :: String -> Bool
isFileUid u = length u == 10 && all (`elem` crockford) u
  where crockford = "0123456789ABCDEFGHJKMNPQRSTVWXYZ" :: String

-- | The name split around its uid, without the "__" before it: the first
-- "__" followed by a uid and then by the end of the name or by a character
-- that is neither a letter nor a digit ("pomme__XJD54PM77T.jpg",
-- "pomme__XJD54PM77T (copie).jpg")
splitUid :: String -> Maybe (String, String, String)
splitUid name = listToMaybe
  [ (take i name, u, after)
  | (i, t) <- zip [0 ..] (tails name)
  , Just r <- [stripPrefix "__" t]
  , let (u, after) = splitAt 10 r
  , isFileUid u
  , case after of
      []    -> True
      c : _ -> not (isAlphaNum c) ]

-- | Uid written in a file name, if any (see 'splitUid')
fileUid :: String -> Maybe String
fileUid name = (\(_, u, _) -> u) <$> splitUid name

-- | The name with this uid: in place of its uid if it has one, else before
-- its first '.' ("archive__XJD54PM77T.tar.gz"), or at its end without '.'
withFileUid :: String -> String -> String
withFileUid u name = case splitUid name of
  Just (before, _, after) -> concat [before, "__", u, after]
  Nothing -> let (before, after) = break (== '.') name
             in concat [before, "__", u, after]

-- | The name of a file without its uid, as given to the browser
servedName :: String -> String
servedName name = maybe name (\(before, _, after) -> before ++ after) (splitUid name)

-- | The name of a file sent by a user (the editor), without the spaces
-- around it, when kiwi can serve a file of this name with a uid (see
-- 'servablePath'): one segment (no '/' nor '\\'), no control character, at
-- most 200 characters. A page name is refused.
uploadName :: T.Text -> Maybe FilePath
uploadName t
  | T.null n || T.length n > 200 || T.any (\c -> isControl c || c == '\\') n = Nothing
  | not (servablePath [n]) || not (servablePath [T.pack $ withFileUid "0000000000" (T.unpack n)]) = Nothing
  | otherwise = Just (T.unpack n)
  where n = T.strip t

-- | Whether a path is the one of an image (by its extension, in any case):
-- its links open in the image viewer of the theme
isImagePath :: FP.FilePath -> Bool
isImagePath p = map toLower (FP.takeExtension p) `elem`
                  [".png", ".jpg", ".jpeg", ".gif", ".webp", ".svg", ".avif"]
