-- | Annotations of the pages: a paragraph whose first line starts with "!!"
-- is a request left in the page (for an agent, or anyone), pending; once
-- done, its mark becomes "!-" and its text stays, until someone removes it.
-- Kiwi finds the pending ones at load time (their lines, see 'Page'), and
-- shows both kinds in a frame (see "Kiwi.HtmlWriter").
module Kiwi.Annotation
  ( annotationLines
  , annotationInlines
  ) where

import qualified Data.Text as T
import qualified Text.Pandoc.Definition as PD


-- | Lines (from 1) of the content of a page that start a pending
-- annotation: "!!" at the start of a paragraph (after a blank line, a
-- heading or the start of the content, indented by 3 spaces at most),
-- outside of fenced code blocks
annotationLines :: T.Text -> [Int]
annotationLines = go Nothing True . zip [1 ..] . T.lines
  where
    go _ _ [] = []
    go fence prevBreak ((n, l) : rest) =
      let (indent, t) = T.span (== ' ') l
          small = T.length indent <= 3
      in case fence of
           Just f | small && closes f t -> go Nothing False rest
                  | otherwise -> go fence False rest
           Nothing
             | small, Just f <- opens t -> go (Just f) False rest
             | T.all (== ' ') l -> go Nothing True rest
             | small && prevBreak && "!!" `T.isPrefixOf` t -> n : go Nothing False rest
             | otherwise -> go Nothing (small && "#" `T.isPrefixOf` t && atx t) rest
    -- a fence: its character and length
    opens t = case T.uncons t of
      Just (c, _) | c `elem` ['`', '~'] ->
        let run = T.length (T.takeWhile (== c) t)
        in if run >= 3 then Just (c, run) else Nothing
      _ -> Nothing
    closes (c, run) t = let r = T.takeWhile (== c) t
                        in T.length r >= run && T.all (== ' ') (T.drop (T.length r) t)
    -- an ATX heading: 1 to 6 "#", then a space or the end of the line
    atx t = let h = T.takeWhile (== '#') t
                after = T.drop (T.length h) t
            in T.length h <= 6 && (T.null after || T.head after == ' ')

-- | The kind of a paragraph that is an annotation (True: pending, "!!";
-- False: done, "!-"), and its inlines without the mark
annotationInlines :: [PD.Inline] -> Maybe (Bool, [PD.Inline])
annotationInlines ils = case dropText "!!" ils of
  Just rest -> Just (True, rest)
  Nothing -> (,) False <$> dropText "!-" ils
  where
    dropText pre is
      | T.null pre = Just (dropWhile (== PD.Space) is)
    dropText pre (PD.Str t : rest)
      | pre `T.isPrefixOf` t = let t' = T.drop (T.length pre) t
                               in Just $ dropWhile (== PD.Space) $
                                    [ PD.Str t' | not (T.null t') ] ++ rest
      | not (T.null t) && t `T.isPrefixOf` pre = dropText (T.drop (T.length t) pre) rest
    dropText _ _ = Nothing
