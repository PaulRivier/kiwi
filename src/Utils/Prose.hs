{-# LANGUAGE TemplateHaskell #-}
-- | Prose written over several lines in the code, without quotes nor
-- string gaps: [prose| … |] (QuasiQuotes).
module Utils.Prose
  ( prose
  , foldProse  -- exported for tests
  ) where

import           Data.Char (isSpace)
import           Data.List (dropWhileEnd, intercalate)
import           Data.String (fromString)
import           Language.Haskell.TH.Quote (QuasiQuoter (..))
import           Language.Haskell.TH.Syntax (lift)


-- | A string literal (any 'Data.String.IsString' type) from prose: the
-- lines of a paragraph joined by a space, the paragraphs (separated by
-- blank lines) by a blank line; the spaces around each line are left out.
-- @|]@ ends the text: it can not be written in it.
prose :: QuasiQuoter
prose = QuasiQuoter
  { quoteExp = \s -> [| fromString $(lift (foldProse s)) |]
  , quotePat = refused "pattern"
  , quoteType = refused "type"
  , quoteDec = refused "declaration" }
  where refused what _ = fail $ "prose: not a " ++ what

-- | The text of 'prose'
foldProse :: String -> String
foldProse = intercalate "\n\n" . map unwords . paragraphs . map strip . lines
  where
    strip = dropWhileEnd isSpace . dropWhile isSpace
    paragraphs ls = case dropWhile null ls of
      [] -> []
      ls' -> let (p, rest) = break null ls' in p : paragraphs rest
