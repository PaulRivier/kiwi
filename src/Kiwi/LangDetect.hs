-- | Detection of the lang of a page, for the "lang: auto" setting.
-- Each lang has a list of frequent words (articles, pronouns,
-- prepositions...), and the lang whose words are the most frequent in the
-- text wins, if it is clearly ahead.
module Kiwi.LangDetect
  ( resolveLang
  , detectLang  -- exported for tests
  , stopWords   -- exported for tests
  ) where

import           Data.Char (isLetter)
import           Data.List (sortOn)
import qualified Data.Map.Strict as M
import           Data.Maybe (fromMaybe, listToMaybe)
import           Data.Ord (Down (..))
import qualified Data.Text as T
import           Data.Text.Normalize (normalize, NormalizationMode(NFC))

import           Kiwi.Types


-- | Lang of a page, from its setting, its title and the plain text of its
-- content
resolveLang :: LangSetting -> T.Text -> T.Text -> Lang
resolveLang (LangFixed l) _ _ = l
resolveLang (LangAuto l) title content =
  fromMaybe l $ detectLang $ T.unwords [title, content]


-- | The lang with the most frequent words, with at least 'minHits' of them
-- and more than twice as many as the next lang. Nothing if no lang is clearly
-- ahead (short text, mixed langs, unknown lang...).
detectLang :: T.Text -> Maybe Lang
detectLang t = case sortOn (Down . snd) (M.toList scores) of
  (l, s) : rest | s >= minHits && s > 2 * maybe 0 snd (listToMaybe rest) -> Just l
  _ -> Nothing
  where
    scores = M.fromListWith (+) [ (l, 1 :: Int) | w <- textWords t
                                                , Just l <- [M.lookup w wordsLangs] ]

minHits :: Int
minHits = 3

-- | Lower case words, split on anything else than letters, so that the
-- elisions are separated: "l'eau" gives "l" and "eau"
textWords :: T.Text -> [T.Text]
textWords = filter (not . T.null) . T.split (not . isLetter) . normalize NFC . T.toLower

wordsLangs :: M.Map T.Text Lang
wordsLangs = M.fromList [ (w, l) | (l, ws) <- stopWords, w <- ws ]

-- | Frequent words of each lang, lower case and in NFC. The lists must be
-- disjoint: a word shared by several langs ("de", "la", "que", "en"...)
-- does not help to tell them apart.
stopWords :: [(Lang, [T.Text])]
stopWords =
  [ (English, T.words
      "the and of to is in that it for was with as be by this are from or \
      \have an they which you were her his she we not but at had been their \
      \there can would will what when who all if about more one my so them \
      \these than then its also into only other some could our should does \
      \do how any because very just after where why while those such \
      \between each through being your him over before both i")
  , (French, T.words
      "le les des du est et une dans pour pas qui sur au aux avec ce cette \
      \ces il elle ils elles nous vous je mais ou où sont été être avoir ont \
      \fait plus sans sous comme leur leurs mon ma mes ton ta tes sa ses \
      \notre votre vos aussi très peut tout tous toute toutes chez donc \
      \alors encore déjà même quand après avant ainsi cela ça ceci dont \
      \était avait sera ne lui qu l d j c n")
  , (Spanish, T.words
      "el los las del y es por con para una lo su sus al como más pero este \
      \esta estos estas ese esa eso ha han hay fue ser está están estaba era \
      \muy también sin sobre cuando donde porque desde hasta todo todos toda \
      \todas ya yo él ella ellos ellas nosotros usted ustedes mi mis otro \
      \otra otros otras puede hace tiene tienen cada así solo aunque bien \
      \qué cómo cuál nada algo entonces mucho poco")
  ]
