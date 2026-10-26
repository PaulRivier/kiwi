{-# LANGUAGE NamedFieldPuns #-}

module Kiwi.Search (
    initPageSearchEngine
  , queryTerms
  , normalizeText
  ) where

-- import           Data.Text.ICU.Char
-- import           Data.Text.ICU.Normalize2
import Data.Char (generalCategory, GeneralCategory(NonSpacingMark))
import Data.Text.Normalize (normalize, NormalizationMode(NFC, NFD))

import           Data.SearchEngine
import qualified Data.Set as S
import qualified Data.Text as T
import           Text.Stemmer.Snowball as SB

import           Kiwi.Types as K


initPageSearchEngine :: PageSearchEngine
initPageSearchEngine =
    initSearchEngine pageSearchConfig defaultSearchRankParameters

-- | The query terms are normalised beforehand by 'queryTerms', in the lang
-- chosen for each query
pageSearchConfig :: SearchConfig (Page, T.Text) PageUID PageField NoFeatures
pageSearchConfig =
    SearchConfig {
      documentKey           = pageUID . fst,
      extractDocumentTerms  = extractTokens,
      transformQueryTerm    = const,
      documentFeatureValue  = const noFeatures
  }
  where
    -- the page, and the plain text of its content
    extractTokens :: (Page, T.Text) -> PageField -> [T.Text]
    extractTokens (p, _) TitleField = extractTerms (alg $ pageLang p) $
                                      pageTitle p
    extractTokens (p, _) TagsField  = concat $
                                      map (extractTagTokens (alg $ pageLang p)) $
                                      pageTags p
    extractTokens (p, content) ContentField = extractTerms (alg $ pageLang p) content

    -- dans le cas des tags, on indexe le tag sous sa forme canonique
    -- [tag] ainsi que les termes qui le composent
    extractTagTokens lg' t = (T.concat ["[", T.toLower t, "]"]) : extractTerms lg' t


-- | Terms of a query, stemmed in the given lang, unless the query starts with
-- the code of another lang ("fr:", "es: "...)
queryTerms :: Lang -> T.Text -> [T.Text]
queryTerms lg q = case T.breakOn ":" (T.stripStart q) of
  (code, rest) | Just lg' <- lookup (T.toLower code) langCodes, not (T.null rest)
                 -> normaliseQuery lg' (T.drop 1 rest)
  _ -> normaliseQuery lg q
  where
    -- same steps as extractTerms: lower case and NFC, stem, remove accents
    normaliseQuery l = map (normalizeText . stem (alg l) . normalize NFC . T.toLower) .
                       T.words

-- | ISO 639-1 codes of the langs
langCodes :: [(T.Text, Lang)]
langCodes = [ ("da", K.Danish), ("nl", K.Dutch), ("en", K.English)
            , ("fi", K.Finnish), ("fr", K.French), ("de", K.German)
            , ("hu", K.Hungarian), ("it", K.Italian), ("no", K.Norwegian)
            , ("pt", K.Portuguese), ("ro", K.Romanian), ("ru", K.Russian)
            , ("es", K.Spanish), ("sv", K.Swedish), ("tr", K.Turkish) ]

-- pageContent :: P.Pandoc -> T.Text
-- pageContent doc = case (P.runPure $ P.writePlain P.def doc) of
--                     Right r ->  r
--                     Left _ -> T.empty

-- | Words are stemmed with their accents (in NFC, as Snowball expects), and the
-- accents are removed from the stems
extractTerms :: Algorithm -> T.Text -> [T.Text]
extractTerms lg = map normalizeText .
                  stems lg .
                  map (normalize NFC) .
                  tokenizeText breakSet

tokenizeText :: S.Set Char -> T.Text -> [T.Text]
tokenizeText brk = filter (not . T.null) .
                     T.split isBreak .
                     T.toLower
  where
    isBreak c = S.member c brk



-- | Text without its accents (also used to sort texts, with T.toLower)
normalizeText :: T.Text -> T.Text
normalizeText = T.filter ((/= NonSpacingMark) . generalCategory) .
                normalize NFD

-- normalizeText :: T.Text -> T.Text
-- normalizeText = T.filter (not . property Diacritic) .
--                 normalize NFD

breakSet :: S.Set Char
breakSet = S.fromList " \n&~\"#'{([-|`\\_^@)]=}+%*<>,?;.:/!§$€«» ’‘"



-- | Ranking parameters of the search engine (undocumented in full-text-search,
-- this is what its code does).
--
-- Pages are ranked with BM25F. The score of a page for a query is:
--
-- > score  = sum over terms t of  IDF(t) * tf' / (k1 + tf')
-- >        + sum over features f of  weight(f) * V_f(value(f))
-- > tf'    = sum over fields of  fieldWeight * tf_field / B_field
-- > B_field = (1 - b) + b * fieldLength / averageFieldLength
-- > IDF(t) = log ((N - n_t + 0.5) / (n_t + 0.5))
--
-- with N the number of pages, and n_t the number of pages containing t. IDF
-- is negative for a term in more than half of the pages: containing it then
-- lowers the score. Average lengths and n_t are computed again when pages
-- are inserted or removed, so scores change with the other pages.
defaultSearchRankParameters :: SearchRankParameters PageField NoFeatures
defaultSearchRankParameters =
    SearchRankParameters {
      paramK1,
      paramB,
      paramFieldWeights,
      -- features are scores that do not depend on the query terms
      -- (popularity, date...), with their value from 'documentFeatureValue',
      -- a weight and a function: log (l + v), v / (l + v), or
      -- 1 / (l + exp (-l' * v))
      paramFeatureWeights     = noFeatures,
      paramFeatureFunctions   = noFeatures,
      -- 'query' ranks the union of the pages containing each term. Pages sets
      -- are added from the smallest (rarest term), the first one is always
      -- kept whole. It stops before a set that would take the total over the
      -- hard limit, and after a set that takes it over the soft limit: the
      -- most frequent terms are then silently ignored.
      paramResultsetSoftLimit = 12,
      paramResultsetHardLimit = 24,
      -- only for 'queryAutosuggest' (unused): over these numbers of pages,
      -- before or after its results filter, it returns nothing
      paramAutosuggestPrefilterLimit  = 100,
      paramAutosuggestPostfilterLimit = 100
    }
  where
    -- saturation of the term frequency: tf' / (k1 + tf') tends to 1, a term
    -- repeated many times does not count much more than once. Usually 1.2 to
    -- 2, the smaller the faster it saturates.
    paramK1 :: Float
    paramK1 = 1.5

    -- normalisation of the term frequency by the field length, from 0 (length
    -- ignored) to 1 (a field twice as long as average counts half). Usually
    -- 0.75 for text, lower for short fields.
    paramB :: PageField -> Float
    paramB TitleField       = 0.3
    paramB TagsField        = 0.2
    paramB ContentField     = 0.7

    -- weight of the fields. It multiplies the term frequency before the
    -- saturation, not the score: with k1 = 1.5, a term in the title gives
    -- 10 / 11.5 = 0.87, and once in the content 1 / 2.5 = 0.4, so the title
    -- only counts about twice as much.
    paramFieldWeights :: PageField -> Float
    paramFieldWeights TitleField   = 10
    paramFieldWeights TagsField    = 5
    paramFieldWeights ContentField = 1



alg :: Lang -> Algorithm
alg l = case l of
  K.Danish -> SB.Danish
  K.Dutch -> SB.Dutch
  K.English -> SB.English
  K.Finnish -> SB.Finnish
  K.French -> SB.French
  K.German -> SB.German
  K.Hungarian -> SB.Hungarian
  K.Italian -> SB.Italian
  K.Norwegian -> SB.Norwegian
  K.Portuguese -> SB.Portuguese
  K.Romanian -> SB.Romanian
  K.Russian -> SB.Russian
  K.Spanish -> SB.Spanish
  K.Swedish -> SB.Swedish
  K.Turkish -> SB.Turkish


