{-# LANGUAGE NamedFieldPuns #-}

module Kiwi.Search (
    initPageSearchEngine
  , searchPages
  , queryTerms
  , queryLang
  , searchLang
  , normalizeText
  ) where

-- import           Data.Text.ICU.Char
-- import           Data.Text.ICU.Normalize2
import Data.Char (generalCategory, GeneralCategory(NonSpacingMark),
                  isPunctuation, isSpace, isSymbol)
import Data.List (find, nub)
import Data.Maybe (fromMaybe)
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
    extractTokens (p, _) TagsField  = concatMap (extractTerms (alg $ pageLang p)) $
                                      pageTags p
    extractTokens (p, content) ContentField = extractTerms (alg $ pageLang p) content

    -- the tags are indexed word by word, without the term "[tag]" of before:
    -- dans le cas des tags, on indexe le tag sous sa forme canonique
    -- [tag] ainsi que les termes qui le composent
    -- extractTagTokens lg' t = (T.concat ["[", T.toLower t, "]"]) : extractTerms lg' t


-- | Visible pages matching the query terms, best first, without the pages
-- whose score is far below the best visible one (see 'relevanceThreshold').
-- 'queryExplain' does the same work as 'query', it also gives the scores.
searchPages :: PageSearchEngine -> (PageUID -> Bool) -> [T.Text] -> [PageUID]
searchPages se visible terms = case filter (visible . snd) (queryExplain se terms) of
  [] -> []
  results@((best, _) : _) ->
    let top = overallScore best
        -- no threshold when the best score is not positive: the query has
        -- only frequent terms (see the IDF below)
        relevant (e, _) = top <= 0 || overallScore e >= relevanceThreshold * top
    in map snd $ takeWhile relevant results

-- | Minimal score of a displayed page, as a ratio of the best score. The score
-- of a term is capped (see 'paramK1'), so a page with two of the terms has
-- about twice the score of a page with only one of them, if they are about as
-- rare: 0.5 keeps the pages close to the best one. 0 displays all the pages.
relevanceThreshold :: Float
relevanceThreshold = 0.4


-- | Terms of a query, stemmed in the given lang, unless the query starts with
-- the code of another lang ("fr:", "es: "...). The words are split after this
-- prefix, as those of the pages (see 'extractTerms').
queryTerms :: Lang -> T.Text -> [T.Text]
queryTerms lg q = let (lg', q') = queryLang lg q in extractTerms (alg lg') q'

-- | Lang of the query, and the query without its lang prefix ("fr:") if any:
-- the lang of the prefix, or else the given one
queryLang :: Lang -> T.Text -> (Lang, T.Text)
queryLang lg q = case T.breakOn ":" (T.stripStart q) of
  (code, rest) | Just lg' <- lookup (T.toLower code) langCodes, not (T.null rest)
                 -> (lg', T.drop 1 rest)
  _ -> (lg, q)

-- | Lang of a query without lang prefix, from the sources of the navigation
-- context, or from all the sources if it is empty: the lang of the
-- @[source …]@ section of each source, or else the default one (from
-- @[metadata-default]@). The query is too short to detect its lang, so
-- @auto/X@ counts as X. When the sources do not share a lang, the default
-- one.
searchLang :: LangSetting     -- ^ default lang
           -> [SourceConfig]
           -> [T.Text]        -- ^ all the sources
           -> S.Set T.Text    -- ^ sources of the context
           -> Lang
searchLang defLang configs allSources ctxSources =
  case nub (map sourceLang sources) of
    [l] -> l
    _   -> baseLang defLang
  where
    sources | S.null ctxSources = allSources
            | otherwise         = S.toList ctxSources
    sourceLang s = baseLang $ fromMaybe defLang $
                   find ((== s) . scSourceName) configs >>= scLang

-- | ISO 639-1 codes of the langs
langCodes :: [(T.Text, Lang)]
langCodes = [ (K.langCode l, l) | l <- [minBound .. maxBound] ]

-- pageContent :: P.Pandoc -> T.Text
-- pageContent doc = case (P.runPure $ P.writePlain P.def doc) of
--                     Right r ->  r
--                     Left _ -> T.empty

-- | Terms of the pages and of the queries. Words are stemmed with their
-- accents (in NFC, as Snowball expects), and the accents are removed from the
-- stems
extractTerms :: Algorithm -> T.Text -> [T.Text]
extractTerms lg = map normalizeText .
                  stems lg .
                  tokenize

-- | Words of a text, in lower case and NFC, split on spaces (non-breaking
-- ones too), punctuation (apostrophes, quotes, dashes, "…"...) and symbols
tokenize :: T.Text -> [T.Text]
tokenize = filter (not . T.null) .
           T.split isBreak .
           normalize NFC .
           T.toLower
  where
    isBreak c = isSpace c || isPunctuation c || isSymbol c

-- tokenizeText :: S.Set Char -> T.Text -> [T.Text]
-- tokenizeText brk = filter (not . T.null) .
--                      T.split isBreak .
--                      T.toLower
--   where
--     isBreak c = S.member c brk



-- | Text without its accents (also used to sort texts, with T.toLower)
normalizeText :: T.Text -> T.Text
normalizeText = T.filter ((/= NonSpacingMark) . generalCategory) .
                normalize NFD

-- normalizeText :: T.Text -> T.Text
-- normalizeText = T.filter (not . property Diacritic) .
--                 normalize NFD

-- breakSet :: S.Set Char
-- breakSet = S.fromList " \n&~\"#'{([-|`\\_^@)]=}+%*<>,?;.:/!§$€«» ’‘"



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
      -- most frequent terms are then silently ignored. These limits count
      -- all the pages, before 'searchPages' keeps the visible ones, so there
      -- is none: a term in many pages that the user can not read would hide
      -- the few ones they can read. 'relevanceThreshold' drops the pages
      -- that are far from the best one.
      paramResultsetSoftLimit = maxBound,
      paramResultsetHardLimit = maxBound,
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


