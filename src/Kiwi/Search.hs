{-# LANGUAGE NamedFieldPuns #-}

module Kiwi.Search (
    initPageSearchEngine
  , queryTerms
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



normalizeText :: T.Text -> T.Text
normalizeText = T.filter ((/= NonSpacingMark) . generalCategory) .
                normalize NFD

-- normalizeText :: T.Text -> T.Text
-- normalizeText = T.filter (not . property Diacritic) .
--                 normalize NFD

breakSet :: S.Set Char
breakSet = S.fromList " \n&~\"#'{([-|`\\_^@)]=}+%*<>,?;.:/!§$€«» ’‘"



defaultSearchRankParameters :: SearchRankParameters PageField NoFeatures
defaultSearchRankParameters =
    SearchRankParameters {
      paramK1,
      paramB,
      paramFieldWeights,
      paramFeatureWeights     = noFeatures,
      paramFeatureFunctions   = noFeatures,
      paramResultsetSoftLimit = 100,
      paramResultsetHardLimit = 200,
      paramAutosuggestPrefilterLimit  = 100,
      paramAutosuggestPostfilterLimit = 100
    }
  where
    paramK1 :: Float
    paramK1 = 1.5

    paramB :: PageField -> Float
    paramB TitleField       = 0.3
    paramB TagsField        = 0.2
    paramB ContentField     = 0.7

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


