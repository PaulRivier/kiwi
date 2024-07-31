{-# LANGUAGE OverloadedStrings, NamedFieldPuns #-}

module Kiwi.Search (
  initPageSearchEngine
  , extractTerms
  ) where

import           Data.Text.ICU.Char
import           Data.Text.ICU.Normalize2
import           Data.SearchEngine
import qualified Data.Set as S
import qualified Data.Text as T
import           NLP.Snowball as SB
import qualified Text.Pandoc as P

import           Kiwi.Types as K


initPageSearchEngine :: Lang -> PageSearchEngine
initPageSearchEngine lg =
    initSearchEngine (pageSearchConfig (alg lg)) defaultSearchRankParameters

pageSearchConfig :: Algorithm -> SearchConfig Page PageUID PageField NoFeatures
pageSearchConfig lg =
    SearchConfig {
      documentKey           = pageUID,
      extractDocumentTerms  = extractTokens,
      transformQueryTerm    = normaliseQueryToken,
      documentFeatureValue  = const noFeatures
  }
  where
    extractTokens :: Page -> PageField -> [T.Text]
    extractTokens p TitleField      = extractTerms (alg $ pageLang p) $
                                      pageTitle p
    extractTokens p TagsField       = concat $
                                      map (extractTagTokens (alg $ pageLang p)) $
                                      pageTags p
    extractTokens p ContentField    = extractTerms (alg $ pageLang p) $
                                      pageContent $ pageDoc p

    normaliseQueryToken :: T.Text -> PageField -> T.Text
    normaliseQueryToken tok _ = T.toLower $ stem lg $ normalizeText tok

    -- dans le cas des tags, on indexe le tag sous sa forme canonique
    -- [tag] ainsi que les termes qui le composent
    extractTagTokens lg' t = (T.concat ["[", T.toLower t, "]"]) : extractTerms lg' t

pageContent :: P.Pandoc -> T.Text
pageContent doc = case (P.runPure $ P.writePlain P.def doc) of
                    Right r ->  r
                    Left _ -> T.empty

extractTerms :: Algorithm -> T.Text -> [T.Text]
extractTerms lg = map normalizeText .
                  stems lg .
                  tokenizeText breakSet

tokenizeText :: S.Set Char -> T.Text -> [T.Text]
tokenizeText brk = filter (not . T.null) .
                     T.split isBreak .
                     T.toLower
  where
    isBreak c = S.member c brk

normalizeText :: T.Text -> T.Text
normalizeText = T.filter (not . property Diacritic) .
                normalize NFD

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


