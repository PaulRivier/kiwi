{-# LANGUAGE OverloadedStrings, NamedFieldPuns #-}

module Kiwi.Search (
  initPageSearchEngine
  , extractTerms
  ) where

import           Data.Text.ICU.Char
import           Data.Text.ICU.Normalize
import           Data.SearchEngine
import qualified Data.Set as S
import qualified Data.Text as T
import           NLP.Snowball as SB
import qualified Text.Pandoc as P

import           Kiwi.Types as K


initPageSearchEngine :: Lang -> PageSearchEngine
initPageSearchEngine lg =
    initSearchEngine (pageSearchConfig (alg lg)) defaultSearchRankParameters

pageSearchConfig :: Algorithm -> SearchConfig Page PageId PageField NoFeatures
pageSearchConfig lg =
    SearchConfig {
      documentKey           = pageId,
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
      -- case field of
      --   TitleField -> stem language $ normalizeText tok
      --   TagsField -> T.toLower tok
      --   ContentField -> stem language $ normalizeText tok

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


-- -- Old code parsing query to extract tags. Not really useful.


-- data QueryTerm = QueryTag T.Text | QueryWord T.Text
--   deriving Show

-- type SParser = X.Parsec String () 

-- prepareQuery ::  T.Text -> [T.Text]
-- prepareQuery q = concat $ map makeTerm $ parseTerms (T.strip q)
--   where
--     makeTerm :: QueryTerm -> [T.Text]
--     makeTerm (QueryTag t) = [ T.concat ["[", T.toLower t, "]"] ]
--     makeTerm (QueryWord t) = extractTerms t

--     parseTerms :: T.Text -> [QueryTerm]
--     parseTerms q' =
--       let toksE = X.parse queryParser "" (T.unpack q')
--       in case toksE of
--         Left e -> trace (show e) []
--         Right toks -> toks

--     queryParser :: SParser [QueryTerm]
--     queryParser = X.sepBy (queryTag <|> queryWord)
--                           (X.many $ X.char ' ')

--     queryTag :: SParser QueryTerm
--     queryTag = do
--       let tagparser = X.many1 (X.noneOf "]")
--       t <- X.between (X.char '[') ( X.char ']') tagparser
--       return $ QueryTag $ T.pack $ t

--     queryWord :: SParser QueryTerm
--     queryWord = do
--       word <- X.many1 (X.noneOf " [")
--       return $ QueryWord $ T.pack word
        
              
