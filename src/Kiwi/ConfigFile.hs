{-# LANGUAGE TemplateHaskell #-}

module Kiwi.ConfigFile
  ( KiwiConfig(..)
  , parseConfig
  , defaultConfigFileContent
  ) where

import qualified Data.ByteString as BS
import           Data.Ini.Config
import           Data.FileEmbed (embedFileRelative)
import           Data.Foldable (toList)
import qualified Data.Text as T
import qualified System.FilePath as FP

import qualified Kiwi.Types as K
import qualified Kiwi.Utils as U

data KiwiConfig = KiwiConfig
  { name :: T.Text
  , contentDir :: FP.FilePath
  , port :: Int
  , editor :: Maybe (FP.FilePath, [String])
  , uiLang   :: K.UI_Lang
  , defaultMeta :: K.MetaData
  , customMetaConfig :: [K.CustomMetaConfig]
  , sourcesConfig :: [K.SourceConfig]
  , toc :: K.TocSetting
  , secureCookie :: Bool
  }
  


configParser :: IniParser KiwiConfig
configParser = do
  meta <- section "metadata-default" $ do
    defTitle <- fieldDefOf "title" string "untitled"
    defTags  <- fieldDefOf "tags" string "untagged"
    defAccess <- fieldDefOf "access" string "private"
    defLang  <- fieldDefOf "lang" U.parseLangSetting (K.LangFixed K.English)
    return K.MetaData { K.metaId = Nothing
                      , K.metaTitle = T.strip defTitle
                      , K.metaTags = U.splitTags defTags
                      , K.metaAccess = U.splitMeta defAccess 
                      , K.metaLang = defLang
                      , K.metaCustom = []
                      }
  customMeta' <- sectionsOf (T.stripPrefix "metadata ") customMetaParser
  sourcesConfig' <- sectionsOf (T.stripPrefix "source ") sourceConfigParser
  section "kiwi" $ do
    name'  <- fieldDefOf "name" string "kiwi"
    contentDir' <- fieldOf "content-directory" string
    port' <- fieldOf "port" number
    editor'  <- fieldDefOf "editor" parseEditor Nothing
    lang'  <- fieldOf "ui-lang" readable
    toc' <- fieldDefOf "toc" readable K.TocOn
    secureCookie' <- fieldFlagDef "secure-cookie" False
    return KiwiConfig
      { name = name'
      , contentDir = contentDir'
      , port = port'
      , editor = editor'
      , uiLang = lang'
      , defaultMeta = meta
      , customMetaConfig = toList customMeta'
      , sourcesConfig = toList sourcesConfig'
      , toc = toc'
      , secureCookie = secureCookie'
      }


customMetaParser :: T.Text -> SectionParser K.CustomMetaConfig
customMetaParser name' = do
  cmcType <- fieldOf "type" readable
  cmcThreshold <- fieldDefOf "threshold" number 1.0
  return K.CustomMetaConfig { K.cmcName = name'
                            , K.cmcType = cmcType
                            , K.cmcThreshold = cmcThreshold
                            }


sourceConfigParser :: T.Text -> SectionParser K.SourceConfig
sourceConfigParser source = do
  rt <- fieldDefOf "root-tag" string ""
  access <- fieldDefOf "access" string ""
  lang <- fieldMbOf "lang" U.parseLangSetting
  return K.SourceConfig { K.scSourceName = source
                        , K.scRootTag = U.tagToSegments $ T.pack rt
                        -- without any group, the global default is used
                        , K.scAccess = case U.splitMeta (T.pack access) of
                            [] -> Nothing
                            groups -> Just groups
                        , K.scLang = lang
                        }


parseConfig :: T.Text -> Either String KiwiConfig
parseConfig t = parseIniFile t configParser

parseEditor :: T.Text -> Either String (Maybe (FP.FilePath, [String]))
parseEditor e = case words $ T.unpack e of
  []          -> Right Nothing
  (cmd:args)  -> Right $ Just (cmd, args)


defaultConfigFileContent :: BS.ByteString
defaultConfigFileContent =
  $(embedFileRelative "static/kiwi.ini.default")
