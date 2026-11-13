{-# LANGUAGE TemplateHaskell #-}

module Kiwi.ConfigFile
  ( KiwiConfig(..)
  , parseConfig
  , defaultConfigFileContent
  ) where

import           Control.Monad (mfilter)
import qualified Data.ByteString as BS
import           Data.Ini.Config
import           Data.FileEmbed (embedFileRelative)
import           Data.Foldable (toList)
import qualified Data.Text as T
import qualified System.FilePath as FP

import qualified Kiwi.Meta as Meta
import qualified Kiwi.Tags as Tags
import qualified Kiwi.Types as K
import qualified Kiwi.Utils as U

data KiwiConfig = KiwiConfig
  { name :: T.Text
  , contentDir :: FP.FilePath
  , port :: Int
  , bind :: Maybe String  -- ^ address to listen on, all of them without it
  , editor :: Maybe (FP.FilePath, [String])
  , uiLang   :: K.UI_Lang
  , defaultMeta :: K.MetaData
  , customMetaConfig :: [K.CustomMetaConfig]
  , sourcesConfig :: [K.SourceConfig]
  , toc :: K.TocSetting
  , secureCookie :: Bool
  , gitCommit :: Bool  -- ^ commit the pages saved in a git repository
  }
  


configParser :: IniParser KiwiConfig
configParser = do
  meta <- section "metadata-default" $ do
    defTitle <- fieldDefOf "title" string "untitled"
    defTags  <- fieldDefOf "tags" string "untagged"
    defAccess <- fieldDefOf "access" accessList [("admin", K.AccessRead)]
    defLang  <- fieldDefOf "lang" U.parseLangSetting K.defaultLangSetting
    return Meta.emptyMeta { K.metaTitle = Just $ T.strip defTitle
                          , K.metaTags = Just $ Tags.splitTags defTags
                          , K.metaAccess = Just defAccess
                          , K.metaLang = Just defLang
                          }
  customMeta' <- sectionsOf (T.stripPrefix "metadata ") customMetaParser
  sourcesConfig' <- sectionsOf (T.stripPrefix "source ") sourceConfigParser
  section "kiwi" $ do
    name'  <- fieldDefOf "name" string "kiwi"
    contentDir' <- fieldOf "content-directory" string
    port' <- fieldOf "port" number
    -- without address (commented out or empty), on all the interfaces
    bind' <- fieldMbOf "bind" string
    -- without editor, the edition of pages is disabled
    editor'  <- fieldDefOf "editor" parseEditor Nothing
    lang'  <- fieldDefOf "ui-lang" K.parseUILang K.UI_English
    toc' <- fieldDefOf "toc" K.parseTocSetting K.TocOn
    secureCookie' <- fieldFlagDef "secure-cookie" False
    -- empty: the default
    gitCommit' <- fieldDefOf "git-commit" (flagOr True "git-commit") True
    return KiwiConfig
      { name = name'
      , contentDir = contentDir'
      , port = port'
      , bind = mfilter (not . null) (trim <$> bind')
      , editor = editor'
      , uiLang = lang'
      , defaultMeta = meta
      , customMetaConfig = toList customMeta'
      , sourcesConfig = toList sourcesConfig'
      , toc = toc'
      , secureCookie = secureCookie'
      , gitCommit = gitCommit'
      }


customMetaParser :: T.Text -> SectionParser K.CustomMetaConfig
customMetaParser name' = do
  cmcType <- fieldOf "type" K.parseCustomMetaType
  cmcThreshold <- fieldDefOf "threshold" number 1.0
  return K.CustomMetaConfig { K.cmcName = name'
                            , K.cmcType = cmcType
                            , K.cmcThreshold = cmcThreshold
                            }


sourceConfigParser :: T.Text -> SectionParser K.SourceConfig
sourceConfigParser source = do
  rt <- fieldDefOf "root-tag" string ""
  access <- fieldDefOf "access" accessList []
  lang <- fieldMbOf "lang" U.parseLangSetting
  return K.SourceConfig { K.scSourceName = source
                        , K.scRootTag = Tags.tagToSegments $ T.pack rt
                        -- without any group, the global default is used
                        , K.scAccess = if null access then Nothing else Just access
                        , K.scLang = lang
                        }


-- | An access list (see 'Meta.parseAccess'), refused with an invalid item
accessList :: T.Text -> Either String K.Access
accessList t = case Meta.parseAccess t of
  (access, []) -> Right access
  (_, invalid) -> Left $ "invalid access: " ++ T.unpack (T.intercalate ", " invalid)
                  ++ " (expected GROUP, GROUP:w or GROUP:a; public can only read)"

-- | A yes/no value (true/false too, any case), or this default when empty
flagOr :: Bool -> String -> T.Text -> Either String Bool
flagOr def key t = case T.toLower (T.strip t) of
  ""      -> Right def
  "yes"   -> Right True
  "true"  -> Right True
  "no"    -> Right False
  "false" -> Right False
  _       -> Left $ "invalid " ++ key ++ " value " ++ show (T.strip t)
                    ++ ", allowed values: yes, no"

trim :: String -> String
trim = T.unpack . T.strip . T.pack

parseConfig :: T.Text -> Either String KiwiConfig
parseConfig t = parseIniFile t configParser

parseEditor :: T.Text -> Either String (Maybe (FP.FilePath, [String]))
parseEditor e = case words $ T.unpack e of
  []          -> Right Nothing
  (cmd:args)  -> Right $ Just (cmd, args)


defaultConfigFileContent :: BS.ByteString
defaultConfigFileContent =
  $(embedFileRelative "static/kiwi.ini.default")
