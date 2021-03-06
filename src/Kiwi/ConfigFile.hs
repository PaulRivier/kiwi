{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

module Kiwi.ConfigFile where

import           Data.Ini.Config
import           Data.Foldable (toList)
import qualified Data.Text as T
import qualified System.FilePath as FP
import           Text.RawString.QQ (r)

import qualified Kiwi.Types as K
import qualified Kiwi.Utils as U

data KiwiConfig = KiwiConfig
  { name :: T.Text
  , contentDir :: FP.FilePath
  , themeDir :: FP.FilePath
  , port :: Int
  , editor :: Maybe (FP.FilePath, [String])
  , uiLang   :: K.UI_Lang
  , defaultMeta :: K.MetaData
  , customMetaConfig :: [K.CustomMetaConfig]
  , sourcesConfig :: [K.SourceConfig]
  , logging :: Bool
  , toc :: K.TocSetting
  }
  


configParser :: IniParser KiwiConfig
configParser = do
  meta <- section "metadata-default" $ do
    defTitle <- fieldDefOf "title" string "untitled"
    defTags  <- fieldDefOf "tags" string "untagged"
    defAccess <- fieldDefOf "access" string "private"
    defLang  <- fieldDefOf "lang" readable K.English
    return K.MetaData { K.metaId = Nothing
                      , K.metaTitle = T.strip defTitle
                      , K.metaTags = U.splitMeta defTags
                      , K.metaAccess = U.splitMeta defAccess 
                      , K.metaLang = defLang
                      , K.metaCustom = []
                      }
  customMeta' <- sectionsOf (T.stripPrefix "metadata ") customMetaParser
  sourcesConfig' <- sectionsOf (T.stripPrefix "source ") sourceConfigParser
  section "kiwi" $ do
    name'  <- fieldDefOf "name" string "kiwi"
    contentDir' <- fieldOf "content-directory" string
    themeDir'  <- fieldOf "theme-directory" string
    port' <- fieldOf "port" number
    editor'  <- fieldDefOf "editor" parseEditor Nothing
    lang'  <- fieldOf "ui-lang" readable
    logging' <- fieldOf "log" flag
    toc' <- fieldDefOf "toc" readable K.TocOn
    return KiwiConfig
      { name = name'
      , contentDir = contentDir'
      , themeDir = themeDir'
      , port = port'
      , editor = editor'
      , uiLang = lang'
      , defaultMeta = meta
      , customMetaConfig = toList customMeta'
      , sourcesConfig = toList sourcesConfig'
      , logging = logging'
      , toc = toc'
      }


customMetaParser :: T.Text -> SectionParser K.CustomMetaConfig
customMetaParser name' = do
  cmcType <- fieldOf "type" readable
  cmcThreshold <- fieldDefOf "browse-threshold" number 1.0
  return K.CustomMetaConfig { K.cmcName = name'
                            , K.cmcType = cmcType
                            , K.cmcThreshold = cmcThreshold
                            }


sourceConfigParser :: T.Text -> SectionParser K.SourceConfig
sourceConfigParser source = do
  rt <- fieldDefOf "root-tag" string ""
  return K.SourceConfig { K.scSourceName = source
                        , K.scRootTag = U.splitMeta $ T.strip $ T.pack rt
                        }


parseConfig :: T.Text -> Either String KiwiConfig
parseConfig t = parseIniFile t configParser

parseEditor :: T.Text -> Either String (Maybe (FP.FilePath, [String]))
parseEditor e = case words $ T.unpack e of
  []          -> Right Nothing
  (cmd:args)  -> Right $ Just (cmd, args)

defaultConfigFileContent :: String
defaultConfigFileContent =
  [r|[kiwi]
# Give a name to this kiwi
name: My Kiwi
# Path to your content, either absolute or relative to this file folder  
content-directory: content
# Path to your kiwi theme directory, either absolute or relative to this file folder.
# Themes come bundled with distribution, you should link to one of them.
theme-directory: themes/dark
# Your favorite text editor command. File path will be added as last argument.
# Leave empty or remove to disable.
# examples :  emacsclient -n -s kiwi   ///   gedit   ///   gvim
editor: gedit
# Server port
port: 3456
# Default language for UI and fulltext search
ui-lang: english
# Do you want HTTP logs in the console (for debug) ?
log: no
# Do you want to display Toc : on | off | auto
toc: on

# built-in metadata default values
[metadata-default]
title: untitled
tags: untagged
# access : built-in special groups are 'admin' and 'public'
# You can use any groups you want here, comma-separated
access: admin
lang: english

# If you want more metadata. Threshold is the minimum ratio of relevance that must
# be reached before the metadata is suggested in the metadata-browser.

# [metadata category]
# type: text
# threshold: 0
# 
# [metadata authors]
# type: text
# threshold: 0.8


# Some sources may be very specific, you may want to nest the tags under a parent tag
# to preserve your browsing experience

# [source dad-botanic]
# root-tag: botanic

|]



