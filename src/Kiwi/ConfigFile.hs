{-# LANGUAGE TemplateHaskell #-}

module Kiwi.ConfigFile
  ( KiwiConfig(..)
  , parseConfig
  , defaultConfigFileContent
  , agentGuideFile
  , checkAgentGuide
  ) where

import           Control.Exception (IOException, try)
import           Control.Monad (mfilter)
import qualified Data.ByteString as BS
import           Data.Ini.Config
import           Data.FileEmbed (embedFileRelative)
import           Data.Foldable (toList)
import qualified Data.Text as T
import qualified Data.Text.Read as TR
import qualified System.Directory as D
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
  , agentGuide :: Maybe FP.FilePath  -- ^ file of the guide of the user for
                                     -- the agents, as written (see 'agentGuideFile')
  , maxUploadSize :: Integer  -- ^ largest file sent by the editor, in MB
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
    -- without it (or empty), no guide of the user
    agentGuide' <- fieldMbOf "agent-guide" string
    maxUploadSize' <- fieldDefOf "max-upload-size" megabytes 20
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
      , agentGuide = mfilter (not . null) (trim <$> agentGuide')
      , maxUploadSize = maxUploadSize'
      }


customMetaParser :: T.Text -> SectionParser K.CustomMetaConfig
customMetaParser name' = do
  cmcType <- fieldOf "type" K.parseCustomMetaType
  cmcThreshold <- fieldDefOf "threshold" number 1.0
  cmcDescription <- fieldMbOf "description" string
  cmcRelating <- fieldDefOf "relating" relatingWeight 0
  return K.CustomMetaConfig { K.cmcName = name'
                            , K.cmcType = cmcType
                            , K.cmcThreshold = cmcThreshold
                            , K.cmcDescription = cmcDescription
                            , K.cmcRelating = cmcRelating
                            }


sourceConfigParser :: T.Text -> SectionParser K.SourceConfig
sourceConfigParser source = do
  access <- fieldDefOf "access" accessList []
  lang <- fieldMbOf "lang" U.parseLangSetting
  -- empty: the default
  readOnly <- fieldDefOf "read-only" (flagOr False "read-only") False
  return K.SourceConfig { K.scSourceName = source
                        -- without any group, the global default is used
                        , K.scAccess = if null access then Nothing else Just access
                        , K.scLang = lang
                        , K.scReadOnly = readOnly
                        }


-- | A size in megabytes: a whole number, 1 or more
megabytes :: T.Text -> Either String Integer
megabytes t = case TR.decimal (T.strip t) of
  Right (n, rest) | T.null rest, n >= 1 -> Right n
  _ -> Left $ "invalid size " ++ show (T.strip t) ++ ", expected a number of megabytes (1 or more)"

-- | An access list (see 'Meta.parseAccess'), refused with an invalid item
accessList :: T.Text -> Either String K.Access
accessList t = case Meta.parseAccess t of
  (access, []) -> Right access
  (_, invalid) -> Left $ "invalid access: " ++ T.unpack (T.intercalate ", " invalid)
                  ++ " (expected GROUP, GROUP:w or GROUP:a; public can only read)"

-- | The weight of a custom field for the related pages: a number, 0 or
-- more; 0 (the default, also when empty) leaves the field out
relatingWeight :: T.Text -> Either String Double
relatingWeight t = case T.strip t of
  "" -> Right 0
  s -> case TR.double s of
    Right (w, rest) | T.null rest, w >= 0 -> Right w
    _ -> Left $ "invalid relating value " ++ show s ++ ", expected a weight (a number, 0 or more)"

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

-- -- | A page as SOURCE/UID, nothing when empty
-- pageRef :: T.Text -> Either String (Maybe K.PageUID)
-- pageRef t = case T.breakOn "/" (T.strip t) of
--   ("", "") -> Right Nothing
--   (s, u) | not (T.null s), Just uid <- T.stripPrefix "/" u, Meta.isValidUid uid
--            -> Right $ Just (s, uid)
--   _ -> Left $ "invalid page " ++ show (T.strip t) ++ ", expected SOURCE/UID"

-- | The file of the guide of the user for the agents, if any: its path in
-- the config, relative to the folder of the config file (given), or absolute
agentGuideFile :: FP.FilePath -> KiwiConfig -> Maybe FP.FilePath
agentGuideFile configFile conf = FP.combine (FP.takeDirectory configFile) <$> agentGuide conf

-- | An error when the file of the guide of the user (see 'agentGuideFile')
-- does not exist or can not be read
checkAgentGuide :: FP.FilePath -> KiwiConfig -> IO (Maybe String)
checkAgentGuide configFile conf = case agentGuideFile configFile conf of
  Nothing -> return Nothing
  Just p -> do
    exists <- D.doesFileExist p
    if not exists then return $ Just (failure p "does not exist")
    else either (\e -> Just $ failure p ("can not be read: " ++ show (e :: IOException)))
                (const Nothing) <$> try (BS.length <$> BS.readFile p)
  where
    failure p why = concat [ "agent guide file ", p, " (config file ", configFile, ") ", why ]

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
