{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances, DerivingStrategies #-}

module Kiwi.Types
  ( -- * Server
    ServerState(..)
  , Account(..)
  , Login
  , Sessions
  , ServerM(..)
  , WebM
  , ActM
    -- * Pages and metadata
  , PagesDB(..)
  , MetaField(..)
  , MetaData(..)
  , Page(..)
  , PageUID
  , TagId
  , TagSegments
  , PandocPage(..)
  , CollectedFromDoc(..)
  , CustomMetaData(..)
  , CustomMetaConfig(..)
  , SourceConfig(..)
  , CustomMetaType(..)
  , DocIndexKey(..)
  , FieldKey(..)
    -- * Search engine
  , PageSearchEngine
  , PageField(..)
    -- * Config, locales
  , TocSetting(..)
  , Lang(..)
  , LangSetting(..)
  , baseLang
  , UI_Lang(..)
  , Locales(..)
  ) where

import           Control.Concurrent.MVar (MVar)
import           Control.Monad.IO.Unlift (MonadUnliftIO)
import           Control.Monad.Reader
import           Data.Char (toLower, isSpace)
import           Data.IORef
import           Data.Ix (Ix)
import qualified Data.Map.Strict as M
import           Data.Maybe (mapMaybe)
import           Data.SearchEngine
import qualified Data.Set as S
import qualified Data.Text as T
-- import qualified Data.Text.Lazy as TL
import qualified Data.Time as Time
import qualified System.FilePath as FP
import qualified Text.Pandoc.Definition as PD
import           Web.Scotty.Trans

import           Kiwi.Log (Logger)
import qualified Utils.DocIndex as DI

data ServerState =
  ServerState { kiwiName      :: T.Text
              , contentDir    :: FP.FilePath
              , contentSources :: [FP.FilePath]
              , kiwiDir       :: FP.FilePath
              , editorCommand :: Maybe (FP.FilePath, [String])
              , uiLang        :: UI_Lang
              , accounts      :: [Account]
              , sessions      :: MVar Sessions
              , login         :: Maybe Login
              , sourcesContext :: S.Set T.Text  -- ^ navigation restricted to
                                                -- these sources, all if empty
              , pagesDB       :: IORef PagesDB
              , pagesDBLock   :: MVar ()  -- ^ serializes pagesDB updates
              , tocSetting    :: TocSetting
              , secureCookie  :: Bool
              , logger        :: Logger
              }


data Account = 
  Account { accountId :: T.Text
          , accountSalt :: T.Text
          , accountHash :: T.Text
          , accountGroups :: [T.Text]
          }

type Login = (T.Text, [T.Text], Time.UTCTime)  -- ^ (user id, groups, login time)

type Sessions = M.Map T.Text Login -- ^ Map Session-Key Login

newtype ServerM a = ServerM { runServerM :: ReaderT ServerState IO a }
  deriving (Applicative, Functor, Monad, MonadIO, MonadReader ServerState, MonadUnliftIO)


-- type WebM a = ScottyT TL.Text ServerM a
-- type ActM a = ActionT TL.Text ServerM a
type WebM a = ScottyT ServerM a
type ActM a = ActionT ServerM a

data PagesDB =
  PagesDB { defaultMeta   :: MetaData
          , customMetaConfig :: [CustomMetaConfig]
          , sourcesConfig :: [SourceConfig]
          , lastUpdate :: Time.UTCTime
          , pagesIndex :: DI.DocIndex Page PageUID MetaField DocIndexKey
          , searchEngine :: PageSearchEngine }


data MetaField = FieldSource
               | FieldTag
               | FieldAccess
               | FieldLang
               | FieldLink
               | FieldCustom T.Text
  deriving (Show, Read, Eq, Ord)



data MetaData =
  MetaData { metaId :: Maybe T.Text
           , metaTitle :: T.Text
           , metaTags  :: [TagId]
           , metaAccess :: [T.Text]
           , metaLang  :: LangSetting
           , metaCustom :: [CustomMetaData]
           }
  deriving (Show)


data Page =
  Page { pageUID  :: PageUID   -- ^ page uid, used for routing
       , pageAbsoluteFSPath :: FP.FilePath  -- ^ path to file, absolute
       , pageMTime :: Time.UTCTime -- ^ time of file modification from filesystem
       , pageDoc :: PD.Pandoc
       , pageTitle :: T.Text
       , pageTags :: [TagId]
       , pageLang :: Lang
       , pageAccess :: [T.Text]
       , pageLinks :: [PageUID]
       , pageCustomMeta :: [CustomMetaData]
       }
  deriving (Show)


type PageUID = (T.Text, T.Text) -- ^ (source, page id)
-- type PageSet = S.Set Page
type TagId = T.Text
type TagSegments = [TagId]

data PandocPage =
  PandocPage { pandocDoc :: PD.Pandoc
             , pandocMeta :: MetaData
             , pandocPageLinks :: [PageUID]
             }
  deriving (Show)

data CollectedFromDoc = CollectedPageLink T.Text -- ^ TargetId
--                    | CollectedFoo ...

data CustomMetaData =
  CustomMetaData { cmdName :: T.Text
                 , cmdKeys :: [DocIndexKey]
                 }
  deriving (Show)


data CustomMetaConfig = 
  CustomMetaConfig { cmcName    :: T.Text
                   , cmcType    :: CustomMetaType
                   , cmcThreshold :: Float
                   }
  deriving (Show)

data SourceConfig = 
  SourceConfig { scSourceName :: T.Text
               , scRootTag :: TagSegments
               , scAccess :: Maybe [T.Text]  -- ^ default access of the pages
               , scLang :: Maybe LangSetting  -- ^ default lang of the pages
               }

data CustomMetaType = CmtText | CmtInt | CmtDate | CmtBool
  deriving (Eq, Show)

instance Read CustomMetaType where
  readsPrec _ s = case (trimStr $ map toLower s) of
    "text"   -> [(CmtText, "")]
    "int"    -> [(CmtInt, "")]
    "date"   -> [(CmtDate, "")]
    "bool"   -> [(CmtBool, "")]
    _        -> []


data DocIndexKey = KeyText T.Text
                 | KeyInt Int
                 | KeyDay Time.Day
                 | KeyBool Bool
                 | KeyLang Lang
                 | KeyPairText (T.Text, T.Text)
  deriving (Eq, Ord, Show, Read)


class Ord k => FieldKey k where
  toKey :: k -> DocIndexKey
  fromKey :: DocIndexKey -> Maybe k
  toKeys :: [k] -> [DocIndexKey]
  toKeys = map toKey
  fromKeys :: [DocIndexKey] -> [k]
  fromKeys = mapMaybe fromKey

instance FieldKey T.Text where
  toKey = KeyText
  fromKey (KeyText k) = Just k
  fromKey _ = Nothing

instance FieldKey Int where
  toKey = KeyInt
  fromKey (KeyInt k) = Just k
  fromKey _ = Nothing

instance FieldKey Time.Day where
  toKey = KeyDay
  fromKey (KeyDay k) = Just k
  fromKey _ = Nothing

instance FieldKey Bool where
  toKey = KeyBool
  fromKey (KeyBool k) = Just k
  fromKey _ = Nothing

instance FieldKey Lang where
  toKey = KeyLang
  fromKey (KeyLang k) = Just k
  fromKey _ = Nothing

instance FieldKey (T.Text,T.Text) where
  toKey = KeyPairText
  fromKey (KeyPairText k) = Just k
  fromKey _ = Nothing



-- Search Engine

type PageSearchEngine = SearchEngine Page PageUID PageField NoFeatures

data PageField = TitleField
               | TagsField
               | ContentField
  deriving (Eq, Ord, Enum, Bounded, Ix, Show)


-- Config, locales ...
data TocSetting = TocOn | TocOff | TocAuto
  deriving (Eq, Ord, Show)

-- -- | Value of the setting, as given to templates
-- tocSettingValue :: TocSetting -> T.Text
-- tocSettingValue TocOn = "on"
-- tocSettingValue TocOff = "off"
-- tocSettingValue TocAuto = "auto"

instance Read TocSetting where
  readsPrec _ s = case (trimStr $ map toLower s) of
    "on" -> [(TocOn, "")]
    "off" -> [(TocOff, "")]
    "auto" -> [(TocAuto, "")]
    _         -> []

data Lang
    = Danish
    | Dutch
    | English
    | Finnish
    | French
    | German
    | Hungarian
    | Italian
    | Norwegian
    | Portuguese
    | Romanian
    | Russian
    | Spanish
    | Swedish
    | Turkish
    deriving (Show, Eq, Ord)

-- | Lang of a page, as set in the metadata: fixed, or detected from the
-- content, with a fallback when the detection fails ("auto/english")
data LangSetting = LangFixed Lang
                 | LangAuto Lang
  deriving (Show, Eq)

-- | The fixed lang, or the fallback of the detection
baseLang :: LangSetting -> Lang
baseLang (LangFixed l) = l
baseLang (LangAuto l)  = l


instance Read Lang where
  readsPrec _ s = case (trimStr $ map toLower s) of
    "danish" -> [(Danish, "")]
    "dutch" -> [(Dutch, "")]
    "english" -> [(English, "")]
    "finnish" -> [(Finnish, "")]
    "french" -> [(French, "")]
    "german" -> [(German, "")]
    "hungarian" -> [(Hungarian, "")]
    "italian" -> [(Italian, "")]
    "norwegian" -> [(Norwegian, "")]
    "portuguese" -> [(Portuguese, "")]
    "romanian" -> [(Romanian, "")]
    "russian" -> [(Russian, "")]
    "spanish" -> [(Spanish, "")]
    "swedish" -> [(Swedish, "")]
    "turkish" -> [(Turkish, "")]
    _         -> []

data UI_Lang = UI_French
             | UI_English
  deriving (Show)

instance Read UI_Lang where
  readsPrec _ s = case (trimStr $ map toLower s) of
    "french" -> [(UI_French, "")]
    "english" -> [(UI_English, "")]
    _         -> []



data Locales = Locales
  { loc_Home :: T.Text
  , loc_Tags :: T.Text
  , loc_TagsList :: T.Text
  , loc_FiltersList :: T.Text
  , loc_PagesList :: T.Text
  , loc_RecentPages :: T.Text
  , loc_Search :: T.Text
  , loc_SearchResults :: T.Text
  , loc_SearchNothing :: T.Text
  , loc_Agenda :: T.Text
  , loc_Today :: T.Text
  , loc_ToCome :: T.Text
  , loc_NotFound :: T.Text
  , loc_BadRequest :: T.Text
  , loc_Forbidden :: T.Text
  , loc_RemoveFilter :: T.Text
  , loc_LoginFailed :: T.Text
  , loc_LangCode :: T.Text  -- ^ html lang attribute
  , loc_Contents :: T.Text
  , loc_Login :: T.Text
  , loc_Logout :: T.Text
  , loc_Username :: T.Text
  , loc_Password :: T.Text
  , loc_EditPage :: T.Text
  , loc_CopyLink :: T.Text
  , loc_Reload :: T.Text
  -- , loc_TimeLocale :: Time.TimeLocale
  }
  deriving (Show)


trimStr :: String -> String
trimStr = f . f
  where f = reverse . dropWhile isSpace
