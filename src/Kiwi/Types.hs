{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Kiwi.Types where

import           Control.Monad.Reader
import           Data.Char (toLower, isSpace)
import           Data.IORef
import           Data.Ix (Ix)
import qualified Data.Map.Strict as M
import           Data.Maybe (catMaybes)
import           Data.SearchEngine
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Time as Time
import qualified System.FilePath as FP
import qualified Text.Mustache as X
import qualified Text.Pandoc as P
import           Web.Scotty.Trans

import qualified Utils.DocIndex as DI

data ServerState =
  ServerState { kiwiName      :: T.Text
              , contentDir    :: FP.FilePath
              , staticDir     :: FP.FilePath
              , kiwiDir       :: FP.FilePath
              , editorCommand :: Maybe (FP.FilePath, [String])
              , uiLang        :: UI_Lang
              , template      :: KiwiTemplate
              , accounts      :: [Account]
              , sessions      :: IORef Sessions
              , login         :: Maybe Login
              , pagesDB       :: IORef PagesDB
              }


data KiwiTemplate =
  KiwiTemplate { layoutTemplate :: X.Template
               , homeTemplate :: X.Template
               , loginTemplate :: X.Template
               , pageTemplate :: X.Template
               , taggedTemplate :: X.Template
               , browseTemplate :: X.Template
               , agendaTemplate :: X.Template
               , searchTemplate :: X.Template
               , notFoundTemplate :: X.Template
               , forbiddenTemplate :: X.Template
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
  deriving (Applicative, Functor, Monad, MonadIO, MonadReader ServerState)


type WebM a = ScottyT TL.Text ServerM a
type ActM a = ActionT TL.Text ServerM a


data PagesDB =
  PagesDB { pagesDir :: FP.FilePath
          , defaultMeta   :: MetaData
          , customMetaConfig :: [CustomMetaConfig]
          , lastUpdate :: Time.UTCTime
          , pagesIndex :: DI.DocIndex Page PageId MetaField DocIndexKey
          , searchEngine :: PageSearchEngine }


data MetaField = FieldTag
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
           , metaLang  :: Lang
           , metaCustom :: [CustomMetaData]
           }
  deriving (Show)


data Page =
  Page { pageId  :: PageId   -- ^ page id, used for routing
       , pageFSPath :: FP.FilePath  -- ^ path to file
       , pageMTime :: Time.UTCTime -- ^ time of file modification from filesystem
       , pageDoc :: P.Pandoc
       , pageTitle :: T.Text
       , pageTags :: [TagId]
       , pageLang :: Lang
       , pageAccess :: [T.Text]
       , pageLinks :: [PageId]
       , pageCustomMeta :: [CustomMetaData]
       }
  deriving (Show)


type PageId = T.Text
type PageSet = S.Set Page
type TagId = T.Text
type TagSegments = [TagId]

data PandocPage =
  PandocPage { pandocDoc :: P.Pandoc
             , pandocMeta :: MetaData
             , pandocPageLinks :: [PageId]
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

data CustomMetaType = CmtText | CmtInt | CmtDate | CmtBool
  deriving (Eq, Show)

instance Read CustomMetaType where
  readsPrec _ s = case (trimStr $ map toLower s) of
    "text"   -> [(CmtText, "")]
    "int"    -> [(CmtInt, "")]
    "date"   -> [(CmtDate, "")]
    "bool"   -> [(CmtBool, "")]
    -- "[text]" -> [(CmtTextList, "")]
   -- "[int]"  -> [(CmtIntList, "")]
    -- "[date]" -> [(CmtDateList, "")]
    _        -> []


data DocIndexKey = KeyText T.Text
                 | KeyInt Int
                 | KeyDay Time.Day
                 | KeyBool Bool
                 | KeyLang Lang
  deriving (Eq, Ord, Show, Read)


class Ord k => FieldKey k where
  toKey :: k -> DocIndexKey
  fromKey :: DocIndexKey -> Maybe k
  toKeys :: [k] -> [DocIndexKey]
  toKeys = map toKey
  fromKeys :: [DocIndexKey] -> [k]
  fromKeys = catMaybes . map fromKey

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


-- Search Engine

type PageSearchEngine = SearchEngine Page PageId PageField NoFeatures

data PageField = TitleField
               | TagsField
               | ContentField
  deriving (Eq, Ord, Enum, Bounded, Ix, Show)

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
  , loc_PagesList :: T.Text
  , loc_Search :: T.Text
  , loc_SearchResults :: T.Text
  , loc_SearchNothing :: T.Text
  , loc_Agenda :: T.Text
  , loc_Today :: T.Text
  , loc_ToCome :: T.Text
  , loc_NotFound :: T.Text
  , loc_Forbidden :: T.Text
  , loc_TimeLocale :: Time.TimeLocale
  }
  deriving (Show)


trimStr :: String -> String
trimStr = f . f
  where f = reverse . dropWhile isSpace
