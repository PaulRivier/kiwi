{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances, DerivingStrategies #-}

module Kiwi.Types
  ( -- * Server
    ServerState(..)
  , Context(..)
  , noContext
  , Account(..)
  , Login(..)
  , RequestState(..)
  , Session(..)
  , Sessions
  , ETags
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
  , parseCustomMetaType
  , DocIndexKey(..)
  , FieldKey(..)
    -- * Search engine
  , PageSearchEngine
  , PageField(..)
    -- * Config, locales
  , TocSetting(..)
  , parseTocSetting
  , Lang(..)
  , parseLang
  , langCode
  , langName
  , LangSetting(..)
  , baseLang
  , UI_Lang(..)
  , parseUILang
  , Locales(..)
  ) where

import           Control.Concurrent.MVar (MVar)
import           Control.Concurrent.QSem (QSem)
import           Control.Monad.IO.Unlift (MonadUnliftIO)
import           Control.Monad.Reader
import qualified Data.ByteString as BS
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
              , kiwiPort      :: Int
              , accounts      :: IORef [Account]  -- ^ read again from the file
                                                  -- at each login and reload
              , accountsLock  :: MVar ()  -- ^ serializes accounts reloads
              , loginSem      :: QSem  -- ^ limits concurrent password hashes
              , sessions      :: MVar Sessions
              , requestState  :: RequestState  -- ^ the only part that
                                               -- depends on the request
              , pagesDB       :: IORef PagesDB
              , pagesDBLock   :: MVar ()  -- ^ serializes pagesDB updates
              , etags         :: MVar ETags
              , tocSetting    :: TocSetting
              , secureCookie  :: Bool
              , logger        :: Logger
              }


-- | Navigation context, kept in the query string of the links, so that each
-- browser tab has its own
data Context =
  Context { contextSources :: S.Set T.Text  -- ^ navigation restricted to these
                                            -- sources, all if empty
          , contextArchive :: Bool          -- ^ in the archived pages only,
                                            -- instead of the other ones
          }

-- | Context of a new navigation: all the sources, not archived pages
noContext :: Context
noContext = Context S.empty False


data Account =
  Account { accountId :: T.Text
          , accountSalt :: T.Text
          , accountHash :: T.Text
          , accountGroups :: [T.Text]
          }
  deriving (Eq)

-- | The salt and the hash are not shown
instance Show Account where
  show a = concat [ "Account ", show (accountId a), " <salt> <hash> "
                  , show (accountGroups a) ]

-- | User of a request
-- type Login = (T.Text, [T.Text], Time.UTCTime)
data Login =
  Login { loginUser   :: T.Text         -- ^ user id
        , loginGroups :: [T.Text]       -- ^ current groups of the account
        , loginTime   :: Time.UTCTime   -- ^ login time
        }
  deriving (Eq, Show)

-- | What depends on the request in the server state, set by
-- 'Kiwi.Controller.withLogin' and 'Kiwi.Controller.withContext'
data RequestState =
  RequestState { login   :: Maybe Login
               , context :: Context
               }

data Session =
  Session { sessionUser   :: T.Text
          , sessionSaltFp :: T.Text  -- ^ fingerprint of the salt of the account
                                     -- (see 'Kiwi.Auth.saltFingerprint')
          , sessionTime   :: Time.UTCTime  -- ^ login time
          }
  deriving (Eq, Show)

-- | Sessions, by the hash of their token (see 'Kiwi.Auth.hashToken')
type Sessions = M.Map T.Text Session

-- | ETags of the HTML pages sent, with the generation of the pages they were
-- rendered from (see 'Kiwi.Controller.withETag'). A page depends on its URL
-- (with the navigation context) and on its reader: the user name is
-- displayed, and the groups give the visible pages.
type ETags = M.Map (Maybe (T.Text, [T.Text]), BS.ByteString) (Int, BS.ByteString)

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
          , dbGeneration :: Int  -- ^ number of loads (see 'Kiwi.PagesDB.updatePagesDB')
          -- | files that are not loaded: (source, path in its pages folder,
          -- reason), by source then path
          , loadErrors :: [(T.Text, FP.FilePath, T.Text)]
          -- , lastUpdate :: Time.UTCTime
          , pagesIndex :: DI.DocIndex Page PageUID MetaField DocIndexKey
          , searchEngine :: PageSearchEngine }


data MetaField = FieldSource
               | FieldTag
               | FieldAccess
               | FieldLang
               | FieldLink
               | FieldArchive  -- ^ every page, archived or not
               | FieldCustom T.Text
  deriving (Show, Read, Eq, Ord)



data MetaData =
  MetaData { metaId :: Maybe T.Text
           , metaTitle :: T.Text
           , metaTags  :: [TagId]
           , metaAccess :: [T.Text]
           , metaLang  :: LangSetting
           , metaArchive :: Bool
           , metaCustom :: [CustomMetaData]
           }
  deriving (Show)


-- | The document is not kept, it is parsed again from the content to be
-- displayed (see 'Kiwi.Pandoc.pageDoc'). Fields are strict, so that none keeps it alive through a thunk.
data Page =
  Page { pageUID  :: !PageUID   -- ^ page uid, used for routing
       , pageAbsoluteFSPath :: !FP.FilePath  -- ^ path to file, absolute
       , pageMTime :: !Time.UTCTime -- ^ time of file modification from filesystem
       -- , pageText :: !T.Text   -- ^ text of the file, metadata included
       , pageBody :: !T.Text   -- ^ markdown content of the file, without its header
       , pageImagesDir :: !T.Text  -- ^ folder of its "image:" links (images-dir)
       , pageFilesDir :: !T.Text   -- ^ folder of its "file:" links (files-dir)
       , pagePlainText :: !T.Text  -- ^ plain text of its content (search, excerpts)
       , pageTitle :: !T.Text
       , pageTags :: ![TagId]
       , pageLang :: !Lang
       , pageAccess :: ![T.Text]
       , pageLinks :: ![PageUID]
       , pageArchive :: !Bool  -- ^ only in the archives navigation
       , pageCustomMeta :: ![CustomMetaData]
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
             , pandocHeader :: M.Map T.Text [T.Text]  -- ^ fields of the header
             , pandocBody :: T.Text       -- ^ markdown content, without the header
             , pandocImagesDir :: T.Text  -- ^ folder of the "image:" links
             , pandocFilesDir :: T.Text   -- ^ folder of the "file:" links
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

-- instance Read CustomMetaType where
--   readsPrec _ s = case (trimStr $ map toLower s) of
--     "text"   -> [(CmtText, "")]
--     "int"    -> [(CmtInt, "")]
--     "date"   -> [(CmtDate, "")]
--     "bool"   -> [(CmtBool, "")]
--     _        -> []

-- | Type of a custom metadata in the config: text, int, date or bool
parseCustomMetaType :: T.Text -> Either String CustomMetaType
parseCustomMetaType = parseChoice "metadata type"
  [("text", CmtText), ("int", CmtInt), ("date", CmtDate), ("bool", CmtBool)]


data DocIndexKey = KeyText T.Text
                 | KeyInt Int
                 | KeyDay Time.Day
                 | KeyBool Bool
                 | KeyLang Lang
                 | KeyPairText (T.Text, T.Text)
  -- Read is not derived any more: Lang has no Read instance
  deriving (Eq, Ord, Show)


class Ord k => FieldKey k where
  toKey :: k -> DocIndexKey
  fromKey :: DocIndexKey -> Maybe k
  -- toKeys :: [k] -> [DocIndexKey]
  -- toKeys = map toKey
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

-- | Pages are indexed with the plain text of their content
type PageSearchEngine = SearchEngine (Page, T.Text) PageUID PageField NoFeatures

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

-- instance Read TocSetting where
--   readsPrec _ s = case (trimStr $ map toLower s) of
--     "on" -> [(TocOn, "")]
--     "off" -> [(TocOff, "")]
--     "auto" -> [(TocAuto, "")]
--     _         -> []

-- | toc setting of the config: on, off or auto
parseTocSetting :: T.Text -> Either String TocSetting
parseTocSetting = parseChoice "toc setting"
  [("on", TocOn), ("off", TocOff), ("auto", TocAuto)]

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
    deriving (Show, Eq, Ord, Enum, Bounded)

-- | Lang of a page, as set in the metadata: fixed, or detected from the
-- content, with a fallback when the detection fails ("auto/english")
data LangSetting = LangFixed Lang
                 | LangAuto Lang
  deriving (Show, Eq)

-- | The fixed lang, or the fallback of the detection
baseLang :: LangSetting -> Lang
baseLang (LangFixed l) = l
baseLang (LangAuto l)  = l


-- instance Read Lang where
--   readsPrec _ s = case (trimStr $ map toLower s) of
--     "danish" -> [(Danish, "")]
--     "dutch" -> [(Dutch, "")]
--     "english" -> [(English, "")]
--     "finnish" -> [(Finnish, "")]
--     "french" -> [(French, "")]
--     "german" -> [(German, "")]
--     "hungarian" -> [(Hungarian, "")]
--     "italian" -> [(Italian, "")]
--     "norwegian" -> [(Norwegian, "")]
--     "portuguese" -> [(Portuguese, "")]
--     "romanian" -> [(Romanian, "")]
--     "russian" -> [(Russian, "")]
--     "spanish" -> [(Spanish, "")]
--     "swedish" -> [(Swedish, "")]
--     "turkish" -> [(Turkish, "")]
--     _         -> []

-- | Lang name, in lower case (danish … turkish)
parseLang :: T.Text -> Either String Lang
parseLang = parseChoice "lang" [ (langName l, l) | l <- [minBound .. maxBound] ]

-- | Name of a lang, as in the config ("french")
langName :: Lang -> T.Text
langName = T.toLower . T.pack . show

-- | ISO 639-1 code of a lang (html lang attribute, query prefix "fr:"...)
langCode :: Lang -> T.Text
langCode l = case l of
  Danish     -> "da"
  Dutch      -> "nl"
  English    -> "en"
  Finnish    -> "fi"
  French     -> "fr"
  German     -> "de"
  Hungarian  -> "hu"
  Italian    -> "it"
  Norwegian  -> "no"
  Portuguese -> "pt"
  Romanian   -> "ro"
  Russian    -> "ru"
  Spanish    -> "es"
  Swedish    -> "sv"
  Turkish    -> "tr"

data UI_Lang = UI_French
             | UI_English
  deriving (Eq, Show)

-- instance Read UI_Lang where
--   readsPrec _ s = case (trimStr $ map toLower s) of
--     "french" -> [(UI_French, "")]
--     "english" -> [(UI_English, "")]
--     _         -> []

-- | ui-lang setting of the config: english or french
parseUILang :: T.Text -> Either String UI_Lang
parseUILang = parseChoice "ui-lang" [("english", UI_English), ("french", UI_French)]



data Locales = Locales
  { loc_Tags :: T.Text
  , loc_TagsList :: T.Text
  , loc_FiltersList :: T.Text
  , loc_PagesList :: T.Text
  , loc_RecentPages :: T.Text
  , loc_Search :: T.Text
  , loc_SearchResults :: T.Text  -- ^ after the number of results
  , loc_SearchNothing :: T.Text
  , loc_SearchQuery :: T.Text    -- ^ before the lang of the query, then the query
  , loc_Colon :: T.Text          -- ^ ":", with a space before it in French
  , loc_NotFound :: T.Text
  , loc_BadRequest :: T.Text
  , loc_Forbidden :: T.Text
  , loc_ServerError :: T.Text
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
  -- | alerts of the admin actions (app.js): session expired (401), no right
  -- (403), other failure (followed by the HTTP status, if any)
  , loc_ActionSession :: T.Text
  , loc_ActionForbidden :: T.Text
  , loc_ActionFailed :: T.Text
  , loc_Metadata :: T.Text
  , loc_Lang :: T.Text
  , loc_Access :: T.Text
  , loc_Copy :: T.Text
  , loc_Archives :: T.Text
  -- | the /doctor page: title, broken links, when there is none
  , loc_Doctor :: T.Text
  , loc_BrokenLinks :: T.Text
  , loc_NoBrokenLink :: T.Text
  , loc_LoadErrors :: T.Text
  -- , loc_TimeLocale :: Time.TimeLocale
  }
  deriving (Show)


-- trimStr :: String -> String
-- trimStr = f . f
--   where f = reverse . dropWhile isSpace

-- | One of the given names, ignoring case and the spaces around. The error
-- message gives the allowed names.
parseChoice :: String -> [(T.Text, a)] -> T.Text -> Either String a
parseChoice what choices t =
  maybe (Left err) Right $ lookup (T.toLower v) choices
  where
    v = T.strip t
    err = concat [ "invalid ", what, " \"", T.unpack v, "\", allowed values: "
                 , T.unpack $ T.intercalate ", " $ map fst choices ]
