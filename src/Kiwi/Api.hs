-- | The API of kiwi, for the agents (language models) and the scripts:
-- routes under /api/, answers in compact text (see "Kiwi.Api.Text"). A
-- request is authenticated by an API token only (Authorization: Bearer,
-- see 'Auth.tokenLogin'), never by the session cookie, and gets the rights
-- of the account of the token.
module Kiwi.Api
  ( withToken
  , apiPages
  , apiVocabulary
  , apiPage
  , apiPageMeta
  , apiFolders
  , apiGuide
  , apiSearch
  , apiPutPage
  , apiPatchPage
  , apiCreatePage
  , apiMetadata
  ) where

import           Control.Monad.Reader (local)
import           Data.IORef (readIORef)
import           Data.List (sortOn)
import           Data.Maybe (fromMaybe)
import qualified Data.Map.Strict as M
import           Data.Ord (Down (..))
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as AesonT
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as TL
import qualified Data.Time as Time
import qualified Network.HTTP.Types as Net
import qualified System.FilePath as FP
import           Text.Read (readMaybe)
import           Web.Scotty.Trans (queryParamMaybe, queryParams, setHeader, status)
import qualified Web.Scotty.Trans as Scotty

import qualified Kiwi.Api.Search as AS
import qualified Kiwi.Api.Text as AT
import qualified Kiwi.Auth as Auth
import qualified Kiwi.Browse as Browse
import qualified Kiwi.DefaultPages as DP
import qualified Kiwi.Edit as Edit
import qualified Kiwi.Log as L
import qualified Kiwi.MetaScript as MS
import           Kiwi.Controller (findPage, getRestrictedDB, keyOrder, textOrder, withContext)
import           Kiwi.Meta (splitPage)
import           Kiwi.Pandoc (pageDoc)
import           Kiwi.Search (alg, explainPages, extractTerms, queryLang, searchLang, termFields)
import           Kiwi.Types (ActM)
import qualified Kiwi.Types as K
import qualified Kiwi.Tags as Tags
import qualified Kiwi.Url as Url
import           Kiwi.Utils (asksK, getDB)
import qualified Kiwi.Utils as U
import qualified Kiwi.Write as Write
import qualified Utils.DocIndex as DI


-- | Runs the action with the login of the API token of the request, and the
-- navigation context of its parameters ("source", "archive=yes", see
-- 'withContext'), or answers 401
withToken :: ActM () -> ActM ()
withToken run = do
  auth <- Scotty.header "Authorization"
  case T.strip <$> (T.stripPrefix "Bearer " . TL.toStrict =<< auth) of
    Nothing -> unauthorized "an API token is required (Authorization: Bearer TOKEN)"
    Just token -> do
      kd <- asksK K.kiwiDir
      tokens <- liftIO' $ Auth.loadTokens (Auth.tokensFile kd)
      accounts' <- liftIO' . readIORef =<< asksK K.accounts
      case Auth.tokenLogin accounts' tokens token of
        Nothing -> do
          logger' <- asksK K.logger
          liftIO' $ logger' L.Warn "API request refused: unknown or revoked token"
          unauthorized "unknown or revoked API token"
        Just l -> local (\ss -> ss { K.requestState = (K.requestState ss) { K.login = Just l } }) $
                    withContext $ do
                      -- the answers depend on the pages, which change
                      setHeader "Cache-Control" "no-store"
                      run
  where
    unauthorized msg = do
      status Net.status401
      setHeader "WWW-Authenticate" "Bearer"
      Scotty.text msg
    liftIO' = Scotty.liftIO

-- | Text answer
answer :: T.Text -> ActM ()
answer = Scotty.text . TL.fromStrict

-- | Error answer, with its status
failWith :: Net.Status -> T.Text -> ActM ()
failWith st msg = status st >> answer msg

-- | An integer parameter, or the default without it
intParam :: T.Text -> Int -> (Int -> ActM ()) -> ActM ()
intParam name def run = do
  vM <- queryParamMaybe name
  case vM of
    Nothing -> run def
    Just v -> case readMaybe (T.unpack v) of
      Just n | n >= 0 -> run n
      _ -> failWith Net.status400 $ "invalid " <> name <> ": " <> v

-- | Paths of the files from the top of the content folder, for the admins
pathsFor :: ActM (Maybe FP.FilePath)
pathsFor = do
  admin <- Auth.isAdmin
  if admin then Just <$> asksK K.contentDir else return Nothing


-- | GET /api/pages: the pages matching the metadata of the parameters (as on
-- /browse: "tag", "lang", "m.FIELD", "has"; "source" and "archive=yes" for
-- the context), modified since a day ("since=YYYY-MM-DD"), the most recently
-- modified first, at most "limit" of them (100 by default)
apiPages :: ActM ()
apiPages = do
  db <- getRestrictedDB
  ctx <- asksK (K.context . K.requestState)
  params <- queryParams
  admin <- Auth.isAdmin
  let filters = filter ((`notElem` ["limit", "since"]) . fst) $ Url.withoutContextParams params
  sinceM <- queryParamMaybe "since"
  intParam "limit" 100 $ \limit ->
    case (Browse.decodeReq (K.customMetaConfig db) filters, traverse readDay sinceM) of
      (Left err, _) -> failWith Net.status400 err
      (_, Nothing) -> failWith Net.status400 "invalid since, expected YYYY-MM-DD"
      (Right req, _) | not admin && M.member K.FieldAccess req ->
                         failWith Net.status400 "unknown parameter: access"
      (Right req, Just since) -> do
        let req' = Browse.sourcesReq (K.contextSources ctx) <> req
            idx = K.pagesIndex db
            selected = if M.null req' then DI.documentsList idx
                       else DI.documentsList $ DI.narrow idx $ Browse.reqToQuery req'
            recent = maybe id (\d -> filter ((>= d) . Time.utctDay . K.pageMTime)) since selected
            pages = sortOn (Down . K.pageMTime) recent
        paths <- pathsFor
        answer $ AT.pagesText paths (length pages) (take limit pages)
  where
    readDay :: T.Text -> Maybe Time.Day
    readDay = Time.parseTimeM False Time.defaultTimeLocale "%Y-%-m-%-d" . T.unpack

-- | GET /api/vocabulary: for each source of the context (all those the
-- user can read without "source"), its tags, langs and custom fields, with
-- the number of pages of each value
apiVocabulary :: ActM ()
apiVocabulary = do
  db <- getRestrictedDB
  ctx <- asksK (K.context . K.requestState)
  let idx = K.pagesIndex db
      sources = [ s | K.KeyText s <- DI.getFieldKeys idx K.FieldSource
                    , S.null (K.contextSources ctx) || S.member s (K.contextSources ctx) ]
      vocabulary s =
        let sub = DI.narrow idx (U.qOnly K.FieldSource s)
            count f k = S.size $ DI.lookupIndex sub f k
            tags = sortOn (map (\seg -> (textOrder seg, seg)) . Tags.tagToSegments . fst)
                     [ (t, count K.FieldTag k) | k@(K.KeyText t) <- DI.getFieldKeys sub K.FieldTag ]
            langs = [ (Browse.showKeyLabel k, count K.FieldLang k)
                    | k <- DI.getFieldKeys sub K.FieldLang ]
            customs = [ (K.cmcName c, typeName (K.cmcType c),
                         [ (Browse.showKeyLabel k, count f k)
                         | k <- sortOn keyOrder (DI.getFieldKeys sub f) ])
                      | c <- sortOn (textOrder . K.cmcName) (K.customMetaConfig db)
                      , let f = K.FieldCustom (K.cmcName c)
                      , not (null $ DI.getFieldKeys sub f) ]
        in AT.vocabularyText s (DI.cardinal sub) tags langs customs
  if null sources
    then answer "no page\n"
    else answer $ T.intercalate "\n" $ map vocabulary $ sortOn textOrder sources
  where
    typeName t = case t of
      K.CmtText -> "text"
      K.CmtInt  -> "int"
      K.CmtDate -> "date"
      K.CmtBool -> "bool"

-- | The page of the request, readable by the user, archived or not, with
-- the text of its file (as shown to an editor with the right of the user:
-- without uid, see 'Edit.editorText'), the hash of the file, and the number
-- of lines before the content in this text; 404 otherwise
withApiPage :: T.Text -> T.Text -> (K.Page -> (T.Text, T.Text, Int) -> ActM ()) -> ActM ()
withApiPage source uid run = do
  db <- getDB
  loginM <- asksK (K.login . K.requestState)
  case findPage db (source, uid) of
    Just p | Just level <- Auth.pageAccessLevel loginM p -> do
      fileE <- Scotty.liftIO $ Write.readPageFile (K.pageAbsoluteFSPath p)
      case fileE >>= \(bytes, t) -> (,) (Edit.fileHash bytes) <$>
                                    Edit.editorText (K.customMetaConfig db) level t of
        Left e -> failWith Net.status500 $ T.pack e
        Right (hash, (text, offset)) -> do
          setHeader "ETag" $ TL.fromStrict $ "\"" <> hash <> "\""
          run p (text, hash, offset)
    _ -> failWith Net.status404 $ "no page " <> source <> "/" <> uid

-- | GET /api/page/SOURCE/UID: the text of the page (see 'withApiPage'), in
-- markdown, with the hash of its file as ETag. "part=outline" gives its
-- outline (see 'AT.outlineText'); "part=section&title=T" the lines of its
-- section of title T; "part=lines&from=N&to=M" these lines.
-- "numbered=yes" gives the lines after their number (not for the outline).
apiPage :: T.Text -> T.Text -> ActM ()
apiPage source uid = withApiPage source uid $ \p (text, hash, offset) -> do
  partM <- queryParamMaybe "part"
  numberedM <- queryParamMaybe "numbered"
  let numbered = numberedM == Just ("yes" :: T.Text)
      total = length (T.lines text)
      send from ls
        | numbered = answer $ AT.numberedLines from ls
        | otherwise = do
            setHeader "Content-Type" "text/markdown; charset=utf-8"
            setHeader "Kiwi-Lines" $ TL.pack $ show from ++ "-" ++ show (from + length ls - 1)
            answer $ T.unlines ls
  case partM :: Maybe T.Text of
    Nothing -> send 1 (T.lines text)
    Just "outline" -> do
      db <- getRestrictedDB
      paths <- pathsFor
      let backlinks = sortOn (textOrder . K.pageTitle) $
                        DI.query (K.pagesIndex db) (U.qOnly K.FieldLink (K.pageUID p))
      secs <- pageSections p text offset total
      answer $ AT.outlineText paths p hash total backlinks secs
    Just "section" -> do
      titleM <- queryParamMaybe "title"
      case titleM of
        Nothing -> failWith Net.status400 "missing parameter: title"
        Just title -> do
          secs <- pageSections p text offset total
          let norm = T.toCaseFold . T.strip
          case filter ((== norm title) . norm . AT.sectionTitle) secs of
            s : _ -> uncurry send $ AT.linesRange (AT.sectionFrom s) (AT.sectionTo s) text
            [] -> failWith Net.status404 $ T.unlines $
                    ("no section " <> title <> ", the sections are:") :
                    map (("  " <>) . AT.sectionTitle) secs
    Just "lines" ->
      intParam "from" 1 $ \from -> intParam "to" total $ \to ->
        uncurry send $ AT.linesRange from to text
    Just other -> failWith Net.status400 $ "invalid part: " <> other
                    <> " (expected outline, section or lines)"

-- | Sections of the text of a page (see 'AT.sections'), its content parsed
-- again
pageSections :: K.Page -> T.Text -> Int -> Int -> ActM [AT.Section]
pageSections p text offset total = do
  db <- getDB
  return $ case splitPage (K.customMetaConfig db) text >>= \(_, content) ->
                  pageDoc (K.sourceFiles db) p { K.pageBody = content } of
    Left _ -> []
    Right doc -> AT.sections doc offset total

-- | GET /api/page/SOURCE/UID/meta: the header of the page (see
-- 'withApiPage'), with the hash of its file as ETag
apiPageMeta :: T.Text -> T.Text -> ActM ()
apiPageMeta source uid = withApiPage source uid $ \_ (text, _, offset) ->
  answer $ T.unlines $ take (offset - 1) $ T.lines text

-- | GET /api/folders/SOURCE, for the admins: the folders of the source that
-- have pages, archives included (see 'AT.foldersText')
apiFolders :: T.Text -> ActM ()
apiFolders source = do
  admin <- Auth.isAdmin
  if not admin then failWith Net.status403 "the folders are only listed for the admins"
  else do
    db <- getDB
    cd <- asksK K.contentDir
    let top = cd FP.</> T.unpack source
        pages = DI.documentsList $ DI.narrow (K.pagesIndex db) (U.qOnly K.FieldSource source)
    if null pages then failWith Net.status404 $ "no page in source " <> source
    else answer $ AT.foldersText source
                    [ FP.makeRelative top (K.pageAbsoluteFSPath p) | p <- pages ]

-- | GET /api/guide: the guide of kiwi for the agents, then the one of the
-- user (the content of the page of agent-guide in kiwi.ini), when the user
-- can read it
apiGuide :: ActM ()
apiGuide = do
  guideM <- asksK K.agentGuide
  db <- getDB
  loginM <- asksK (K.login . K.requestState)
  let kiwiGuide = TE.decodeUtf8Lenient DP.agentGuidePage
      userGuide = do
        uid <- guideM
        p <- findPage db uid
        _ <- Auth.pageAccessLevel loginM p
        return p
  answer $ case userGuide of
    Nothing -> kiwiGuide
    Just p -> T.concat [ kiwiGuide, "\n\n# Guide of the user: ", K.pageTitle p, "\n\n"
                       , K.pageBody p ]


-- | GET /api/search?q=QUERY: full-text search for an agent (see
-- "Kiwi.Api.Search"), in the pages of the context ("source", "archive=yes"),
-- at most "limit" pages (10 by default). The lang of the query is the one of
-- the sources, unless it starts with a lang code ("fr:"), as on the search
-- page.
apiSearch :: ActM ()
apiSearch = do
  qM <- queryParamMaybe "q"
  db <- getRestrictedDB
  allDB <- getDB
  ctx <- asksK (K.context . K.requestState)
  let sources = K.contextSources ctx
      allSources = [ s | K.KeyText s <- DI.getFieldKeys (K.pagesIndex allDB) K.FieldSource ]
      ctxLang = searchLang (fromMaybe K.defaultLangSetting $ K.metaLang $ K.defaultMeta allDB)
                           (K.sourcesConfig allDB) allSources sources
      visible uid = S.null sources || S.member (fst uid) sources
  intParam "limit" 10 $ \limit -> case qM of
    Nothing -> failWith Net.status400 "missing parameter: q"
    Just raw -> do
      let (lang, text) = queryLang ctxLang raw
          q = AS.parseQuery text
          termsOf ws = [ (t, w) | w <- ws, t <- extractTerms (alg lang) w ]
          terms = termsOf (AS.queryWords q)
          excludedTerms = map fst $ termsOf (AS.queryExcluded q)
          excluded = S.fromList [ uid | (uid, _, _) <- explainPages (K.searchEngine db) excludedTerms ]
          -- the fields are only computed for the pages shown (lazily)
          hits = [ AS.Hit p score found (termFields p found)
                 | (uid, score, found) <- explainPages (K.searchEngine db) (map fst terms)
                 , visible uid, not (S.member uid excluded)
                 , Just p <- [findPage db uid]
                 , all (AS.hasPhrase p) (AS.queryPhrases q) ]
          ranked = AS.rankHits (map fst terms) hits
          total = length (AS.rankedHits ranked)
      if null terms
        then failWith Net.status400 "the query has no word to search"
        else do
          results <- mapM (withPassages (map fst terms)) (take limit $ AS.rankedHits ranked)
          answer $ AS.searchText (T.strip raw) lang terms ranked total results
  where
    -- the passages of a page, read from its file, with the lines of the text
    -- of /api/page
    withPassages terms h = do
      let p = AS.hitPage h
      db <- getDB
      loginM <- asksK (K.login . K.requestState)
      fileE <- Scotty.liftIO $ Write.readPageFile (K.pageAbsoluteFSPath p)
      case (fileE, Auth.pageAccessLevel loginM p) of
        (Right (_, t), Just level)
          | Right (text, offset) <- Edit.editorText (K.customMetaConfig db) level t -> do
              let total = length (T.lines text)
              secs <- pageSections p text offset total
              return (h, AS.passages (K.pageLang p) terms text offset secs)
        _ -> return (h, [])


-- | Text of the body of a request, LF line endings, at most 'maxTextLength'
-- characters, or answers 400 or 413
withBodyText :: (T.Text -> ActM ()) -> ActM ()
withBodyText run = do
  b <- Scotty.body
  case TE.decodeUtf8' (BL.toStrict b) of
    Left _ -> failWith Net.status400 "the body is not valid UTF-8"
    Right t | T.length t > maxTextLength -> failWith Net.status413 "the text is too long"
            | otherwise -> run (T.replace "\r\n" "\n" t)

-- | Largest text of a request, in characters
maxTextLength :: Int
maxTextLength = 1000000

-- | The page of the request, when the user can write it, with their right;
-- 404 when the user can not read it, 403 when they can not write it
withWritablePage :: T.Text -> T.Text -> (K.Page -> K.AccessLevel -> ActM ()) -> ActM ()
withWritablePage source uid run = do
  db <- getDB
  loginM <- asksK (K.login . K.requestState)
  case findPage db (source, uid) of
    Just p | Just level <- Auth.pageAccessLevel loginM p ->
      if level >= K.AccessWrite then run p level
      else failWith Net.status403 $ "the account can not write the page " <> source <> "/" <> uid
    _ -> failWith Net.status404 $ "no page " <> source <> "/" <> uid

-- | Answer of a write: the page, the hash of its file (also as ETag), and
-- more lines
saved :: Net.Status -> T.Text -> K.PageUID -> T.Text -> [T.Text] -> ActM ()
saved st what (s, u) hash more = do
  setHeader "ETag" $ TL.fromStrict $ "\"" <> hash <> "\""
  status st
  answer $ T.unlines $ [ T.concat [what, " ", s, "/", u], "hash: " <> hash ] ++ more

-- | Answer of a write that failed
writeFailed :: Edit.SaveFailure -> ActM ()
writeFailed f = case f of
  Edit.SaveConflict -> failWith Net.status412
    "the page changed since it was read (its hash is not the one of If-Match): read it again"
  Edit.SaveNotApplied e -> failWith Net.status409 (T.pack e)
  Edit.SaveInvalid e -> failWith Net.status400 (T.pack e)
  Edit.SaveForbidden e -> failWith Net.status403 (T.pack e)
  Edit.SaveFailed e -> failWith Net.status500 (T.pack e)

-- | The user of the request
userName :: ActM T.Text
userName = maybe "" K.loginUser <$> asksK (K.login . K.requestState)

-- | PUT /api/page/SOURCE/UID: replaces the text of a page (header and
-- content, as given by GET, without uid) by the body, when its file still has
-- the hash of If-Match (412 otherwise, 428 without it). The keys that the
-- user can not change keep their value (see 'Edit.editedText'), and are
-- listed in the answer.
apiPutPage :: T.Text -> T.Text -> ActM ()
apiPutPage source uid = withWritablePage source uid $ \p level -> withBodyText $ \text -> do
  ifMatch <- Scotty.header "If-Match"
  case T.filter (/= '"') . TL.toStrict <$> ifMatch of
    Nothing -> failWith Net.status428
      "If-Match is required, with the hash of the page as read (its ETag)"
    Just hash -> do
      ss <- asksK id
      user <- userName
      r <- Scotty.liftIO $ Write.updatePage ss user p (Edit.sentText level hash text)
      case r of
        Left f -> writeFailed f
        Right (newHash, ignored) -> saved Net.status200 "saved" (source, uid) newHash
          [ "ignored keys (not allowed): " <> T.intercalate ", " ignored | not (null ignored) ]

-- | PATCH /api/page/SOURCE/UID: replaces exact passages of the text of the
-- page (as given by GET), with a JSON body {"edits": [{"old": "…", "new":
-- "…"}, …]} (see 'Edit.applyEdits'): all of them or none. The answer gives
-- the lines of the new passages.
apiPatchPage :: T.Text -> T.Text -> ActM ()
apiPatchPage source uid = withWritablePage source uid $ \p level -> do
  b <- Scotty.body
  case A.decode b >>= AesonT.parseMaybe editsParser of
    Nothing -> failWith Net.status400
      "expected a JSON body: {\"edits\": [{\"old\": \"exact text\", \"new\": \"text\"}]}"
    Just edits -> do
      ss <- asksK id
      user <- userName
      db <- getDB
      let edits' = [ (T.replace "\r\n" "\n" o, T.replace "\r\n" "\n" n) | (o, n) <- edits ]
      -- the lines of the new passages, from the text before the save (for
      -- the answer only)
      before <- Scotty.liftIO $ Write.readPageFile (K.pageAbsoluteFSPath p)
      let ranges = case before of
            Right (_, t) | Right (shown, _) <- Edit.editorText (K.customMetaConfig db) level t
                         , Right (_, rs) <- Edit.applyEdits edits' shown -> rs
            _ -> []
      r <- Scotty.liftIO $ Write.updatePage ss user p (Edit.editsChange level edits')
      case r of
        Left f -> writeFailed f
        Right (newHash, ignored) -> saved Net.status200 "saved" (source, uid) newHash $
          [ "lines: " <> T.intercalate ", " [ T.pack (show a ++ "-" ++ show z) | (a, z) <- ranges ]
          | not (null ranges) ] ++
          [ "ignored keys (not allowed): " <> T.intercalate ", " ignored | not (null ignored) ]
  where
    editsParser = A.withObject "body" $ \o -> do
      es <- o A..: "edits"
      mapM (A.withObject "edit" $ \e -> (,) <$> e A..: "old" <*> e A..: "new") es

-- | POST /api/pages/SOURCE: creates a page from the body (header and
-- content, as given by GET), with a new uid, at "path" (admins only, see
-- 'Write.createPage') or in a file named after its title. 201 with the uid
-- of the page.
apiCreatePage :: T.Text -> ActM ()
apiCreatePage source = withBodyText $ \text -> do
  pathM <- queryParamMaybe "path"
  ss <- asksK id
  loginM <- asksK (K.login . K.requestState)
  admin <- Auth.isAdmin
  r <- Scotty.liftIO $ Write.createPage ss loginM source (T.unpack <$> pathM) text
  case r of
    Left f -> writeFailed f
    Right (uid, path) -> do
      fileE <- Scotty.liftIO $ Write.readPageFile (contentPath ss uid path)
      let hash = either (const "") (Edit.fileHash . fst) fileE
      saved Net.status201 "created" uid hash [ "path: " <> T.pack path | admin ]
  where
    contentPath ss (s, _) path = K.contentDir ss FP.</> T.unpack s FP.</> path


-- | POST /api/metadata/SOURCE: applies the metadata script of the body to
-- the pages of the source (see "Kiwi.MetaScript" and 'Write.runMetaScript'):
-- all of its commands, or none. The answer gives the number of pages
-- changed by each command.
apiMetadata :: T.Text -> ActM ()
apiMetadata source = withBodyText $ \script ->
  case MS.parseScript script of
    Left err -> failWith Net.status400 (T.pack err)
    Right [] -> failWith Net.status400 "the script has no command"
    Right cmds -> do
      ss <- asksK id
      loginM <- asksK (K.login . K.requestState)
      r <- Scotty.liftIO $ Write.runMetaScript ss loginM source cmds
      case r of
        Left f -> writeFailed f
        Right counts -> answer $ T.unlines $ MS.resultLines source counts
