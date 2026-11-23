-- | The API of kiwi, for the agents (language models) and the scripts:
-- routes under /api/, answers in compact text (see "Kiwi.Api.Text"). A
-- request is authenticated by an API token only (Authorization: Bearer,
-- see 'Auth.tokenLogin'), never by the session cookie, and gets the rights
-- of the account of the token.
module Kiwi.Api
  ( withToken
  , apiPages
  , apiFacets
  , apiPage
  , apiPageMeta
  , apiGuide
  -- , guideText  -- used by the start warning of the long guides, commented out
  , apiSearch
  , apiPutPage
  , apiPatchPage
  , apiCreatePage
  , apiMetadata
  , serveMcp
  ) where

import           Control.Exception (IOException, try)
import           Control.Monad.Reader (local)
import           Data.IORef (readIORef)
import           Data.List (nub, sortOn)
import           Data.Maybe (fromMaybe)
import qualified Data.Map.Strict as M
import           Data.Ord (Down (..))
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as AesonT
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as TL
import qualified Data.Time as Time
import qualified Network.Wai as Wai
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
import           Kiwi.Keywords (keywordsText)
import qualified Kiwi.Log as L
import qualified Kiwi.Mcp as Mcp
import qualified Kiwi.MetaScript as MS
import           Kiwi.Controller (findPage, getRestrictedDB, relatedOf, keyOrder, sameOrigin, textOrder,
                                  withContext, withLogin)
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
-- 'withContext'), or answers 401. Without token, a GET request of an admin
-- logged in by the web (session cookie) is accepted too, to read the API in
-- a browser; the writes need a token (no cross-site request with a cookie).
withToken :: ActM () -> ActM ()
withToken run = do
  auth <- Scotty.header "Authorization"
  method <- Wai.requestMethod <$> Scotty.request
  case T.strip <$> (T.stripPrefix "Bearer " . TL.toStrict =<< auth) of
    Nothing | method == "GET" -> withLogin $ do
      admin <- Auth.isAdmin
      if admin then withContext $ setHeader "Cache-Control" "no-store" >> run
        else unauthorized "an API token is required (Authorization: Bearer TOKEN)"
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


-- | GET /api/pages: the pages selected by the parameters (see
-- 'withSelection'), the most recently modified first, at most "limit" of
-- them (100 by default): SOURCE/UID alone, with the fields of "show" under
-- each one (see 'AT.pagesText'). The fields "links" (the uids of the pages
-- it links, with "(missing)" for a page that does not exist or can not be
-- read) and "backlinks" (the uids of the readable pages that link it) are
-- in the same source as the page; "keywords": see "Kiwi.Keywords";
-- "related": SOURCE/UID of the related pages (see "Kiwi.Related"), the
-- closest first.
apiPages :: ActM ()
apiPages = withSelection ["limit", "show"] $ \db pages ->
  withShow (AT.pageFields ++ map K.cmcName (K.customMetaConfig db)) $ \shown ->
    intParam "limit" 100 $ \limit -> do
      let kws = shownKeywords db shown
      related <- relatedOf db
      answer $ AT.pagesText (extraField db kws (related S.empty)) shown (length pages)
                            (take limit $ sortOn (Down . K.pageMTime) pages)

-- | The keywords of the pages when they are shown (see 'K.pageKeywords'),
-- none otherwise
shownKeywords :: K.PagesDB -> [T.Text] -> M.Map K.PageUID [K.Keyword]
shownKeywords db shown
  | "keywords" `elem` shown = K.kwPages $ K.pageKeywords db
  | otherwise = M.empty

-- | The fields of a page that the index gives (see 'apiPages'): "links",
-- "backlinks", "keywords" (see "Kiwi.Keywords", 'shownKeywords') and
-- "related" (given the related pages of a page)
extraField :: K.PagesDB -> M.Map K.PageUID [K.Keyword] -> (K.Page -> [K.Page]) -> K.Page -> T.Text
           -> Maybe T.Text
extraField db kws related p f =
      let idx = K.pagesIndex db
          exists uid = maybe False (const True) $ DI.findDoc idx uid
          list vs = if null vs then Nothing else Just (T.intercalate ", " vs)
      in case f of
           "links" -> list [ snd l <> if exists l then "" else " (missing)"
                           | l <- U.nodup (K.pageLinks p) ]
           "backlinks" -> list $ sortOn textOrder
                            [ snd (K.pageUID b) | b <- DI.query idx (U.qOnly K.FieldLink (K.pageUID p)) ]
           "keywords" -> keywordsText kws p
           "related" -> list [ src <> "/" <> uid | (src, uid) <- map K.pageUID (related p) ]
           _ -> Nothing

-- | The fields of the "show" parameters (repeated, or separated by commas),
-- all of them among the given ones (400 otherwise); none without them
withShow :: [T.Text] -> ([T.Text] -> ActM ()) -> ActM ()
withShow known run = do
  params <- queryParams
  let shown = nub [ f | ("show", v) <- params, f <- map T.strip (T.splitOn "," v), not (T.null f) ]
  case filter (`notElem` known) shown of
    f : _ -> failWith Net.status400 $ T.concat [ "unknown field for show: ", f, "; known: "
                                                , T.intercalate ", " known ]
    [] -> run shown

-- | The pages that the user can read matching the metadata of the parameters
-- (as on /browse: "tag" (all of them), "lang", "m.FIELD", "has", "access"
-- for the admins; "source" and "archive=yes" for the context),
-- "since=YYYY-MM-DD" and "page=SOURCE/UID" (one of them), the parameters
-- given excepted, with the
-- restricted database; 400 for a wrong parameter
withSelection :: [T.Text] -> (K.PagesDB -> [K.Page] -> ActM ()) -> ActM ()
withSelection others run = do
  db <- getRestrictedDB
  ctx <- asksK (K.context . K.requestState)
  params <- queryParams
  admin <- Auth.isAdmin
  let filters = [ (k, v) | (k, v) <- Url.withoutContextParams params
                         , k `notElem` ("since" : "page" : others) ]
      pageParams = [ v | ("page", v) <- params ]
  sinceM <- queryParamMaybe "since"
  case (Browse.decodeReq (K.customMetaConfig db) filters, traverse readDay sinceM, traverse pageRef pageParams) of
    (Left err, _, _) -> failWith Net.status400 err
    (_, Nothing, _) -> failWith Net.status400 "invalid since, expected YYYY-MM-DD"
    (_, _, Nothing) -> failWith Net.status400 "invalid page, expected SOURCE/UID"
    (Right req, _, _) | not admin && M.member K.FieldAccess req ->
                       failWith Net.status400 "unknown parameter: access"
    (Right req, Just since, Just uids) -> do
      let req' = Browse.sourcesReq (K.contextSources ctx) <> req
          idx = K.pagesIndex db
          selected = if M.null req' then DI.documentsList idx
                     else DI.documentsList $ DI.narrow idx $ Browse.reqToQuery req'
          keep p = maybe True (\d -> Time.utctDay (K.pageMTime p) >= d) since
                   && (null uids || K.pageUID p `elem` uids)
      run db $ filter keep selected
  where
    readDay :: T.Text -> Maybe Time.Day
    readDay = Time.parseTimeM False Time.defaultTimeLocale "%Y-%-m-%-d" . T.unpack
    pageRef v = case T.breakOnEnd "/" v of
      (s, u) | T.length s > 1 && not (T.null u) -> Just (T.dropEnd 1 s, u)
      _ -> Nothing

-- | GET /api/facets: for each source of the pages selected by the
-- parameters of /api/pages (see 'withSelection'; all the pages that the user
-- can read without them), the tags, langs, folders (for the admins: where
-- to create a page) and custom fields of these pages, with the number of
-- pages of each value, or only those of "show" (tags, lang, folder, custom
-- fields): the agent narrows its selection step by step, as a person on
-- /browse
apiFacets :: ActM ()
apiFacets = withSelection ["show"] $ \db pages -> do
 admin <- Auth.isAdmin
 cd <- asksK K.contentDir
 withShow (["tags", "lang"] ++ [ "folder" | admin ] ++ map K.cmcName (K.customMetaConfig db)) $ \shown -> do
  let wants f = null shown || f `elem` shown
      idx = DI.narrowOnSet (K.pagesIndex db) (S.fromList $ map K.pageUID pages)
      sources = [ s | K.KeyText s <- DI.getFieldKeys idx K.FieldSource ]
      facets s =
        let sub = DI.narrow idx (U.qOnly K.FieldSource s)
            count f k = S.size $ DI.lookupIndex sub f k
            tags = sortOn (map (\seg -> (textOrder seg, seg)) . Tags.tagToSegments . fst)
                     [ (t, count K.FieldTag k) | k@(K.KeyText t) <- DI.getFieldKeys sub K.FieldTag ]
            langs = [ (Browse.showKeyLabel k, count K.FieldLang k)
                    | k <- DI.getFieldKeys sub K.FieldLang ]
            folders = AT.folderCounts
                        [ FP.makeRelative (cd FP.</> T.unpack s) (K.pageAbsoluteFSPath p)
                        | p <- DI.documentsList sub ]
            customs = [ (K.cmcName c, typeName (K.cmcType c) <> maybe "" ("; " <>) (K.cmcDescription c),
                         [ (Browse.showKeyLabel k, count f k)
                         | k <- sortOn keyOrder (DI.getFieldKeys sub f) ])
                      | c <- sortOn (textOrder . K.cmcName) (K.customMetaConfig db)
                      , wants (K.cmcName c)
                      , let f = K.FieldCustom (K.cmcName c)
                      , not (null $ DI.getFieldKeys sub f) ]
        in AT.facetsText s (DI.cardinal sub) (if wants "tags" then Just tags else Nothing)
                         (if wants "lang" then Just langs else Nothing)
                         (if admin && wants "folder" then Just folders else Nothing) customs
  if null pages
    then answer "no page\n"
    else answer $ T.intercalate "\n" $ map facets $ sortOn textOrder sources
  where
    typeName t = case t of
      K.CmtText -> "text"
      K.CmtInt  -> "int"
      K.CmtDate -> "date"
      K.CmtBool -> "bool"

-- Replaced by the fields links and backlinks of /api/pages
-- -- | GET /api/links: the links between pages, for the pages selected by the
-- -- parameters of /api/pages (see 'withSelection'): by source, one link by
-- -- line, from the uid of the page to the uid of the page it links (see
-- -- 'AT.linksText')
-- apiLinks :: ActM ()
-- apiLinks = withSelection [] $ \db pages -> do
--   let bySource = M.fromListWith (++) [ (fst (K.pageUID p), [p]) | p <- pages ]
--       exists uid = maybe False (const True) $ DI.findDoc (K.pagesIndex db) uid
--   if null pages
--     then answer "no page\n"
--     else answer $ T.intercalate "\n"
--            [ AT.linksText s [ (snd (K.pageUID p), snd l, exists l)
--                             | p <- sortOn (snd . K.pageUID) ps, l <- U.nodup (K.pageLinks p) ]
--            | (s, ps) <- sortOn (textOrder . fst) (M.toList bySource) ]

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
-- "part=whole" the whole text, whatever its size.
-- "numbered=yes" gives the lines after their number (not for the outline).
-- The lines are those of the content, from its first line: the header is
-- not numbered, nor in the parts. A page longer than 'maxWholePage'
-- characters is not given whole without part: its outline instead, after a
-- line that gives its size and says how to read it.
apiPage :: T.Text -> T.Text -> ActM ()
apiPage source uid = withApiPage source uid $ \p (text, _, offset) -> do
  partM <- queryParamMaybe "part"
  numberedM <- queryParamMaybe "numbered"
  let numbered = numberedM == Just ("yes" :: T.Text)
      (headerLines, contentLines) = splitAt offset (T.lines text)
      content = T.unlines contentLines
      total = length contentLines
      send from ls
        | numbered = answer $ AT.numberedLines from ls
        | otherwise = do
            setHeader "Content-Type" "text/markdown; charset=utf-8"
            setHeader "Kiwi-Lines" $ TL.pack $ show from ++ "-" ++ show (from + length ls - 1)
            answer $ T.unlines ls
      outline note = do
        db <- getRestrictedDB
        paths <- pathsFor
        let backlinks = sortOn (textOrder . K.pageTitle) $
                          DI.query (K.pagesIndex db) (U.qOnly K.FieldLink (K.pageUID p))
        secs <- pageSections p content
        answer $ note <> AT.outlineText paths p backlinks secs
      whole
        | numbered = answer $ T.unlines (map (AT.unnumbered <>) headerLines)
                              <> AT.numberedLines 1 contentLines
        | otherwise = do
            setHeader "Content-Type" "text/markdown; charset=utf-8"
            answer text
  case partM :: Maybe T.Text of
    Nothing
      | T.length text > maxWholePage -> outline $ T.concat
          [ "this page has ", T.pack (show $ T.length text), " characters, more than "
          , T.pack (show maxWholePage), ": its outline below. Read a section (part section), "
          , "lines (part lines, from 1 to ", T.pack (show total), "), or the whole page (part whole).\n" ]
      | otherwise -> whole
    Just "whole" -> whole
    Just "outline" -> outline ""
    Just "section" -> do
      titleM <- queryParamMaybe "title"
      case titleM of
        Nothing -> failWith Net.status400 "missing parameter: title"
        Just title -> do
          secs <- pageSections p content
          let norm = T.toCaseFold . T.strip
          case filter ((== norm title) . norm . AT.sectionTitle) secs of
            s : _ -> uncurry send $ AT.linesRange (AT.sectionFrom s) (AT.sectionTo s) content
            [] -> failWith Net.status404 $ T.unlines $
                    ("no section " <> title <> ", the sections are:") :
                    map (("  " <>) . AT.sectionTitle) secs
    Just "lines" ->
      intParam "from" 1 $ \from -> intParam "to" total $ \to ->
        uncurry send $ AT.linesRange from to content
    Just other -> failWith Net.status400 $ "invalid part: " <> other
                    <> " (expected whole, outline, section or lines)"

-- | Largest page given whole by GET /api/page without part, in characters
-- (about 10000 tokens, where Claude Code warns about the size of an answer)
maxWholePage :: Int
maxWholePage = 40000

-- | Sections of the content of a page (see 'AT.sections'), parsed again
pageSections :: K.Page -> T.Text -> ActM [AT.Section]
pageSections p content = do
  db <- getDB
  return $ case pageDoc (K.sourceFiles db) p { K.pageBody = content } of
    Left _ -> []
    Right doc -> AT.sections doc (length $ T.lines content)

-- | GET /api/page/SOURCE/UID/meta: the header of the page (see
-- 'withApiPage'), with the hash of its file as ETag
apiPageMeta :: T.Text -> T.Text -> ActM ()
apiPageMeta source uid = withApiPage source uid $ \_ (text, _, offset) ->
  answer $ T.unlines $ take (offset - 1) $ T.lines text

-- Replaced by the facet folder of /api/facets, for the admins
-- -- | GET /api/folders/SOURCE, for the admins: the folders of the source that
-- -- have pages, archives included (see 'AT.foldersText')
-- apiFolders :: T.Text -> ActM ()
-- apiFolders source = do
--   admin <- Auth.isAdmin
--   if not admin then failWith Net.status403 "the folders are only listed for the admins"
--   else do
--     db <- getDB
--     cd <- asksK K.contentDir
--     let top = cd FP.</> T.unpack source
--         pages = DI.documentsList $ DI.narrow (K.pagesIndex db) (U.qOnly K.FieldSource source)
--     if null pages then failWith Net.status404 $ "no page in source " <> source
--     else answer $ AT.foldersText source
--                     [ FP.makeRelative top (K.pageAbsoluteFSPath p) | p <- pages ]

-- | GET /api/guide: the documentation for the agents, by "topic":
--
-- * "wiki" (without topic): the documentation of the API, then the guide of
--   the user (the file of agent-guide in kiwi.ini, read again at each
--   request), if any; when this file can not be read, the documentation
--   alone (the error is logged); see 'guideText';
-- * "metadata-scripts": the syntax of the metadata scripts (the same text
--   as on the metadata editor).
apiGuide :: ActM ()
apiGuide = do
  topicM <- queryParamMaybe "topic"
  case topicM :: Maybe T.Text of
    Nothing -> wiki
    Just "wiki" -> wiki
    Just "metadata-scripts" -> answer $ TE.decodeUtf8Lenient DP.metadataScriptPage
    Just other -> failWith Net.status400 $ "unknown topic: " <> other
                    <> " (expected wiki or metadata-scripts)"
  where
    wiki = do
      guideM <- asksK K.agentGuide
      logger' <- asksK K.logger
      userGuide <- Scotty.liftIO $ case guideM of
        Nothing -> return Nothing
        Just p -> try (BS.readFile p) >>= \r -> case r of
          Right bytes -> return $ Just (TE.decodeUtf8Lenient bytes)
          Left e -> do
            logger' L.Error $ T.pack $ concat
              [ "unable to read the agent guide file ", p, ": ", show (e :: IOException) ]
            return Nothing
      answer $ guideText (TE.decodeUtf8Lenient DP.agentApiDocPage) userGuide

-- | The guide of the agents: the documentation of the API, then the guide
-- of the user, if any, under its own title
guideText :: T.Text -> Maybe T.Text -> T.Text
guideText apiDoc = maybe apiDoc (\g -> T.concat [apiDoc, "\n\n# Guide of the user\n\n", g])


-- | GET /api/search?q=QUERY: full-text search for an agent (see
-- "Kiwi.Api.Search"), in the pages selected by the parameters of /api/pages
-- (see 'withSelection': context, tags, fields…), at most "limit" pages (10
-- by default); the first line names the words that no page selected
-- contains. The lang of the query is the one of the sources, unless it
-- starts with a lang code ("fr:"), as on the search page. "show=keywords"
-- adds the keywords of each page (see "Kiwi.Keywords").
apiSearch :: ActM ()
apiSearch = withSelection ["q", "limit", "show"] $ \db selected -> withShow ["keywords"] $ \shown -> do
  qM <- queryParamMaybe "q"
  allDB <- getDB
  ctx <- asksK (K.context . K.requestState)
  let sources = K.contextSources ctx
      allSources = [ s | K.KeyText s <- DI.getFieldKeys (K.pagesIndex allDB) K.FieldSource ]
      ctxLang = searchLang (fromMaybe K.defaultLangSetting $ K.metaLang $ K.defaultMeta allDB)
                           (K.sourcesConfig allDB) allSources sources
      inSelection = S.fromList (map K.pageUID selected)
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
          -- the union of the terms: the pages with more terms first, then
          -- by score (with few pages, the IDF of a term in half of them or
          -- more is 0 or less, so the score alone could put a page with
          -- fewer terms first)
          candidates = [ c | c@(uid, _, _) <- explainPages (K.searchEngine db) (map fst terms)
                           , S.member uid inSelection ]
          -- the words of the query whose terms no page selected contains
          foundTerms = S.fromList (concat [ found | (_, _, found) <- candidates ])
          missing = [ w | w <- nub (map snd terms)
                        , not (any (`S.member` foundTerms) [ t | (t, w') <- terms, w' == w ]) ]
          hits = sortOn (\h -> (Down (length $ AS.hitTerms h), Down (AS.hitScore h)))
                 [ AS.Hit p score found (termFields p found)
                 | (uid, score, found) <- candidates, not (S.member uid excluded)
                 , Just p <- [findPage db uid]
                 , all (AS.hasPhrase p) (AS.queryPhrases q) ]
          total = length hits
      if null terms
        then failWith Net.status400 "the query has no word to search"
        else do
          results <- mapM (withPassages (map fst terms)) (take limit hits)
          let kws = shownKeywords db shown
          let fields p = [ (f, v) | f <- shown, Just v <- [extraField db kws (const []) p f] ]
          answer $ AS.searchText (T.strip raw) lang terms missing fields total results
  where
    -- the passages of a page, from its content as loaded, with the lines of
    -- its content
    withPassages terms h = do
      let p = AS.hitPage h
      secs <- pageSections p (K.pageBody p)
      return (h, AS.passages (K.pageLang p) terms (K.pageBody p) secs)


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
      messageM <- queryParamMaybe "message"
      r <- Scotty.liftIO $ Write.updatePage ss user messageM p (Edit.sentText level hash text)
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
      -- the lines of the new passages in the content ("header" for those
      -- of the header), from the text before the save (for the answer only)
      before <- Scotty.liftIO $ Write.readPageFile (K.pageAbsoluteFSPath p)
      let ranges = case before of
            Right (_, t) | Right (shown, _) <- Edit.editorText (K.customMetaConfig db) level t
                         , Right (new, rs) <- Edit.applyEdits edits' shown ->
                             let off = AT.headerLength new
                             in nub [ if z <= off then "header"
                                           else T.pack (show (max 1 (a - off)) ++ "-" ++ show (z - off))
                                         | (a, z) <- rs ]
            _ -> []
      messageM <- queryParamMaybe "message"
      r <- Scotty.liftIO $ Write.updatePage ss user messageM p (Edit.editsChange level edits')
      case r of
        Left f -> writeFailed f
        Right (newHash, ignored) -> saved Net.status200 "saved" (source, uid) newHash $
          [ "lines: " <> T.intercalate ", " ranges | not (null ranges) ] ++
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
  messageM <- queryParamMaybe "message"
  ss <- asksK id
  loginM <- asksK (K.login . K.requestState)
  admin <- Auth.isAdmin
  r <- Scotty.liftIO $ Write.createPage ss loginM messageM source (T.unpack <$> pathM) text
  case r of
    Left f -> writeFailed f
    Right (uid, path) -> do
      fileE <- Scotty.liftIO $ Write.readPageFile (contentPath ss uid path)
      let hash = either (const "") (Edit.fileHash . fst) fileE
          content = drop (AT.headerLength text) (T.lines text)
      saved Net.status201 "created" uid hash $
        [ "path: " <> T.pack path | admin ] ++
        [ "content_size: " <> T.pack (show $ T.length $ T.unlines content)
        , "content_lines: " <> T.pack (show $ length content) ]
  where
    contentPath ss (s, _) path = K.contentDir ss FP.</> T.unpack s FP.</> path


-- | POST /api/metadata: applies the metadata script of the body (see
-- "Kiwi.MetaScript" and 'Write.runMetaScript'): all of its commands, or
-- none; with "check=yes", nothing is written. The answer gives the number of
-- pages changed by each command (and these pages, when checked).
apiMetadata :: ActM ()
apiMetadata = withBodyText $ \script -> do
  checkM <- queryParamMaybe "check"
  messageM <- queryParamMaybe "message"
  let check = checkM == Just ("yes" :: T.Text)
  case MS.parseScript script of
    Left err -> failWith Net.status400 (T.pack err)
    Right [] -> failWith Net.status400 "the script has no command"
    Right cmds -> do
      ss <- asksK id
      loginM <- asksK (K.login . K.requestState)
      r <- Scotty.liftIO $ Write.runMetaScript ss loginM messageM check cmds
      case r of
        Left f -> writeFailed f
        Right done -> answer $ T.unlines $ Write.metaScriptResult check done

-- Replaced by GET /api/guide?topic=metadata-scripts
-- -- | GET /api/metadata-guide: the syntax of the metadata scripts (the same
-- -- text as on the metadata editor)
-- apiMetadataGuide :: ActM ()
-- apiMetadataGuide = answer $ TE.decodeUtf8Lenient DP.metadataScriptPage


-- | POST /mcp: a message of the Model Context Protocol (see "Kiwi.Mcp"),
-- its tools calling the API in memory (the application given), with the
-- token of the request. 403 for the Origin of another host (DNS rebinding),
-- 400 for a version of the protocol that is not known (a client of a newer
-- version then falls back to initialize), 202 without answer (a
-- notification).
serveMcp :: Wai.Application -> ActM ()
serveMcp app = do
  same <- sameOrigin
  versionM <- Scotty.header "MCP-Protocol-Version"
  case versionM of
    _ | not same -> failWith Net.status403 "forbidden origin"
    Just v | TL.toStrict v `notElem` Mcp.protocolVersions ->
      failWith Net.status400 $ "unsupported protocol version: " <> TL.toStrict v
    _ -> withBodyText $ \msg -> do
      req <- Scotty.request
      answer' <- Scotty.liftIO $
        Mcp.handleMessage (Mcp.appBackend app req) (BL.fromStrict $ TE.encodeUtf8 msg)
      maybe (status Net.status202) Scotty.json answer'
