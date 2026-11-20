-- | The Model Context Protocol of the agents (Claude Code, Claude Desktop…),
-- served by kiwi on POST /mcp (see 'Kiwi.Api.serveMcp'): each tool makes
-- one request to the API (see "Kiwi.Api"), in memory, and gives its text
-- answer as it is. The API is the only implementation: the MCP only
-- translates.
--
-- The protocol: JSON-RPC 2.0, one message by POST, the answer in JSON
-- (neither SSE nor session). Requests answered: initialize (with the guide
-- of the API as instructions), ping, tools/list, tools/call; the
-- notifications are ignored.
module Kiwi.Mcp
  ( handleMessage
  , appBackend
  , protocolVersions
  , Backend        -- exported for tests
  , ApiRequest (..)  -- exported for tests
  ) where

import           Data.Aeson ((.=), (.:), (.:?))
import qualified Data.Aeson as A
import qualified Data.Aeson.Key as AK
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Aeson.Types as AT
import qualified Data.ByteString.Lazy as BL
import qualified Data.CaseInsensitive as CI
import           Data.Maybe (fromMaybe, mapMaybe)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Network.HTTP.Types as Net
import qualified Network.Wai as Wai

import           Paths_kiwi (version)
import           Data.Version (showVersion)
import           Utils.WaiCall (callApp, localRequest)


-- | A request to the API: method, path, query parameters, headers and body
data ApiRequest = ApiRequest { apiMethod :: T.Text
                             , apiPath :: T.Text
                             , apiParams :: [(T.Text, T.Text)]
                             , apiHeaders :: [(T.Text, T.Text)]
                             , apiBody :: T.Text }
  deriving (Eq, Show)

-- | A GET request without body
apiGet :: T.Text -> [(T.Text, T.Text)] -> ApiRequest
apiGet path params = ApiRequest "GET" path params [] ""

-- | A request to the API, answered with its status, headers (names in lower
-- case) and text
type Backend = ApiRequest -> IO (Int, [(T.Text, T.Text)], T.Text)

-- | The API as an application called in memory, with the token (the
-- Authorization header) and the address of the client of a request to
-- /mcp
appBackend :: Wai.Application -> Wai.Request -> Backend
appBackend app mcpReq ar = do
  let req = (localRequest (TE.encodeUtf8 $ apiMethod ar) (TE.encodeUtf8 $ apiPath ar)
                          [ (TE.encodeUtf8 k, Just $ TE.encodeUtf8 v) | (k, v) <- apiParams ar ]
                          (filter ((== "Authorization") . fst) (Wai.requestHeaders mcpReq) ++
                           [ (CI.mk (TE.encodeUtf8 k), TE.encodeUtf8 v) | (k, v) <- apiHeaders ar ]))
              { Wai.remoteHost = Wai.remoteHost mcpReq }
  (st, headers, body) <- callApp app req (BL.fromStrict $ TE.encodeUtf8 $ apiBody ar)
  return ( Net.statusCode st
         , [ (TE.decodeUtf8Lenient (CI.foldedCase k), TE.decodeUtf8Lenient v) | (k, v) <- headers ]
         , TE.decodeUtf8Lenient (BL.toStrict body) )

-- | Answer of a message (none for a notification or a response)
handleMessage :: Backend -> BL.ByteString -> IO (Maybe A.Value)
handleMessage backend msg = case A.decode msg of
  Nothing -> return $ Just $ rpcError A.Null (-32700) "parse error"
  Just (A.Object o) -> case (KM.lookup "method" o, KM.lookup "id" o) of
    (Just (A.String method), Just rid) -> do
      let params = fromMaybe (A.object []) (KM.lookup "params" o)
      r <- request backend method params
      return $ Just $ either (\(c, m) -> rpcError rid c m) (rpcResult rid) r
    -- notifications, and responses of the client
    _ -> return Nothing
  -- a batch (an array) too: there are none since the version 2025-06-18
  Just _ -> return $ Just $ rpcError A.Null (-32600) "invalid request"

rpcResult :: A.Value -> A.Value -> A.Value
rpcResult rid result = A.object ["jsonrpc" .= ("2.0" :: T.Text), "id" .= rid, "result" .= result]

rpcError :: A.Value -> Int -> T.Text -> A.Value
rpcError rid code message =
  A.object [ "jsonrpc" .= ("2.0" :: T.Text), "id" .= rid
           , "error" .= A.object ["code" .= code, "message" .= message] ]

-- | Versions of the protocol known by this server, the latest first: the
-- version asked by the client when known, else the latest one
protocolVersions :: [T.Text]
protocolVersions = [latestVersion, "2025-03-26", "2024-11-05"]

latestVersion :: T.Text
latestVersion = "2025-06-18"

-- | Result of a request, or the code and message of its error
request :: Backend -> T.Text -> A.Value -> IO (Either (Int, T.Text) A.Value)
request backend method params = case method of
  "initialize" -> do
    let asked = AT.parseMaybe (A.withObject "params" (.: "protocolVersion")) params
        v = case asked of
              Just a | a `elem` protocolVersions -> a
              _ -> latestVersion
    (st, _, guide) <- backend (apiGet "/api/guide" [])
    return $ Right $ A.object $
      [ "protocolVersion" .= v
      , "capabilities" .= A.object ["tools" .= A.object []]
      , "serverInfo" .= A.object [ "name" .= ("kiwi" :: T.Text)
                                 , "version" .= showVersion version ] ]
      ++ [ "instructions" .= guide | st == 200 ]
  "ping" -> return $ Right $ A.object []
  "tools/list" -> return $ Right $ A.object ["tools" .= map toolJson tools]
  "tools/call" -> case AT.parseMaybe callParser params of
    Nothing -> return $ Left (-32602, "invalid tools/call parameters")
    Just (name, args) -> case lookup name [ (toolName t, t) | t <- tools ] of
      Nothing -> return $ Left (-32602, "unknown tool: " <> name)
      Just t -> case toolRequest t args of
        Left err -> return $ Right $ toolResult True err
        Right req -> do
          (st, headers, body) <- backend req
          let ok = st >= 200 && st < 300
          return $ Right $ toolResult (not ok) $
            if ok then toolOutput t headers body
            else T.concat [ "error ", T.pack (show st), ": ", body ]
  _ -> return $ Left (-32601, "unknown method: " <> method)
  where
    callParser = A.withObject "params" $ \o ->
      (,) <$> o .: "name" <*> (fromMaybe KM.empty <$> o .:? "arguments")

toolResult :: Bool -> T.Text -> A.Value
toolResult isError txt = A.object
  [ "content" .= [A.object ["type" .= ("text" :: T.Text), "text" .= txt]]
  , "isError" .= isError ]


-- | A tool: its name, description, input schema (properties and required
-- ones), its request to the API from its arguments, and its output from the
-- answer of the API (headers and text)
data Tool = Tool { toolName :: T.Text
                 , toolDescription :: T.Text
                 , toolProperties :: [(T.Text, A.Value)]
                 , toolRequired :: [T.Text]
                 , toolRequest :: A.Object -> Either T.Text ApiRequest
                 , toolOutput :: [(T.Text, T.Text)] -> T.Text -> T.Text }

toolJson :: Tool -> A.Value
toolJson t = A.object
  [ "name" .= toolName t
  , "description" .= toolDescription t
  , "inputSchema" .= A.object [ "type" .= ("object" :: T.Text)
                              , "properties" .= A.object [ AK.fromText k .= v | (k, v) <- toolProperties t ]
                              , "required" .= toolRequired t ] ]

-- | JSON schemas of the arguments
prop :: T.Text -> T.Text -> A.Value
prop typ desc = A.object ["type" .= typ, "description" .= desc]

stringsProp :: T.Text -> A.Value
stringsProp desc = A.object [ "type" .= ("array" :: T.Text)
                            , "items" .= A.object ["type" .= ("string" :: T.Text)]
                            , "description" .= desc ]

sourcesProp :: (T.Text, A.Value)
sourcesProp = ("sources", stringsProp "Sources (folders of the wiki) to look in; all the readable ones without it.")

archiveProp :: (T.Text, A.Value)
archiveProp = ("archive", prop "boolean" "true to look in the archived pages instead of the others.")

-- | Arguments of a tool as query parameters
text :: A.Object -> T.Text -> Maybe T.Text
text o k = case KM.lookup (AK.fromText k) o of
  Just (A.String s) -> Just s
  Just (A.Number n) -> Just $ T.pack $ show (round n :: Integer)
  _ -> Nothing

-- | The strings of a list argument. Some clients send a list as a string
-- holding its JSON (["a","b"]): it is read as the list. A string alone is a
-- list of one string.
texts :: A.Object -> T.Text -> [T.Text]
texts o k = case KM.lookup (AK.fromText k) o of
  Just (A.Array vs) -> strings vs
  Just (A.String s)
    | Just (A.Array vs) <- A.decodeStrict (TE.encodeUtf8 s) -> strings vs
    | otherwise -> [s]
  _ -> []
  where strings vs = [ s | A.String s <- foldr (:) [] vs ]

-- | A boolean argument: true, or the string "true" (some clients send
-- every argument as a string)
flag :: A.Object -> T.Text -> Bool
flag o k = case KM.lookup (AK.fromText k) o of
  Just (A.Bool b) -> b
  Just (A.String s) -> T.toLower (T.strip s) == "true"
  _ -> False

contextParams :: A.Object -> [(T.Text, T.Text)]
contextParams o = [ ("source", s) | s <- texts o "sources" ] ++ [ ("archive", "yes") | flag o "archive" ]

optional :: A.Object -> [T.Text] -> [(T.Text, T.Text)]
optional o ks = mapMaybe (\k -> (,) k <$> text o k) ks

-- | A page reference SOURCE/UID
pagePath :: T.Text -> Either T.Text (T.Text, T.Text)
pagePath ref = case T.breakOn "/" (T.strip ref) of
  (s, u) | not (T.null s), Just uid <- T.stripPrefix "/" u, not (T.null uid), not ("/" `T.isInfixOf` uid)
           -> Right (s, uid)
  _ -> Left $ "invalid page " <> ref <> ", expected SOURCE/UID"

asIs :: [(T.Text, T.Text)] -> T.Text -> T.Text
asIs _ body = body

tools :: [Tool]
tools =
  [ Tool "search"
      "Full-text search in the wiki. Use it first to find where an information is, before \
      \reading pages. The words are alternatives: the pages containing any of them are found, \
      \best first (the more words, the better), so add synonyms freely. There is no OR nor \
      \AND operator (OR is searched as a word). Each result gives the page (SOURCE/UID), its \
      \title, tags, how many of the words it contains and where, its summary, and up to three \
      \passages with their line and section. Words are matched by their stem. \
      \\"exact phrase\" and -excluded words are allowed."
      [ ("query", prop "string" "The words to search, e.g. burns \"first aid\" -baby")
      , sourcesProp, archiveProp
      , ("limit", prop "integer" "Maximum number of pages (10 by default).") ]
      ["query"]
      (\o -> case text o "query" of
               Nothing -> Left "missing argument: query"
               Just q -> Right $ apiGet "/api/search" (("q", q) : contextParams o ++ optional o ["limit"]))
      asIs
  , Tool "facets"
      "Explore the wiki by its metadata, as a person narrowing a selection step by step, \
      \at a low cost: for the pages matching the filters (all the pages without filter), by \
      \source, their tags (with their parents, as written in the headers), langs and custom \
      \fields with their values, each with its number of pages. To see what the wiki holds on \
      \a subject, start without filter, add a tag or a field value at each call, then list the \
      \pages with list_pages once the selection is small. For a precise information, use \
      \search instead. Before writing a page, look at the facets to reuse the existing tags \
      \and values."
      (selectionProps ++
       [("show", stringsProp "Only these facets: tags, lang, or custom fields; all without it.")])
      [] (\o -> Right $ apiGet "/api/facets" (selectionParams o ++ showParam o))
      asIs
  , Tool "list_pages"
      "List the pages matching metadata, the most recently modified first: SOURCE/UID alone, \
      \and under each page the fields of show (title, tags, summary, lang, modified, \
      \custom fields), written as in a header. Narrow the selection with facets \
      \or search first. show [title, tags] gives an inventory; has_fields and show with a \
      \custom field prepare a metadata script."
      (selectionProps ++
       [ ("show", stringsProp "Fields given under each page: title, tags, summary, lang, modified (day), custom fields.")
       , ("limit", prop "integer" "Maximum number of pages (100 by default).") ])
      []
      (\o -> Right $ apiGet "/api/pages" (selectionParams o ++ showParam o ++ optional o ["limit"]))
      asIs
  , Tool "links"
      "The links between the pages matching the filters (all the pages without filter), by \
      \source: one link by line, UID -> UID, with (missing) when the linked page does not \
      \exist or can not be read. The pages that no page links are those absent on the right. \
      \The pages linking one page are in its outline (read_page, part outline)."
      selectionProps [] (Right . apiGet "/api/links" . selectionParams)
      asIs
  , Tool "read_page"
      "Read a page (SOURCE/UID): its header, then its content in CommonMark. For a long page, \
      \read its outline first (part: outline: tags, summary, backlinks, \
      \sections with their lines), then only the section or the lines you need. Line numbers \
      \count from the first line of the content: the header is not numbered (read it whole, \
      \or with list_pages and show). The hash identifies the version of the file."
      [ ("page", prop "string" "The page, as SOURCE/UID.")
      , ("part", A.object [ "type" .= ("string" :: T.Text)
                          , "enum" .= (["outline", "section", "lines"] :: [T.Text])
                          , "description" .= ("What to read; the whole page without it." :: T.Text) ])
      , ("title", prop "string" "With part section: the title of the section.")
      , ("from", prop "integer" "With part lines: the first line.")
      , ("to", prop "integer" "With part lines: the last line.")
      , ("numbered", prop "boolean" "true to number the lines.") ]
      ["page"]
      (\o -> do
          (s, u) <- maybe (Left "missing argument: page") pagePath (text o "page")
          return $ apiGet (T.concat ["/api/page/", s, "/", u])
                          ( optional o ["part", "title", "from", "to"]
                            ++ [ ("numbered", "yes") | flag o "numbered" ] ))
      (\headers body ->
         let etag = T.filter (/= '"') <$> lookupCI "etag" headers
             range = lookupCI "kiwi-lines" headers
         in T.concat [ maybe "" (\h -> "hash: " <> h <> "\n") etag
                     , maybe "" (\r -> "lines: " <> r <> "\n") range
                     , if isJust' etag || isJust' range then "---\n" else ""
                     , body ])
  , Tool "folders"
      "The folders of a source with their number of pages (admins only), to choose where a \
      \new page goes."
      [("source", prop "string" "The source.")]
      ["source"]
      (\o -> maybe (Left "missing argument: source") (\s -> Right $ apiGet ("/api/folders/" <> s) [])
                   (text o "source"))
      asIs
  , Tool "edit_page"
      "Change a page by replacing exact passages of its text (as read_page gives it, without \
      \line numbers): each old text must be found exactly once in the page, copied exactly, \
      \with enough of the text around it to be unique; it is replaced by the new text. To \
      \insert, give a line as old text, and this line followed by the new lines as new text. \
      \All the edits apply, or none. The rest of the page is kept, even if it changed since \
      \you read it. Prefer it to write_page. The header can be changed the same way."
      [ ("page", prop "string" "The page, as SOURCE/UID.")
      , ("edits", A.object
          [ "type" .= ("array" :: T.Text)
          , "items" .= A.object
              [ "type" .= ("object" :: T.Text)
              , "properties" .= A.object [ "old" .= prop "string" "Exact text of the page to replace."
                                         , "new" .= prop "string" "Its replacement (empty to delete it)." ]
              , "required" .= (["old", "new"] :: [T.Text]) ]
          , "description" .= ("The replacements, in their order." :: T.Text) ])
      , messageProp ]
      ["page", "edits"]
      (\o -> do
          (s, u) <- maybe (Left "missing argument: page") pagePath (text o "page")
          edits <- maybe (Left "missing argument: edits") Right (KM.lookup "edits" o)
          return $ ApiRequest "PATCH" (T.concat ["/api/page/", s, "/", u]) (optional o ["message"])
                     [("Content-Type", "application/json")]
                     (TE.decodeUtf8 $ BL.toStrict $ A.encode $ A.object ["edits" .= edits]))
      asIs
  , Tool "write_page"
      "Replace the whole text of a page (header and content). Only for a deep rewrite: \
      \prefer edit_page. Give the hash that read_page gave: the write is refused if the page \
      \changed since."
      [ ("page", prop "string" "The page, as SOURCE/UID.")
      , ("hash", prop "string" "The hash of the page as read.")
      , ("text", prop "string" "The new text: header, blank line, content.")
      , messageProp ]
      ["page", "hash", "text"]
      (\o -> do
          (s, u) <- maybe (Left "missing argument: page") pagePath (text o "page")
          h <- maybe (Left "missing argument: hash") Right (text o "hash")
          t <- maybe (Left "missing argument: text") Right (text o "text")
          return $ ApiRequest "PUT" (T.concat ["/api/page/", s, "/", u]) (optional o ["message"])
                     [("If-Match", "\"" <> h <> "\"")] t)
      asIs
  , Tool "create_page"
      "Create a page in a source, from its text: header (title, tags, summary...), a blank \
      \line, then its content in CommonMark. Kiwi gives it its uid (in the answer, as \
      \SOURCE/UID). Search before creating: do not duplicate an existing page. The file is \
      \named after the title; admins may give its path."
      [ ("source", prop "string" "The source of the page.")
      , ("text", prop "string" "The text of the page: header, blank line, content.")
      , ("path", prop "string" "Path of the file from the top of the source, without extension (admins only).")
      , messageProp ]
      ["source", "text"]
      (\o -> do
          s <- maybe (Left "missing argument: source") Right (text o "source")
          t <- maybe (Left "missing argument: text") Right (text o "text")
          return $ ApiRequest "POST" ("/api/pages/" <> s) (optional o ["path", "message"]) [] t)
      asIs
  , Tool "edit_metadata"
      "Change the metadata (headers) of many pages at once, with a script: one line by \
      \directive or command, all of them applied or none, in one commit. A `pages` line chooses \
      \the pages of the commands after it: `pages SOURCE/*` (a source), `pages SOURCE/UID,SOURCE/UID` \
      \(pages), `pages *` (everything). Commands: `meta FIELD move OLD NEW`, `meta FIELD delete VALUE` \
      \(a tag with its sub-tags), `meta FIELD rename-to NEW`, `meta FIELD set VALUE`, \
      \`meta FIELD add VALUE`, `meta FIELD remove VALUE` (exact values), `meta FIELD unset`. \
      \Read metadata_guide before the first script. Run with check first to see the pages that \
      \would change, and ask the user before changing the vocabulary of the wiki. The check \
      \also runs on the pages that the account can only read, marked \"(read only)\": the \
      \user can then apply the script with an account that can change them."
      [ ("script", prop "string" "The lines of the script; # starts a comment.")
      , ("check", prop "boolean" "true to only check the script and list the pages it would change, writing nothing.")
      , messageProp ]
      ["script"]
      (\o -> do
          sc <- maybe (Left "missing argument: script") Right (text o "script")
          return $ ApiRequest "POST" "/api/metadata" ([ ("check", "yes") | flag o "check" ] ++ optional o ["message"]) [] sc)
      asIs
  , Tool "metadata_guide"
      "The syntax of the metadata scripts of edit_metadata, with examples."
      [] []
      (\_ -> Right $ apiGet "/api/metadata-guide" [])
      asIs
  ]
  where
    lookupCI k hs = lookup k hs
    isJust' = maybe False (const True)

-- | Properties of the tools selecting pages by their metadata (see
-- 'selectionParams')
selectionProps :: [(T.Text, A.Value)]
selectionProps =
  [ sourcesProp, archiveProp
  , ("tags", stringsProp "Tags the pages must all have, written as in the headers (\"a > b\"); a tag includes its sub-tags.")
  , ("fields", A.object [ "type" .= ("object" :: T.Text)
                        , "additionalProperties" .= A.object ["type" .= ("string" :: T.Text)]
                        , "description" .= ("Custom fields the pages must have, with this value: {\"author\": \"Smith\"}." :: T.Text) ])
  , ("has_fields", stringsProp "Custom fields the pages must have, whatever their value.")
  , ("pages", stringsProp "Only these pages, as SOURCE/UID.")
  , ("since", prop "string" "Only the pages modified since this day, YYYY-MM-DD.") ]

-- | Parameters of /api/pages and /api/facets from the arguments of
-- 'selectionProps'
selectionParams :: A.Object -> [(T.Text, T.Text)]
selectionParams o =
  contextParams o ++ [ ("tag", t) | t <- texts o "tags" ]
  ++ [ ("m." <> AK.toText k, v) | Just (A.Object fs) <- [KM.lookup "fields" o], (k, A.String v) <- KM.toList fs ]
  ++ [ ("has", f) | f <- texts o "has_fields" ]
  ++ [ ("page", p) | p <- texts o "pages" ]
  ++ optional o ["since"]

-- | The "show" parameter of the "show" argument
showParam :: A.Object -> [(T.Text, T.Text)]
showParam o = [ ("show", T.intercalate "," fs) | let fs = texts o "show", not (null fs) ]

-- | The "message" argument of the tools that write
messageProp :: (T.Text, A.Value)
messageProp = ("message", prop "string" "Optional: what was done and why, kept in the history of the page (git commit message).")
