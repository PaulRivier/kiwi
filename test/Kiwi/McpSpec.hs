module Kiwi.McpSpec (mcpSpec) where

import           Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson as A
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy.Char8 as BLC
import           Data.IORef (modifyIORef, newIORef, readIORef)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Network.HTTP.Types as Net
import qualified Network.Wai as Wai
import qualified Network.Wai.Test as WT
import qualified System.FilePath as FP
import           Test.Hspec

import qualified Kiwi.Auth as Auth
import           Kiwi.Mcp (ApiRequest (..), handleMessage, instructions)
import           Kiwi.Types
import           TestUtils


mcpSpec :: Spec
mcpSpec = do
  let -- a fake API: answers the guide, and gives the request it got
      backend calls req = do
        let path = apiPath req
        modifyIORef calls (req :)
        return $ case path of
          "/api/guide" -> (200, [], "the guide")
          "/api/page/s/p" -> (200, [("etag", "\"abc\""), ("kiwi-lines", "1-3")], "title: P\n\nx\n")
          "/api/page/s/none" -> (404, [], "no page s/none")
          _ -> (200, [], "ok")
      run msg = do
        calls <- newIORef []
        answers <- handleMessage (backend calls) (BLC.pack msg)
        (,) answers . reverse <$> readIORef calls

  it "initializes with short instructions (the guide read with its tool), in the version of the client" $ do
    (Just a, _) <- run "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"initialize\",\
                    \\"params\":{\"protocolVersion\":\"2025-03-26\",\"capabilities\":{}}}"
    (result a >>= field "instructions") `shouldBe` Just (A.String instructions)
    instructions `shouldSatisfy` T.isInfixOf "call the tool guide first"
    (result a >>= field "protocolVersion") `shouldBe` Just (A.String "2025-03-26")
    field "id" a `shouldBe` Just (A.Number 1)

  it "ignores the notifications, and refuses the unknown methods" $ do
    (none, _) <- run "{\"jsonrpc\":\"2.0\",\"method\":\"notifications/initialized\"}"
    none `shouldBe` Nothing
    (Just a, _) <- run "{\"jsonrpc\":\"2.0\",\"id\":\"x\",\"method\":\"resources/list\"}"
    (field "error" a >>= field "code") `shouldBe` Just (A.Number (-32601))
    (Just b, _) <- run "not json"
    (field "error" b >>= field "code") `shouldBe` Just (A.Number (-32700))

  it "lists the tools" $ do
    (Just a, _) <- run "{\"jsonrpc\":\"2.0\",\"id\":2,\"method\":\"tools/list\"}"
    let names = do A.Array ts <- result a >>= field "tools"
                   return [ n | t <- foldr (:) [] ts, Just (A.String n) <- [field "name" t] ]
    names `shouldBe` Just [ "search", "facets", "list_pages", "read_page"
                          , "edit_page", "write_page", "create_page", "edit_metadata"
                          , "check_metadata", "guide" ]
    -- a title and the hints of what each tool does
    let hintsOf n = do
          A.Array ts <- result a >>= field "tools"
          t : _ <- Just [ t | t <- foldr (:) [] ts, field "name" t == Just (A.String n) ]
          an <- field "annotations" t
          return (field "title" t, field "readOnlyHint" an, field "destructiveHint" an)
    hintsOf "search" `shouldBe` Just (Just (A.String "Search the wiki"), Just (A.Bool True), Nothing)
    hintsOf "create_page" `shouldBe` Just (Just (A.String "Create a page"), Just (A.Bool False), Just (A.Bool False))
    hintsOf "edit_page" `shouldBe` Just (Just (A.String "Edit a page"), Just (A.Bool False), Just (A.Bool True))
    hintsOf "check_metadata" `shouldBe` Just (Just (A.String "Check a metadata script"), Just (A.Bool True), Nothing)

  it "checks a metadata script with check_metadata, and applies it with edit_metadata" $ do
    (_, checks) <- run "{\"jsonrpc\":\"2.0\",\"id\":9,\"method\":\"tools/call\",\"params\":{\
                       \\"name\":\"check_metadata\",\"arguments\":{\"script\":\"pages *\"}}}"
    checks `shouldBe` [ApiRequest "POST" "/api/metadata" [("check", "yes")] [] "pages *"]
    (_, applies) <- run "{\"jsonrpc\":\"2.0\",\"id\":10,\"method\":\"tools/call\",\"params\":{\
                        \\"name\":\"edit_metadata\",\"arguments\":{\"script\":\"pages *\",\"check\":true}}}"
    applies `shouldBe` [ApiRequest "POST" "/api/metadata" [] [] "pages *"]
    (_, guides) <- run "{\"jsonrpc\":\"2.0\",\"id\":11,\"method\":\"tools/call\",\"params\":{\
                       \\"name\":\"guide\",\"arguments\":{\"topic\":\"metadata-scripts\"}}}"
    guides `shouldBe` [ApiRequest "GET" "/api/guide" [("topic", "metadata-scripts")] [] ""]

  it "calls the API, and gives its text with the hash of a page" $ do
    (as, calls) <- run "{\"jsonrpc\":\"2.0\",\"id\":3,\"method\":\"tools/call\",\"params\":{\
                       \\"name\":\"list_pages\",\"arguments\":{\"sources\":[\"s\"],\"tags\":[\"a > b\"],\
                       \\"fields\":{\"zone\":\"north\"},\"has_fields\":[\"author\"],\
                       \\"show\":[\"title\",\"zone\"],\"pages\":[\"s/p\"],\"limit\":5}}}"
    fmap toolText as `shouldBe` Just (Just "ok")
    calls `shouldBe` [ApiRequest "GET" "/api/pages" [ ("source", "s"), ("tag", "a > b")
                                                    , ("m.zone", "north"), ("has", "author")
                                                    , ("page", "s/p"), ("show", "title,zone")
                                                    , ("limit", "5") ] [] ""]
    (_, facetCalls) <- run "{\"jsonrpc\":\"2.0\",\"id\":6,\"method\":\"tools/call\",\"params\":{\
                           \\"name\":\"facets\",\"arguments\":{\"tags\":[\"sport\",\"cuisine\"]}}}"
    facetCalls `shouldBe` [ApiRequest "GET" "/api/facets" [("tag", "sport"), ("tag", "cuisine")] [] ""]
    (Just p, _) <- run "{\"jsonrpc\":\"2.0\",\"id\":4,\"method\":\"tools/call\",\"params\":{\
                    \\"name\":\"read_page\",\"arguments\":{\"page\":\"s/p\"}}}"
    toolText p `shouldBe` Just "hash: abc\nlines: 1-3\n---\ntitle: P\n\nx\n"
    isError p `shouldBe` Just (A.Bool False)

  it "gives the errors of the API and of the arguments as tool errors" $ do
    (Just a, _) <- run "{\"jsonrpc\":\"2.0\",\"id\":5,\"method\":\"tools/call\",\"params\":{\
                    \\"name\":\"read_page\",\"arguments\":{\"page\":\"s/none\"}}}"
    toolText a `shouldBe` Just "error 404: no page s/none"
    isError a `shouldBe` Just (A.Bool True)
    (Just b, calls) <- run "{\"jsonrpc\":\"2.0\",\"id\":6,\"method\":\"tools/call\",\"params\":{\
                        \\"name\":\"read_page\",\"arguments\":{\"page\":\"nope\"}}}"
    fmap (T.isPrefixOf "invalid page") (toolText b) `shouldBe` Just True
    calls `shouldBe` []

  it "sends the edits of a page as JSON, and a new text with its hash" $ do
    (_, calls) <- run "{\"jsonrpc\":\"2.0\",\"id\":7,\"method\":\"tools/call\",\"params\":{\
                      \\"name\":\"edit_page\",\"arguments\":{\"page\":\"s/p\",\
                      \\"edits\":[{\"old\":\"a\",\"new\":\"b\"}],\"message\":\"why\"}}}"
    map (\r -> (apiMethod r, apiPath r, apiParams r, apiBody r)) calls
      `shouldBe` [("PATCH", "/api/page/s/p", [("message", "why")], "{\"edits\":[{\"new\":\"b\",\"old\":\"a\"}]}")]
    (_, puts) <- run "{\"jsonrpc\":\"2.0\",\"id\":8,\"method\":\"tools/call\",\"params\":{\
                     \\"name\":\"write_page\",\"arguments\":{\"page\":\"s/p\",\"hash\":\"abc\",\
                     \\"text\":\"title: T\\n\\nx\\n\"}}}"
    map (\r -> (apiMethod r, apiHeaders r, apiBody r)) puts
      `shouldBe` [("PUT", [("If-Match", "\"abc\"")], "title: T\n\nx\n")]

  -- POST /mcp, on a whole kiwi: the tools call its API in memory
  let pages = [ ("p.md", "title: Page\nuid: p\naccess: family:w\n\nHello world\n")
              , ("g.md", "title: Guide\nuid: g\naccess: family\n\nBe kind\n")
              , ("n.md", "title: Note\nsummary: A short note\nuid: n\naccess: family\nauthor: me\n\nto do\n") ]
      onKiwi test = withKiwiConfig ["agent-guide: agent-guide.md"] [("bob", "secret", ["family"])] pages $
                      \cd ss app -> do
        writeFile (FP.takeDirectory cd FP.</> "agent-guide.md") "Be kind\n"
        accs <- readIORef (accounts ss)
        made <- mapM Auth.newApiToken accs
        _ <- Auth.dumpTokens (M.fromList $ map snd made) (Auth.tokensFile $ kiwiDir ss)
        let token = case made of
              (t, _) : _ -> t
              [] -> error "no token"
        WT.runSession (test cd token) app
      send method headers b = WT.srequest $ WT.SRequest
        (WT.setPath Wai.defaultRequest { Wai.requestMethod = method
                                       , Wai.requestHeaders = headers } "/mcp")
        (BLC.pack b)
      post token = send "POST" . (("Authorization", "Bearer " <> TE.encodeUtf8 token) :)
      status' = Net.statusCode . WT.simpleStatus
      answer r = A.decode (WT.simpleBody r) :: Maybe A.Value

  it "serves the protocol on POST /mcp, the tools calling the API" $ onKiwi $ \cd token -> do
    i <- post token [] "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"initialize\",\
                       \\"params\":{\"protocolVersion\":\"2025-06-18\",\"capabilities\":{}}}"
    liftIO $ status' i `shouldBe` 200
    liftIO $ lookup "Content-Type" (WT.simpleHeaders i)
               `shouldSatisfy` maybe False ("application/json" `BLC.isPrefixOf`) . fmap BLC.fromStrict
    liftIO $ (answer i >>= result >>= field "instructions" >>= str) `shouldBe` Just instructions
    -- the guide of the user, with the tool guide
    g <- post token [] "{\"jsonrpc\":\"2.0\",\"id\":8,\"method\":\"tools/call\",\"params\":{\
                       \\"name\":\"guide\",\"arguments\":{}}}"
    liftIO $ fmap (T.isSuffixOf "Be kind\n") (answer g >>= toolText) `shouldBe` Just True
    n <- post token [("MCP-Protocol-Version", "2025-06-18")]
              "{\"jsonrpc\":\"2.0\",\"method\":\"notifications/initialized\"}"
    liftIO $ (status' n, WT.simpleBody n) `shouldBe` (202, "")
    r <- post token [] "{\"jsonrpc\":\"2.0\",\"id\":2,\"method\":\"tools/call\",\"params\":{\
                       \\"name\":\"read_page\",\"arguments\":{\"page\":\"s/p\"}}}"
    liftIO $ fmap (T.isInfixOf "Hello world") (answer r >>= toolText) `shouldBe` Just True
    liftIO $ fmap (T.isPrefixOf "hash: ") (answer r >>= toolText) `shouldBe` Just True
    q <- post token [] "{\"jsonrpc\":\"2.0\",\"id\":4,\"method\":\"tools/call\",\"params\":{\
                       \\"name\":\"search\",\"arguments\":{\"query\":\"hello world\"}}}"
    liftIO $ fmap (T.isPrefixOf "1 page for hello world") (answer q >>= toolText) `shouldBe` Just True
    -- the pages having a field, with their title, summary, size and keywords by default
    ann <- post token [] "{\"jsonrpc\":\"2.0\",\"id\":5,\"method\":\"tools/call\",\"params\":{\
                         \\"name\":\"list_pages\",\"arguments\":{\"has_fields\":[\"author\"]}}}"
    liftIO $ (answer ann >>= toolText) `shouldBe` Just "1 page, the most recently modified first\ns/n\n  title: Note\n  summary: A short note\n  content_size: chars=6 lines=1\n  keywords: note, short\n\n"
    -- a list sent as a string holding its JSON, as some clients do
    annStr <- post token [] "{\"jsonrpc\":\"2.0\",\"id\":6,\"method\":\"tools/call\",\"params\":{\
                            \\"name\":\"list_pages\",\"arguments\":{\"has_fields\":\"[\\\"author\\\"]\"}}}"
    liftIO $ (answer annStr >>= toolText) `shouldBe` Just "1 page, the most recently modified first\ns/n\n  title: Note\n  summary: A short note\n  content_size: chars=6 lines=1\n  keywords: note, short\n\n"
    -- an empty show: the uids alone
    uids <- post token [] "{\"jsonrpc\":\"2.0\",\"id\":7,\"method\":\"tools/call\",\"params\":{\
                          \\"name\":\"list_pages\",\"arguments\":{\"has_fields\":[\"author\"],\"show\":[]}}}"
    liftIO $ (answer uids >>= toolText) `shouldBe` Just "1 page, the most recently modified first\ns/n\n"
    e <- post token [] "{\"jsonrpc\":\"2.0\",\"id\":3,\"method\":\"tools/call\",\"params\":{\
                       \\"name\":\"edit_page\",\"arguments\":{\"page\":\"s/p\",\
                       \\"edits\":[{\"old\":\"Hello\",\"new\":\"Hi\"}]}}}"
    liftIO $ (answer e >>= isError) `shouldBe` Just (A.Bool False)
    content <- liftIO $ readFile (cd FP.</> "s" FP.</> "p.md")
    liftIO $ content `shouldSatisfy` ("Hi world" `isIn`)

  it "refuses no token, another origin, an unknown version, GET and DELETE" $ onKiwi $ \_ token -> do
    let ping = "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"ping\"}"
    none <- send "POST" [] ping
    liftIO $ status' none `shouldBe` 401
    other <- post token [("Origin", "http://evil.example")] ping
    liftIO $ status' other `shouldBe` 403
    newer <- post token [("MCP-Protocol-Version", "2099-01-01")] ping
    liftIO $ status' newer `shouldBe` 400
    batch <- post token [] ("[" ++ ping ++ "]")
    liftIO $ (answer batch >>= field "error" >>= field "code") `shouldBe` Just (A.Number (-32600))
    g <- send "GET" [] ""
    liftIO $ (status' g, lookup "Allow" (WT.simpleHeaders g)) `shouldBe` (405, Just "POST")
    d <- send "DELETE" [] ""
    liftIO $ status' d `shouldBe` 405
  where
    field k (A.Object o) = KM.lookup k o
    field _ _ = Nothing
    result = field "result"
    str (A.String t) = Just t
    str _ = Nothing
    isError a = result a >>= field "isError"
    toolText a = do
      A.Array cs <- result a >>= field "content"
      c : _ <- Just (foldr (:) [] cs)
      A.String t <- field "text" c
      return t
    isIn x y = T.pack x `T.isInfixOf` T.pack y
