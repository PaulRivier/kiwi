module Kiwi.McpSpec (mcpSpec) where

import qualified Data.Aeson as A
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy.Char8 as BLC
import           Data.IORef (modifyIORef, newIORef, readIORef)
import qualified Data.Text as T
import           Test.Hspec

import           Kiwi.Mcp (ApiRequest (..), handleMessage)


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
      field k (A.Object o) = KM.lookup k o
      field _ _ = Nothing
      result a = field "result" a
      toolText a = do
        A.Array cs <- result a >>= field "content"
        c : _ <- Just (foldr (:) [] cs)
        A.String t <- field "text" c
        return t
      isError a = result a >>= field "isError"

  it "initializes with the guide as instructions, in the version of the client" $ do
    ([a], _) <- run "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"initialize\",\
                    \\"params\":{\"protocolVersion\":\"2025-03-26\",\"capabilities\":{}}}"
    (result a >>= field "instructions") `shouldBe` Just (A.String "the guide")
    (result a >>= field "protocolVersion") `shouldBe` Just (A.String "2025-03-26")
    field "id" a `shouldBe` Just (A.Number 1)

  it "ignores the notifications, and refuses the unknown methods" $ do
    (none, _) <- run "{\"jsonrpc\":\"2.0\",\"method\":\"notifications/initialized\"}"
    none `shouldBe` []
    ([a], _) <- run "{\"jsonrpc\":\"2.0\",\"id\":\"x\",\"method\":\"resources/list\"}"
    (field "error" a >>= field "code") `shouldBe` Just (A.Number (-32601))
    ([b], _) <- run "not json"
    (field "error" b >>= field "code") `shouldBe` Just (A.Number (-32700))

  it "lists the tools" $ do
    ([a], _) <- run "{\"jsonrpc\":\"2.0\",\"id\":2,\"method\":\"tools/list\"}"
    let names = do A.Array ts <- result a >>= field "tools"
                   return [ n | t <- foldr (:) [] ts, Just (A.String n) <- [field "name" t] ]
    names `shouldBe` Just [ "search", "list_pages", "vocabulary", "read_page", "folders"
                          , "edit_page", "write_page", "create_page", "edit_metadata" ]

  it "calls the API, and gives its text with the hash of a page" $ do
    (as, calls) <- run "{\"jsonrpc\":\"2.0\",\"id\":3,\"method\":\"tools/call\",\"params\":{\
                       \\"name\":\"list_pages\",\"arguments\":{\"sources\":[\"s\"],\"tags\":[\"a > b\"],\
                       \\"fields\":{\"zone\":\"north\"},\"limit\":5}}}"
    map toolText as `shouldBe` [Just "ok"]
    calls `shouldBe` [ApiRequest "GET" "/api/pages" [ ("source", "s"), ("tag", "a > b")
                                                    , ("m.zone", "north"), ("limit", "5") ] [] ""]
    ([p], _) <- run "{\"jsonrpc\":\"2.0\",\"id\":4,\"method\":\"tools/call\",\"params\":{\
                    \\"name\":\"read_page\",\"arguments\":{\"page\":\"s/p\"}}}"
    toolText p `shouldBe` Just "hash: abc\nlines: 1-3\n---\ntitle: P\n\nx\n"
    isError p `shouldBe` Just (A.Bool False)

  it "gives the errors of the API and of the arguments as tool errors" $ do
    ([a], _) <- run "{\"jsonrpc\":\"2.0\",\"id\":5,\"method\":\"tools/call\",\"params\":{\
                    \\"name\":\"read_page\",\"arguments\":{\"page\":\"s/none\"}}}"
    toolText a `shouldBe` Just "error 404: no page s/none"
    isError a `shouldBe` Just (A.Bool True)
    ([b], calls) <- run "{\"jsonrpc\":\"2.0\",\"id\":6,\"method\":\"tools/call\",\"params\":{\
                        \\"name\":\"read_page\",\"arguments\":{\"page\":\"nope\"}}}"
    fmap (T.isPrefixOf "invalid page") (toolText b) `shouldBe` Just True
    calls `shouldBe` []

  it "sends the edits of a page as JSON, and a new text with its hash" $ do
    (_, calls) <- run "{\"jsonrpc\":\"2.0\",\"id\":7,\"method\":\"tools/call\",\"params\":{\
                      \\"name\":\"edit_page\",\"arguments\":{\"page\":\"s/p\",\
                      \\"edits\":[{\"old\":\"a\",\"new\":\"b\"}]}}}"
    map (\r -> (apiMethod r, apiPath r, apiBody r)) calls
      `shouldBe` [("PATCH", "/api/page/s/p", "{\"edits\":[{\"new\":\"b\",\"old\":\"a\"}]}")]
    (_, puts) <- run "{\"jsonrpc\":\"2.0\",\"id\":8,\"method\":\"tools/call\",\"params\":{\
                     \\"name\":\"write_page\",\"arguments\":{\"page\":\"s/p\",\"hash\":\"abc\",\
                     \\"text\":\"title: T\\n\\nx\\n\"}}}"
    map (\r -> (apiMethod r, apiHeaders r, apiBody r)) puts
      `shouldBe` [("PUT", [("If-Match", "\"abc\"")], "title: T\n\nx\n")]
