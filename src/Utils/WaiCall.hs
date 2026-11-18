-- | A WAI application called in memory, without server nor socket: a
-- request built from its parts, its response read whole.
module Utils.WaiCall
  ( localRequest
  , callApp
  ) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as BL
import           Data.IORef (atomicModifyIORef', modifyIORef', newIORef, readIORef, writeIORef)
import qualified Network.HTTP.Types as Net
import qualified Network.Wai as Wai
import           Network.Wai.Internal (ResponseReceived (..))


-- | A request: its method, path (without query), query parameters and
-- headers
localRequest :: Net.Method -> BS.ByteString -> Net.Query -> Net.RequestHeaders -> Wai.Request
localRequest method path query headers = Wai.defaultRequest
  { Wai.requestMethod = method
  , Wai.rawPathInfo = path
  , Wai.pathInfo = Net.decodePathSegments path
  , Wai.rawQueryString = Net.renderQuery True query
  , Wai.queryString = query
  , Wai.requestHeaders = headers }

-- | The status, headers and body of the response of the application to the
-- request with this body
callApp :: Wai.Application -> Wai.Request -> BL.ByteString
        -> IO (Net.Status, Net.ResponseHeaders, BL.ByteString)
callApp app req body = do
  -- the body, given by chunks, then empty chunks at its end
  chunks <- newIORef (BL.toChunks body)
  let nextChunk = atomicModifyIORef' chunks $ \cs -> case cs of
        c : rest -> (rest, c)
        []       -> ([], BS.empty)
      req' = Wai.setRequestBodyChunks nextChunk
               req { Wai.requestBodyLength = Wai.KnownLength (fromIntegral $ BL.length body) }
  result <- newIORef Nothing
  _ <- app req' $ \resp -> do
    let (st, headers, withBody) = Wai.responseToStream resp
    acc <- newIORef mempty
    withBody $ \stream -> stream (\b -> modifyIORef' acc (<> b)) (return ())
    b <- readIORef acc
    writeIORef result $ Just (st, headers, BB.toLazyByteString b)
    return ResponseReceived
  maybe (fail "the application gave no response") return =<< readIORef result
