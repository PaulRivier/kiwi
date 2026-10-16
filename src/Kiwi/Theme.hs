{-# LANGUAGE TemplateHaskell #-}

-- | The theme static files (css, js, images) are embedded in the binary
module Kiwi.Theme (serveTheme) where

import qualified Crypto.Hash as H
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import           Data.FileEmbed (embedDir, makeRelativeToProject)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Network.HTTP.Types as Net
import           Network.Mime (defaultMimeLookup)
import qualified Network.Wai as W
import qualified System.FilePath as FP


-- | Theme files by path segments, with their content and ETag
themeFiles :: M.Map [T.Text] (BS.ByteString, BS.ByteString)
themeFiles = M.fromList
  [ (map T.pack $ FP.splitDirectories fp, (content, etag content))
  | (fp, content) <- $(makeRelativeToProject "static/themes/dark" >>= embedDir) ]
  where
    etag c = BC.pack $ concat ["\"", show (H.hash c :: H.Digest H.SHA1), "\""]


-- | Serves the theme files under /static/
serveTheme :: W.Middleware
serveTheme app req respond = case W.pathInfo req of
  ("static" : path)
    | W.requestMethod req `elem` [Net.methodGet, Net.methodHead]
    , Just (content, etag) <- M.lookup path themeFiles ->
        respond $
          if lookup Net.hIfNoneMatch (W.requestHeaders req) == Just etag
          then W.responseLBS Net.status304 (cacheHeaders etag) ""
          else W.responseLBS Net.status200
                 ((Net.hContentType, defaultMimeLookup $ T.intercalate "/" path)
                  : cacheHeaders etag)
                 (BL.fromStrict content)
  _ -> app req respond
  where
    cacheHeaders etag = [ (Net.hCacheControl, "public, max-age=3600")
                        , ("ETag", etag) ]
