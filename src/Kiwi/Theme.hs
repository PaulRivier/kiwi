{-# LANGUAGE TemplateHaskell #-}

-- | The theme static files (css, js, images) are embedded in the binary.
--
-- For theme development, when the environment variable KIWI_THEME_DIR is set
-- (to a theme folder, such as static/themes/dark), the files are read from
-- this folder at each request instead, and are not cached by the browser: a
-- page reload shows the changes, without building again.
module Kiwi.Theme (serveTheme) where

import           Control.Exception (IOException, try)
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
import           System.Environment (lookupEnv)
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
    | W.requestMethod req `elem` [Net.methodGet, Net.methodHead] -> do
        devDir <- lookupEnv "KIWI_THEME_DIR"
        case devDir of
          Just dir -> serveFromDir dir path
          Nothing -> serveEmbedded path
  _ -> app req respond
  where
    serveEmbedded path = case M.lookup path themeFiles of
      Just (content, etag) -> respond $
        if lookup Net.hIfNoneMatch (W.requestHeaders req) == Just etag
        then W.responseLBS Net.status304 (cacheHeaders etag) ""
        else W.responseLBS Net.status200
               ((Net.hContentType, mimeType path) : cacheHeaders etag)
               (BL.fromStrict content)
      Nothing -> app req respond
    -- no path outside of the folder
    serveFromDir dir path
      | any (\s -> s == ".." || T.any (`elem` ['/', '\\']) s) path = app req respond
      | otherwise = do
          contentE <- try $ BS.readFile $ FP.joinPath (dir : map T.unpack path)
          case contentE of
            Left (_ :: IOException) -> app req respond
            Right content -> respond $ W.responseLBS Net.status200
              [ (Net.hContentType, mimeType path), (Net.hCacheControl, "no-cache") ]
              (BL.fromStrict content)
    mimeType = defaultMimeLookup . T.intercalate "/"
    cacheHeaders etag = [ (Net.hCacheControl, "public, max-age=3600")
                        , ("ETag", etag) ]
