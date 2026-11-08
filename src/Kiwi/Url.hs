-- | URLs of the pages, and the navigation context in their query string
module Kiwi.Url
  ( pageUrl
  , pageUIDLink
  , contextParams
  , withoutContextParams
  , inContext
  , renderParams
  ) where

import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Network.HTTP.Types.URI as URI

import           Kiwi.Types


pageUrl :: Page -> T.Text
pageUrl p = let (s,i) = pageUID p in
  T.concat ["/page/", urlEncodeText s, "/", urlEncodeText i]

-- | Query parameters of the navigation context: "source" for each source,
-- as on the browse page, and "archive=yes" in the archives
contextParams :: Context -> [(T.Text, T.Text)]
contextParams ctx = [ ("source", s) | s <- S.toList (contextSources ctx) ]
                    ++ [ ("archive", "yes") | contextArchive ctx ]

-- | The query parameters that are not those of the navigation context (see
-- 'contextParams'): the parameters of the page itself
withoutContextParams :: [(T.Text, T.Text)] -> [(T.Text, T.Text)]
withoutContextParams = filter ((`notElem` ["source", "archive"]) . fst)

-- | Add the navigation context to an URL without query string, before the
-- fragment if any
inContext :: Context -> T.Text -> T.Text
inContext ctx url = let (path, fragment) = T.breakOn "#" url
                    in T.concat [path, renderParams (contextParams ctx), fragment]

-- | Query string of the parameters, empty without parameter
renderParams :: [(T.Text, T.Text)] -> T.Text
renderParams ps = TE.decodeUtf8 $ URI.renderQuery True
  [ (TE.encodeUtf8 n, Just $ TE.encodeUtf8 v) | (n, v) <- ps ]

pageUIDLink :: Page -> T.Text
pageUIDLink p = let (_,i) = pageUID p in
  T.concat ["/", i]

urlEncodeText :: T.Text -> T.Text
urlEncodeText = TE.decodeUtf8 . URI.urlEncode False . TE.encodeUtf8

-- urlDecodeText :: T.Text -> T.Text
-- urlDecodeText = TE.decodeUtf8 . URI.urlDecode False . TE.encodeUtf8
