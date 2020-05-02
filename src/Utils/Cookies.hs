{-# LANGUAGE OverloadedStrings #-}
module Utils.Cookies
    ( makeSimpleCookie
    , setCookie
    , setSimpleCookie
    , getCookie
    , getCookies
    , deleteCookie
    ) where

import Control.Monad ( liftM )

import qualified Data.Text as TS
import qualified Data.Text.Encoding as TS
import qualified Data.Text.Lazy.Encoding as TL

import qualified Data.Map as Map

import qualified Data.ByteString.Lazy as BSL

import Data.Time.Clock ( secondsToDiffTime )

import Blaze.ByteString.Builder ( toLazyByteString )

import Web.Scotty.Trans
import Web.Cookie


makeSimpleCookie :: TS.Text -- ^ name
                 -> TS.Text -- ^ value
                 -> Integer -- ^ lifetime in seconds
                 -> SetCookie
makeSimpleCookie n v r = def { setCookieName  = TS.encodeUtf8 n
                             , setCookieValue = TS.encodeUtf8 v
                             , setCookieMaxAge = Just $ secondsToDiffTime r
                             }


setCookie :: (Monad m, ScottyError e)
          => SetCookie
          -> ActionT e m ()
setCookie c = addHeader "Set-Cookie" (TL.decodeUtf8 . toLazyByteString $ renderSetCookie c)


-- | 'makeSimpleCookie' and 'setCookie' combined.
setSimpleCookie :: (Monad m, ScottyError e)
                => TS.Text -- ^ name
                -> TS.Text -- ^ value
                -> Integer -- ^ lifetime in seconds
                -> ActionT e m ()
setSimpleCookie n v r = setCookie $ makeSimpleCookie n v r


getCookie :: (Monad m, ScottyError e)
          => TS.Text -- ^ name
          -> ActionT e m (Maybe TS.Text)
getCookie c = liftM (Map.lookup c) getCookies


-- | Returns all cookies
getCookies :: (Monad m, ScottyError e)
           => ActionT e m (Map.Map TS.Text TS.Text)
getCookies = liftM (Map.fromList . maybe [] parse) $ header "Cookie"
    where parse = parseCookiesText . BSL.toStrict . TL.encodeUtf8


deleteCookie :: (Monad m, ScottyError e)
             => TS.Text -- ^ name
             -> ActionT e m ()
deleteCookie c = setCookie $ (makeSimpleCookie c "" 0)
