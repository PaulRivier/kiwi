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
import Control.Monad.IO.Class (MonadIO)

import qualified Data.Text as TS
import qualified Data.Text.Encoding as TS
import qualified Data.Text.Lazy.Encoding as TL

import qualified Data.Map as Map

import qualified Data.ByteString.Lazy as BSL

import Data.Time.Clock ( secondsToDiffTime )

import Blaze.ByteString.Builder ( toLazyByteString )

import Web.Scotty.Trans
import Web.Cookie


-- TODO : scotty intègre maintenant des libs pour les cookies
-- https://hackage-content.haskell.org/package/scotty-0.30/docs/Web-Scotty-Cookie.html

makeSimpleCookie :: TS.Text -- ^ name
                 -> TS.Text -- ^ value
                 -> Integer -- ^ lifetime in seconds
                 -> SetCookie
makeSimpleCookie n v r = def { setCookieName  = TS.encodeUtf8 n
                             , setCookieValue = TS.encodeUtf8 v
                             , setCookieMaxAge = Just $ secondsToDiffTime r
                             }




setCookie :: (MonadIO m)
          => SetCookie
          -> ActionT  m ()
setCookie c = addHeader "Set-Cookie" (TL.decodeUtf8 . toLazyByteString $ renderSetCookie c)


-- | 'makeSimpleCookie' and 'setCookie' combined.
setSimpleCookie :: (MonadIO m)
                => TS.Text -- ^ name
                -> TS.Text -- ^ value
                -> Integer -- ^ lifetime in seconds
                -> ActionT m ()
setSimpleCookie n v r = setCookie $ makeSimpleCookie n v r


getCookie :: (MonadIO m)
          => TS.Text -- ^ name
          -> ActionT m (Maybe TS.Text)
getCookie c = liftM (Map.lookup c) getCookies


-- | Returns all cookies
getCookies :: (MonadIO m)
           => ActionT m (Map.Map TS.Text TS.Text)
getCookies = liftM (Map.fromList . maybe [] parse) $ header "Cookie"
    where parse = parseCookiesText . BSL.toStrict . TL.encodeUtf8


deleteCookie :: (MonadIO m)
             => TS.Text -- ^ name
             -> ActionT m ()
deleteCookie c = setCookie $ (makeSimpleCookie c "" 0)
