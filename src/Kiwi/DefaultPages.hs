{-# LANGUAGE TemplateHaskell #-}


module Kiwi.DefaultPages
  ( defaultHomePage
  , commonmarkDocPage
  ) where


import qualified Data.ByteString as BS
import           Data.FileEmbed (embedFileRelative)


defaultHomePage :: BS.ByteString
defaultHomePage = $(embedFileRelative "static/defaultHomePage.md")


commonmarkDocPage :: BS.ByteString
commonmarkDocPage = $(embedFileRelative "static/CommonMark.md")
