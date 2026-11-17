{-# LANGUAGE TemplateHaskell #-}


module Kiwi.DefaultPages
  ( defaultHomePage
  , commonmarkDocPage
  , readmePage
  , agentGuidePage
  ) where


import qualified Data.ByteString as BS
import           Data.FileEmbed (embedFileRelative)


defaultHomePage :: BS.ByteString
defaultHomePage = $(embedFileRelative "static/defaultHomePage.md")


commonmarkDocPage :: BS.ByteString
commonmarkDocPage = $(embedFileRelative "static/CommonMark.md")


readmePage :: BS.ByteString
readmePage = $(embedFileRelative "README.md")


-- | Guide of the agents using the API (see "Kiwi.Api"), the same for every
-- kiwi
agentGuidePage :: BS.ByteString
agentGuidePage = $(embedFileRelative "static/agent-guide.md")
