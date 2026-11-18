{-# LANGUAGE TemplateHaskell #-}


module Kiwi.DefaultPages
  ( defaultHomePage
  , commonmarkDocPage
  , readmePage
  , agentGuidePage
  , metadataScriptPage
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


-- | Syntax of the metadata scripts, for the admins (metadata editor) and the
-- agents (API)
metadataScriptPage :: BS.ByteString
metadataScriptPage = $(embedFileRelative "static/metadata-script.md")
