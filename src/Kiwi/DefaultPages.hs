{-# LANGUAGE TemplateHaskell #-}


module Kiwi.DefaultPages
  ( defaultHomePage
  , commonmarkDocPage
  , readmePage
  , agentApiDocPage
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


-- | Documentation of the API for the agents (see "Kiwi.Api"), the same for
-- every kiwi; the guide of the user follows it
agentApiDocPage :: BS.ByteString
agentApiDocPage = $(embedFileRelative "static/agent-api-doc.md")


-- | Syntax of the metadata scripts, for the admins (metadata editor) and the
-- agents (API)
metadataScriptPage :: BS.ByteString
metadataScriptPage = $(embedFileRelative "static/metadata-script.md")
