module Kiwi.Log
  ( LogLevel (..)
  , Logger
  , mkLogger
  ) where

import           Control.Monad (when)
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import           System.IO (stderr, stdout)


-- | From the most to the least important
data LogLevel = Error | Warn | Info | Debug
  deriving (Eq, Ord, Show)

-- | Writes a message of the given level
type Logger = LogLevel -> T.Text -> IO ()

-- | Writes the messages up to the max level, prefixed with the kiwi name.
-- Errors and warnings go to stderr, the others to stdout. Each line is
-- written by one call, which holds the lock of the handle: the lines of
-- concurrent requests are not mixed (stderr is not buffered, a Text would
-- be written character by character).
mkLogger :: LogLevel -> T.Text -> Logger
mkLogger maxLevel name level msg = when (level <= maxLevel) $
  BS.hPut handle $ TE.encodeUtf8 $ T.concat ["[kiwi:", name, "] ", msg, "\n"]
  where handle = if level <= Warn then stderr else stdout
