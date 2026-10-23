module Kiwi.Log
  ( LogLevel (..)
  , Logger
  , mkLogger
  ) where

import           Control.Monad (when)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import           System.IO (stderr, stdout)


-- | From the most to the least important
data LogLevel = Error | Warn | Info | Debug
  deriving (Eq, Ord, Show)

-- | Writes a message of the given level
type Logger = LogLevel -> T.Text -> IO ()

-- | Writes the messages up to the max level, prefixed with the kiwi name.
-- Errors and warnings go to stderr, the others to stdout.
mkLogger :: LogLevel -> T.Text -> Logger
mkLogger maxLevel name level msg = when (level <= maxLevel) $
  TIO.hPutStrLn handle $ T.concat ["[kiwi:", name, "] ", msg]
  where handle = if level <= Warn then stderr else stdout
