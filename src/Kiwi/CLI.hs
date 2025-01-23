module Kiwi.CLI (Config(..), ServerConfig(..), InitConfig(..), AccessConfig(..), AccessOp(..),
                 parseCLI ) where

-- Afficher version Cabal
import Paths_kiwi (version)
import Data.Version (showVersion)

import Options.Applicative
import qualified System.FilePath as FP

data Config = Server ServerConfig
            | Init InitConfig
            | Access AccessConfig


data ServerConfig =
  ServerConfig { serverCfps :: [FP.FilePath] }

data InitConfig =
  InitConfig { initDirectory :: FP.FilePath }

data AccessConfig =
  AccessConfig { accessCfp :: FP.FilePath
               , accessOp  :: AccessOp }

data AccessOp = AccessCreate String
              | AccessRemove String

parser :: Parser Config
parser = hsubparser
         ( command "serve" (info serveParser (progDesc "Start your kiwi server, serve one or more directory"))   <>
           command "init"  (info initParser (progDesc "Init a new kiwi directory")) <>
           command "access" (info accessParser (progDesc "Manage accounts and permissions")) )
  where
    serveParser :: Parser Config
    serveParser =  Server . ServerConfig <$> configFilePathsParser

    initParser  :: Parser Config
    initParser  =  Init <$> (InitConfig <$> initDirParser)

    accessParser :: Parser Config
    accessParser = Access <$>
      ( AccessConfig <$> configFilePathParser <*>
        ( ( AccessCreate <$> option readLogin ( long "create"
                                                <> metavar "LOGIN"
                                                <> help "create new account with login" ) ) <|>
          ( AccessRemove <$> option readLogin ( long "remove"
                                                <> metavar "LOGIN"
                                                <> help "remove account with login" ) ) )
      )

        
    configFilePathParser = argument str ( metavar "CONFIG_FILE"
                                          <> help "Path to your kiwi config file" )

    configFilePathsParser = some $
      argument str ( metavar "CONFIG_FILE [CONFIG_FILE]"
                     <> help "Paths to your kiwi config files (one or more)" )

    initDirParser = argument str ( metavar "KIWI_DIRECTORY"
                                   <> help "Path to your new kiwi directory, \
                                           \ must be an existing empty folder" )

    readLogin = maybeReader $
      \l -> case all (flip elem alphanum) l of
              True -> Just l
              False -> Nothing
      where alphanum = ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9'] ++ "-_"


spec :: ParserInfo Config
spec = info (helper <*> parser)
            (fullDesc
            <> progDesc (concat ["Version ", showVersion version, ". Kiwi is a powerful personal wiki system."])
            <> header "Kiwi")

parseCLI :: IO Config
parseCLI = execParser spec




