module Kiwi.CLI (Config(..), ServerConfig(..), InitConfig(..), AccessConfig(..), AccessOp(..),
                 parseCLI ) where

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
            <> progDesc "Kiwi is a powerful personal wiki system"
            <> header "Kiwi")

parseCLI :: IO Config
parseCLI = execParser spec





-- data ServerConfig =
--   ServerConfig { serveContentDir :: FP.FilePath
--                , staticDir :: FP.FilePath
--                , port :: Int
--                , editor :: String
--                , logging :: Bool }

-- data HelloConfig =
--   HelloConfig { helloContentDir :: FP.FilePath
--               , name :: String }


-- contentDir :: Config -> FP.FilePath
-- contentDir (Server sc) = serveContentDir sc
-- contentDir (Hello hc) = helloContentDir hc

-- parser :: Parser Config
-- parser = hsubparser
--          ( command "serve" (info serveParser (progDesc "Start kiwi server"))   <>
--            command "hello" (info helloParser (progDesc "Say hello kiwi")) )
--   where
--     serveParser :: Parser Config
--     serveParser =  Server <$>
--       ( ServerConfig <$>
--         contentDirParser <*>
--         strOption ( long "static-dir"
--                     <> metavar "STATIC_PATH"
--                     <> value "static"
--                     <> help "Path of the static directory, default to 'static' in current path" )
--         <*>
--         option auto ( long "port"
--                       <> metavar "PORT"
--                       <> value 3456
--                       <> help "Port to listen on, default to 3456" )
--         <*>
--         strOption ( long "editor"
--                     <> metavar "TEXT_EDITOR_COMMAND"
--                     <> value "gedit %s"
--                     <> help "Text editor command, with %s as filename placeholder" )
--         <*>
--         switch ( long "log"
--                  <> help "Log requests, useful for debugging" )
--       )
      

--     helloParser :: Parser Config
--     helloParser = Hello <$>
--       ( HelloConfig <$>
--         contentDirParser <*>
--         strOption ( long "name"
--                     <> metavar "NAME"
--                     <> help "Name to say hello to" ) )

--     contentDirParser = argument str ( metavar "CONTENT_PATH"
--                                       <> value "content"
--                                       <> help "Path of the content root directory, \
--                                               \ default to 'content' in current path" )

