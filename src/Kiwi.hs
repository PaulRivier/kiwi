{-# LANGUAGE OverloadedStrings #-}

module Kiwi where

import           Control.Concurrent.Async (mapConcurrently)
import           Control.Exception (bracket_)
import           Control.Monad (forM_)
import qualified Crypto.Hash.SHA256 as H
import qualified Data.ByteString.Base16 as B16
import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import qualified Network.Wai.Middleware.Static as Static
import qualified System.Directory as D
import qualified System.FilePath as FP
import           System.IO

import Kiwi.CLI as CLI
import Kiwi.ConfigFile as Conf
import qualified Kiwi.DefaultPages as DP
import Kiwi.Server (kiwiServer)
import qualified Kiwi.Utils as U
import Kiwi.Types


runKiwiRun :: IO ()
runKiwiRun = do
  cli <- CLI.parseCLI
  case cli of
    Server c -> do
      cache <- U.initWAICaching
      _ <- mapConcurrently (runServer cache) (CLI.serverCfps c)
      return ()
    Init c -> initKiwiDir (CLI.initDirectory c)
    Access action -> runAccess action


runServer :: Static.CacheContainer -> FP.FilePath -> IO ()
runServer cache cfp = do
  cfgTextM <- U.getFileContent cfp
  case cfgTextM of
    Nothing -> putStrLn "This config file does not exist"
    Just cfgText -> do
      case Conf.parseConfig cfgText of
        Left e -> do
          putStrLn "Error while reading config file :"
          putStrLn e
          putStrLn "Kiwi will now write a clean kiwi.ini to kiwi.ini.default \
                   \to help you fix your config"
          let kd = FP.takeDirectory cfp
          writeFile (FP.combine kd "kiwi.ini.default") Conf.defaultConfigFileContent
        Right conf -> kiwiServer cfp conf cache


initKiwiDir :: FP.FilePath -> IO ()
initKiwiDir d = do
  exists <- D.doesDirectoryExist d
  case exists of
    False -> putStrLn "Directory does not exist, check your path"
    True -> do
      clean  <-  filter (\(x:_) -> x /= '.') <$> D.listDirectory d
      case null clean of 
        False -> do
          putStrLn $ unlines ["Directory must be empty for initialisation",
                              "In case you want to update your kiwi.ini file, ",
                              "kiwi will copy current kiwi.ini file format to kiwi.ini.default"]
          writeFile (FP.combine d "kiwi.ini.default") Conf.defaultConfigFileContent
        True -> createKiwi d
  where
    createKiwi r = do
      forM_ (dirs r) D.createDirectory
      writeFile (FP.combine r "kiwi.ini") Conf.defaultConfigFileContent
      writeFile (FP.combine r "content/my/pages/Home.md") DP.defaultHomePage
      writeFile (FP.combine r "content/my/pages/CommonMark.md") DP.commonmarkDocPage
      putStrLn $ unlines ["Kiwi directory created at " ++ r,
                          "You should now do the following steps :",
                          " - check kiwi.ini file",
                          " - make sure to link to a proper theme folder",
                          " - start wiki with command : kiwi serve " ++ FP.combine r "kiwi.ini"]
      -- todo : config, Home.md, static
      
    dirs r = map (FP.combine r) ["content", "content/my", "content/my/pages",
                                 "content/my/images", "content/my/files"]
      


runAccess :: AccessConfig -> IO ()
runAccess c = do
  let cfp = accessCfp c
  let op = accessOp c
  let accountsFilePath = FP.combine (FP.takeDirectory cfp) "_accounts"
  accounts' <- U.loadAccounts accountsFilePath
  case op of
    AccessCreate l -> do
      putStrLn "Password:"
      pass <- noEcho TIO.getLine
      putStrLn "Groups (comma separated):"
      groups <- TIO.getLine
      salt <- U.randomText 10
      let block = T.concat [salt, pass]
      let hash = TE.decodeUtf8 $ B16.encode $ H.hash $ TE.encodeUtf8 block
      let acc = Account (T.pack l) salt hash (map T.strip $ T.splitOn "," groups)
      let newAccounts = L.nubBy (\a b -> accountId a == accountId b) (acc:accounts')
      U.dumpAccounts newAccounts accountsFilePath
      putStrLn "Access created"
    AccessRemove l -> do
      let newAccounts = filter (\a -> accountId a /= (T.pack l)) accounts'
      U.dumpAccounts newAccounts accountsFilePath
      putStrLn "Access removed"

noEcho :: IO a -> IO a
noEcho action = do
  bracket_ (hSetEcho stdin False)
           (hSetEcho stdin True)
           action

          
