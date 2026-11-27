module Kiwi.ConfigFileSpec (configSpec) where

import           Data.List (isInfixOf)
import qualified System.FilePath as FP
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import           Test.Hspec

import qualified Kiwi.ConfigFile as Conf
import           Kiwi.Types
import qualified Kiwi.Utils as U
import           TestUtils (withContentDir)

configSpec :: Spec
configSpec = do
  let ini ls = T.unlines $ [ "[kiwi]", "content-directory: content", "port: 3456" ] ++ ls
                           ++ [ "[metadata-default]" ]
      failsWith msg r = case r of
        Left e -> e `shouldSatisfy` (msg `isInfixOf`)
        Right _ -> expectationFailure "the config should be invalid"

  it "parses config values, ignoring case and spaces" $ do
    parseLang " French " `shouldBe` Right French
    parseLang "turkish" `shouldBe` Right Turkish
    parseUILang "FRENCH" `shouldBe` Right UI_French
    parseTocSetting "auto" `shouldBe` Right TocAuto
    parseCustomMetaType " Date" `shouldBe` Right CmtDate

  it "gives the allowed values of an invalid config value" $ do
    parseLang "klingon" `shouldBe`
      Left "invalid lang \"klingon\", allowed values: danish, dutch, english, \
           \finnish, french, german, hungarian, italian, norwegian, portuguese, \
           \romanian, russian, spanish, swedish, turkish"
    parseUILang "german" `shouldBe`
      Left "invalid ui-lang \"german\", allowed values: english, french"
    parseTocSetting "yes" `shouldBe`
      Left "invalid toc setting \"yes\", allowed values: on, off, auto"
    parseCustomMetaType "float" `shouldBe`
      Left "invalid metadata type \"float\", allowed values: text, int, date, bool"
    U.parseLangSetting "auto/french/x" `shouldBe`
      Left "invalid lang \"auto/french/x\", expected LANG, auto or auto/LANG"

  it "has default values for ui-lang, toc, editor and access" $ do
    let c = Conf.parseConfig $ ini []
    fmap Conf.uiLang c `shouldBe` Right UI_English
    fmap Conf.toc c `shouldBe` Right TocOn
    fmap Conf.editor c `shouldBe` Right Nothing
    fmap (metaAccess . Conf.defaultMeta) c `shouldBe` Right (Just [("admin", AccessRead)])

  it "reads ui-lang, toc, editor and the custom metadata types and descriptions" $ do
    let c = Conf.parseConfig $ ini [ "ui-lang: french", "toc: off"
                                   , "editor: emacsclient -n -s kiwi" ]
                               <> "[metadata year]\ntype: int\ndescription: of publication\n"
                               <> "[metadata author]\ntype: text\nrelating: 2.5\n"
    fmap Conf.uiLang c `shouldBe` Right UI_French
    fmap Conf.toc c `shouldBe` Right TocOff
    fmap Conf.editor c `shouldBe` Right (Just ("emacsclient", ["-n", "-s", "kiwi"]))
    fmap (map cmcType . Conf.customMetaConfig) c `shouldBe` Right [CmtInt, CmtText]
    fmap (map cmcDescription . Conf.customMetaConfig) c `shouldBe` Right [Just "of publication", Nothing]
    -- the weight of a field for the related pages, 0 (left out) by default
    fmap (map cmcRelating . Conf.customMetaConfig) c `shouldBe` Right [0, 2.5]
    -- an empty editor disables the edition, as no editor
    fmap Conf.editor (Conf.parseConfig $ ini ["editor:"]) `shouldBe` Right Nothing

  it "refuses invalid values, with the allowed ones" $ do
    failsWith "allowed values: english, french" $ Conf.parseConfig $ ini ["ui-lang: german"]
    failsWith "allowed values: on, off, auto" $ Conf.parseConfig $ ini ["toc: yes"]
    failsWith "allowed values: yes, no" $ Conf.parseConfig $ ini [] <> "[source s]\nread-only: maybe\n"
    failsWith "allowed values: text, int, date, bool" $ Conf.parseConfig $
      ini [] <> "[metadata year]\ntype: float\n"
    failsWith "expected a weight" $ Conf.parseConfig $
      ini [] <> "[metadata author]\ntype: text\nrelating: yes\n"
    failsWith "allowed values: danish" $ Conf.parseConfig $ ini [] <> "lang: klingon\n"

  it "reads the default config file, without editor" $ do
    let c = Conf.parseConfig $ TE.decodeUtf8 Conf.defaultConfigFileContent
    fmap Conf.editor c `shouldBe` Right Nothing
    fmap Conf.uiLang c `shouldBe` Right UI_English
    fmap (metaAccess . Conf.defaultMeta) c `shouldBe` Right (Just [("admin", AccessRead)])
    fmap Conf.bind c `shouldBe` Right (Just "127.0.0.1")

  it "reads read-only in the source sections, no by default" $ do
    let c = Conf.parseConfig $ ini [] <> "[source a]\nread-only: Yes\n[source b]\nread-only:\n\
                                         \[source c]\naccess: family\n"
    fmap (map (\sc -> (scSourceName sc, scReadOnly sc)) . Conf.sourcesConfig) c
      `shouldBe` Right [("a", True), ("b", False), ("c", False)]

  it "ignores git-commit, which is no longer read" $
    fmap Conf.port (Conf.parseConfig $ ini ["git-commit: no"]) `shouldBe` Right 3456

  it "reads the largest file sent by the editor, 20 MB by default" $ do
    Conf.maxUploadSize <$> Conf.parseConfig (ini []) `shouldBe` Right 20
    Conf.maxUploadSize <$> Conf.parseConfig (ini ["max-upload-size: 5 "]) `shouldBe` Right 5
    failsWith "invalid size \"0\", expected a number of megabytes"
      (Conf.parseConfig $ ini ["max-upload-size: 0"])
    failsWith "invalid size \"2.5\"" (Conf.parseConfig $ ini ["max-upload-size: 2.5"])

  it "reads the address to listen on, all of them without it" $ do
    fmap Conf.bind (Conf.parseConfig $ ini []) `shouldBe` Right Nothing
    fmap Conf.bind (Conf.parseConfig $ ini ["bind:"]) `shouldBe` Right Nothing
    fmap Conf.bind (Conf.parseConfig $ ini ["bind: ::1 "]) `shouldBe` Right (Just "::1")

  it "finds the agent guide file from the folder of the config file, and checks it" $
    withContentDir $ \cd -> do
      let kd = FP.takeDirectory cd
          cfp = kd FP.</> "kiwi.ini"
          conf ls = either error id $ Conf.parseConfig $ ini ls
      Conf.agentGuideFile cfp (conf []) `shouldBe` Nothing
      Conf.agentGuideFile cfp (conf ["agent-guide:"]) `shouldBe` Nothing
      Conf.agentGuideFile cfp (conf ["agent-guide: guide.md "]) `shouldBe` Just (kd FP.</> "guide.md")
      Conf.agentGuideFile cfp (conf ["agent-guide: /etc/guide.md"]) `shouldBe` Just "/etc/guide.md"
      Conf.checkAgentGuide cfp (conf []) >>= (`shouldBe` Nothing)
      missing <- Conf.checkAgentGuide cfp (conf ["agent-guide: guide.md"])
      missing `shouldSatisfy` maybe False ("guide.md (config file " `isInfixOf`)
      missing `shouldSatisfy` maybe False ("does not exist" `isInfixOf`)
      writeFile (kd FP.</> "guide.md") "Be kind\n"
      Conf.checkAgentGuide cfp (conf ["agent-guide: guide.md"]) >>= (`shouldBe` Nothing)
