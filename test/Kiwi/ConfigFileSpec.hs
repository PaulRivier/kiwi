module Kiwi.ConfigFileSpec (configSpec) where

import           Data.List (isInfixOf)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import           Test.Hspec

import qualified Kiwi.ConfigFile as Conf
import           Kiwi.Types
import qualified Kiwi.Utils as U

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
    fmap (metaAccess . Conf.defaultMeta) c `shouldBe` Right (Just ["admin"])

  it "reads ui-lang, toc, editor and the custom metadata types" $ do
    let c = Conf.parseConfig $ ini [ "ui-lang: french", "toc: off"
                                   , "editor: emacsclient -n -s kiwi" ]
                               <> "[metadata year]\ntype: int\n"
    fmap Conf.uiLang c `shouldBe` Right UI_French
    fmap Conf.toc c `shouldBe` Right TocOff
    fmap Conf.editor c `shouldBe` Right (Just ("emacsclient", ["-n", "-s", "kiwi"]))
    fmap (map cmcType . Conf.customMetaConfig) c `shouldBe` Right [CmtInt]
    -- an empty editor disables the edition, as no editor
    fmap Conf.editor (Conf.parseConfig $ ini ["editor:"]) `shouldBe` Right Nothing

  it "refuses invalid values, with the allowed ones" $ do
    failsWith "allowed values: english, french" $ Conf.parseConfig $ ini ["ui-lang: german"]
    failsWith "allowed values: on, off, auto" $ Conf.parseConfig $ ini ["toc: yes"]
    failsWith "allowed values: text, int, date, bool" $ Conf.parseConfig $
      ini [] <> "[metadata year]\ntype: float\n"
    failsWith "allowed values: danish" $ Conf.parseConfig $ ini [] <> "lang: klingon\n"

  it "reads the default config file, without editor" $ do
    let c = Conf.parseConfig $ TE.decodeUtf8 Conf.defaultConfigFileContent
    fmap Conf.editor c `shouldBe` Right Nothing
    fmap Conf.uiLang c `shouldBe` Right UI_English
    fmap (metaAccess . Conf.defaultMeta) c `shouldBe` Right (Just ["admin"])
    fmap Conf.bind c `shouldBe` Right (Just "127.0.0.1")

  it "reads the address to listen on, all of them without it" $ do
    fmap Conf.bind (Conf.parseConfig $ ini []) `shouldBe` Right Nothing
    fmap Conf.bind (Conf.parseConfig $ ini ["bind:"]) `shouldBe` Right Nothing
    fmap Conf.bind (Conf.parseConfig $ ini ["bind: ::1 "]) `shouldBe` Right (Just "::1")
