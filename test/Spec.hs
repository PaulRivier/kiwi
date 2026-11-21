module Main (main) where

import           Test.Hspec

import           Kiwi.ApiSpec (apiSpec)
import           Kiwi.AuthSpec (passwordSpec, accountsSpec, sessionsSpec, readableQuerySpec)
import           Kiwi.BrowseSpec (browseReqSpec)
import           Kiwi.ConfigFileSpec (configSpec)
import           Kiwi.ControllerSpec (metaIndexSpec)
import           Kiwi.EditSpec (editSpec)
import           Kiwi.FilesSpec (filesSpec)
import           Kiwi.GitSpec (gitSpec)
import           Kiwi.HtmlWriterSpec (htmlWriterSpec)
import           Kiwi.KeywordsSpec (keywordsSpec)
import           Kiwi.LangDetectSpec (langDetectSpec)
import           Kiwi.McpSpec (mcpSpec)
import           Kiwi.MetaScriptSpec (metaScriptSpec)
import           Kiwi.MetaSpec (metaSpec)
import           Kiwi.PagesDBSpec (pagesDBSpec)
import           Kiwi.PandocSpec (loadPageSpec, metaDataSpec)
import           Kiwi.SearchSpec (searchSpec)
import           Kiwi.TagsSpec (tagsSpec)
import           Kiwi.ViewSpec (viewSpec)
import           Utils.DocIndexSpec (docIndexSpec)


main :: IO ()
main = hspec $ do
  describe "Kiwi.Pandoc.loadPage" loadPageSpec
  describe "Kiwi.Pandoc metadata" metaDataSpec
  describe "Kiwi.Meta" metaSpec
  describe "Kiwi.ConfigFile" configSpec
  describe "Kiwi.Tags" tagsSpec
  describe "Kiwi.Browse" browseReqSpec
  describe "Kiwi.Auth passwords" passwordSpec
  describe "Kiwi.Auth accounts" accountsSpec
  describe "Kiwi.Auth sessions" sessionsSpec
  describe "Kiwi.Auth.readableQuery" readableQuerySpec
  describe "Kiwi.Controller.metaIndex" metaIndexSpec
  describe "Utils.DocIndex" docIndexSpec
  describe "Kiwi.LangDetect" langDetectSpec
  describe "Kiwi.Search" searchSpec
  describe "Kiwi.Keywords" keywordsSpec
  describe "Kiwi.Files" filesSpec
  describe "Kiwi.PagesDB.updatePagesDB" pagesDBSpec
  describe "Kiwi.HtmlWriter" htmlWriterSpec
  describe "Kiwi.View and Kiwi.Server" viewSpec
  describe "Kiwi.Edit and the page editor" editSpec
  describe "Kiwi.Git.commitFiles" gitSpec
  describe "Kiwi.Api" apiSpec
  describe "Kiwi.Mcp" mcpSpec
  describe "Kiwi.MetaScript" metaScriptSpec
