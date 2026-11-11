module Main (main) where

import           Test.Hspec

import           Kiwi.AuthSpec (passwordSpec, accountsSpec, sessionsSpec, readableQuerySpec)
import           Kiwi.BrowseSpec (browseReqSpec)
import           Kiwi.ConfigFileSpec (configSpec)
import           Kiwi.ControllerSpec (metaIndexSpec)
import           Kiwi.HtmlWriterSpec (htmlWriterSpec)
import           Kiwi.LangDetectSpec (langDetectSpec)
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
  describe "Kiwi.PagesDB.updatePagesDB" pagesDBSpec
  describe "Kiwi.HtmlWriter" htmlWriterSpec
  describe "Kiwi.View and Kiwi.Server" viewSpec
