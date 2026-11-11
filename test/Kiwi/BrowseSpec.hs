module Kiwi.BrowseSpec (browseReqSpec) where

import           Data.Either (isLeft)
import qualified Data.Map as M
import           Data.Maybe (fromMaybe)
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Time as Time
import qualified Network.HTTP.Types.URI as URI
import           Test.Hspec

import qualified Kiwi.Browse as Browse
import           Kiwi.Types
import qualified Kiwi.Url as Url
import qualified Utils.DocIndex as DI

browseReqSpec :: Spec
browseReqSpec = do
  let cmcs = [ CustomMetaConfig "author" CmtText 1
             , CustomMetaConfig "year" CmtInt 1
             , CustomMetaConfig "date" CmtDate 1
             , CustomMetaConfig "done" CmtBool 1 ]
      -- decode the query string of an URL, as scotty does
      decodeUrl url = Browse.decodeReq cmcs $ map (fmap $ fromMaybe "") $
                      URI.parseQueryText $ TE.encodeUtf8 $ T.dropWhile (/= '?') url
      req = M.fromList . map (fmap S.fromList)

  it "roundtrips browseUrl / decodeReq" $ do
    let r = req [ (FieldTag, [KeyText "a>b", KeyText "x\\>y", KeyText "é ç/&?=+#%;"])
                , (FieldCustom "author", [KeyText "Smith & Wesson"])
                , (FieldCustom "year", [KeyInt 1999, KeyInt (-3)])
                , (FieldCustom "date", [KeyDay (Time.fromGregorian 2020 2 29)])
                , (FieldCustom "done", [KeyBool True])
                , (FieldAccess, [KeyText "family"])
                , (FieldLang, [KeyLang French])
                , (FieldSource, [KeyText "my source"]) ]
    decodeUrl (Browse.browseUrl noContext r) `shouldBe` Right r
    Browse.browseUrl noContext (req [(FieldLang, [KeyLang French])])
      `shouldBe` "/browse?lang=french"

  it "builds readable URLs, in a stable order" $ do
    Browse.browseUrl noContext M.empty `shouldBe` "/browse"
    Browse.browseUrl noContext (req [ (FieldCustom "done", [KeyBool False])
                     , (FieldTag, [KeyText "maison>produits"])
                     , (FieldSource, [KeyText "a-p"])
                     , (FieldCustom "date", [KeyDay (Time.fromGregorian 2020 2 9)]) ])
      `shouldBe` "/browse?source=a-p&tag=maison%3Eproduits&m.date=2020-02-09&m.done=false"

  it "keeps the archive mode of the context in browse URLs, not its sources" $
    Browse.browseUrl (Context (S.fromList ["x"]) True) (req [(FieldTag, [KeyText "t"])])
      `shouldBe` "/browse?tag=t&archive=yes"

  it "reads the keys of custom fields as in the pages" $
    -- the booleans of the URLs made by kiwi are true and false
    map decodeUrl ["/browse?m.done=yes", "/browse?m.done=1", "/browse?m.done=True"]
      `shouldBe` replicate 3 (decodeUrl "/browse?m.done=true")

  it "refuses unknown parameters and invalid values" $ do
    mapM_ (\url -> decodeUrl url `shouldSatisfy` isLeft)
      [ "/browse?foo=bar", "/browse?author=Smith", "/browse?m.unknown=x"
      , "/browse?m.year=12a", "/browse?m.date=2020-02-30", "/browse?m.done=oui"
      , "/browse?tag=", "/browse?tag=>", "/browse?source="
      , "/browse?lang=klingon", "/browse?access=" ]

  it "normalizes the tags of a request" $ do
    decodeUrl "/browse?tag=maison%20%3E%20produits"
      `shouldBe` Right (req [(FieldTag, [KeyText "maison>produits"])])
    decodeUrl "/browse?tag=a&tag=a%3Eb&tag=a%3Eb%3Ec&tag=ab&tag=b"
      `shouldBe` Right (req [(FieldTag, [KeyText "a>b>c", KeyText "ab", KeyText "b"])])

  it "adds keys to a request, without the parents of a tag" $ do
    Browse.addToReq (Browse.createReq (FieldTag, KeyText "a")) (FieldTag, KeyText "b")
      `shouldBe` req [(FieldTag, [KeyText "a", KeyText "b"])]
    Browse.addToReq (Browse.createReq (FieldTag, KeyText "a")) (FieldTag, KeyText "a>b")
      `shouldBe` req [(FieldTag, [KeyText "a>b"])]
    Browse.addToReq (Browse.createReq (FieldTag, KeyText "a\\>b")) (FieldTag, KeyText "a>b")
      `shouldBe` req [(FieldTag, [KeyText "a\\>b", KeyText "a>b"])]

  it "combines sources with an union, the other fields with an intersection" $
    Browse.reqToQuery (req [ (FieldSource, [KeyText "a", KeyText "b"])
                      , (FieldTag, [KeyText "x", KeyText "y"]) ])
      `shouldBe` DI.And [ DI.Any FieldSource (S.fromList [KeyText "a", KeyText "b"])
                        , DI.All FieldTag (S.fromList [KeyText "x", KeyText "y"]) ]

  it "adds the navigation context to URLs, before the fragment" $ do
    let ss = S.fromList ["b c", "a"]
    Url.inContext noContext "/page/s/p#h" `shouldBe` "/page/s/p#h"
    Url.inContext (Context ss False) "/page/s/p#h"
      `shouldBe` "/page/s/p?source=a&source=b%20c#h"
    Url.inContext (Context ss True) "/page/s/p"
      `shouldBe` "/page/s/p?source=a&source=b%20c&archive=yes"
    Url.inContext (Context S.empty True) "/metadata" `shouldBe` "/metadata?archive=yes"
    Url.inContext (Context ss True) "/browse"
      `shouldBe` Browse.browseUrl (Context ss True) (Browse.sourcesReq ss)

  it "removes keys from a request" $ do
    let r = req [(FieldTag, [KeyText "a", KeyText "b"]), (FieldSource, [KeyText "s"])]
    Browse.removeFromReq r (FieldTag, KeyText "a")
      `shouldBe` req [(FieldTag, [KeyText "b"]), (FieldSource, [KeyText "s"])]
    Browse.removeFromReq r (FieldSource, KeyText "s")
      `shouldBe` req [(FieldTag, [KeyText "a", KeyText "b"])]

