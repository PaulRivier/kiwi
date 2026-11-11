module Kiwi.HtmlWriterSpec (htmlWriterSpec) where

import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import           Lucid (renderText)
import           Test.Hspec

import           Kiwi.HtmlWriter (writeHtml5)
import           Kiwi.Types
import           TestUtils

htmlWriterSpec :: Spec
htmlWriterSpec = do
  let html = TL.toStrict . renderText . writeHtml5 (const True) . pandocDoc . load

  it "marks the links to missing pages as broken, without their target attributes" $ do
    let exists uid = uid == ("src", "sub:Here")
        render = TL.toStrict . renderText . writeHtml5 exists . pandocDoc . load
    render "t: t\n\n[a](<page:Here#x>) [b](<page:Gone>) [c](http://e.org)"
      `shouldBe` T.concat
        [ "<p><a href=\"/page/src/sub:Here#x\" class=\"kiwi-link-page\">a</a> "
        , "<a href=\"/page/src/sub:Gone\" class=\"kiwi-link-page kiwi-link-broken\">b</a> "
        , "<a href=\"http://e.org\" class=\"kiwi-link-external\" target=\"_blank\">c</a></p>" ]

  it "renders footnotes as numbered references and a list at the end" $
    html "t: t\n\nA[^x] and B[^y].\n\n[^x]: Note x.\n\n[^y]: Note y."
      `shouldBe` T.concat
        [ "<p>A<a href=\"#fn1\" id=\"fnref1\" class=\"footnote-ref\">1</a>"
        , " and B<a href=\"#fn2\" id=\"fnref2\" class=\"footnote-ref\">2</a>.</p>"
        , "<section class=\"footnotes\"><ol>"
        , "<li id=\"fn1\"><p>Note x. <a href=\"#fnref1\" class=\"footnote-back\">↑</a></p></li>"
        , "<li id=\"fn2\"><p>Note y. <a href=\"#fnref2\" class=\"footnote-back\">↑</a></p></li>"
        , "</ol></section>" ]

  it "renders no footnotes section without notes" $
    html "t: t\n\nNo note." `shouldBe` "<p>No note.</p>"

  it "renders a lone image as a figure, captioned by its description" $
    html "t: t\n\n![A *nice* `cat`](http://x/cat.png \"Cat\")"
      `shouldBe` T.concat
        [ "<figure><img src=\"http://x/cat.png\" alt=\"\" title=\"Cat\">"
        , "<figcaption>A <em>nice</em> <code>cat</code></figcaption></figure>" ]

  it "renders no caption for a lone image without description" $
    html "t: t\n\n![](http://x/cat.png)"
      `shouldBe` "<figure><img src=\"http://x/cat.png\" alt=\"\"></figure>"

  it "gives an inline image the full text of its description as alt" $
    html "t: t\n\nSee ![A *nice* `cat`](http://x/cat.png)."
      `shouldBe` "<p>See <img src=\"http://x/cat.png\" alt=\"A nice cat\">.</p>"

  it "aligns table cells with classes" $ do
    let t = html "t: t\n\n| a | b | c |\n|:--|:-:|--:|\n| 1 | 2 | 3 |\n"
    t `shouldSatisfy` T.isInfixOf "<th scope=\"col\" class=\"align-left\">a</th>"
    t `shouldSatisfy` T.isInfixOf "<td class=\"align-left\">1</td>"
    t `shouldSatisfy` T.isInfixOf "<td class=\"align-center\">2</td>"
    t `shouldSatisfy` T.isInfixOf "<td class=\"align-right\">3</td>"
    t `shouldSatisfy` (not . T.isInfixOf "style=")

