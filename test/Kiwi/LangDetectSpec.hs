module Kiwi.LangDetectSpec (langDetectSpec) where

import qualified Data.Set as S
import           Test.Hspec

import           Kiwi.LangDetect (detectLang, resolveLang, stopWords)
import           Kiwi.Types

langDetectSpec :: Spec
langDetectSpec = do
  it "has disjoint lists of words" $ do
    let sets = [ S.fromList ws | (_, ws) <- stopWords ]
    sum (map S.size sets) `shouldBe` S.size (S.unions sets)

  it "detects french, english and spanish" $ do
    detectLang "Le chat est sur le toit, et il n'en descendra pas avant ce soir."
      `shouldBe` Just French
    detectLang "The cat is on the roof, and it will not come down before tonight."
      `shouldBe` Just English
    detectLang "El gato está en el tejado y no bajará hasta la noche, como siempre."
      `shouldBe` Just Spanish

  it "tells french and spanish apart, with accents and elisions" $ do
    detectLang "C'est l'été : qu'il fasse beau, ça m'est égal, j'irai déjà à la plage."
      `shouldBe` Just French
    detectLang "¿Qué es eso? Él dijo que también vendría, pero todavía no está aquí."
      `shouldBe` Just Spanish

  it "gives up on short or mixed texts" $ do
    detectLang "Recette" `shouldBe` Nothing
    detectLang "the cat" `shouldBe` Nothing
    detectLang "The cat and the dog. Le chat et le chien." `shouldBe` Nothing
    detectLang "" `shouldBe` Nothing

  it "resolves the lang of a page from its title and content" $ do
    let fr = "Une recette pour les jours de pluie, avec du chocolat et des noix."
    resolveLang (LangFixed English) "Gâteau" fr `shouldBe` English
    resolveLang (LangAuto English) "Gâteau" fr `shouldBe` French
    resolveLang (LangAuto Spanish) "Cake" "" `shouldBe` Spanish
    resolveLang (LangAuto Spanish) "The joy of the cake and its icing" ""
      `shouldBe` English

