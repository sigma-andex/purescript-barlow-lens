module Data.Lens.Barlow.AccessSpec where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Lens (over, preview, set, view)
import Data.Lens.Barlow (barlow)
import Data.Lens.Barlow.Helpers as Barlow
import Data.List (List(..), (:))
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.String (toUpper)
import Data.Tuple (Tuple(..))
import Foreign.Object as Object
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

data Star = Star String Int

derive instance Generic Star _
derive instance Eq Star
instance Show Star where
  show (Star n m) = "Star " <> n <> " " <> show m

-- | A constructor with more than 9 arguments
data Catalogue = Catalogue Int Int Int Int Int Int Int Int Int String { name :: String } Int

derive instance Generic Catalogue _

catalogue :: Catalogue
catalogue = Catalogue 1 2 3 4 5 6 7 8 9 "ten" { name: "eleven" } 12

spec :: Spec Unit
spec =
  describe "Data.Lens.Barlow" do
    describe "[key] and [?key]" do
      let
        sky =
          { zodiac: [ { name: "Virgo" }, { name: "Leo" } ]
          , named: Map.fromFoldable [ Tuple "virgo" "Spica", Tuple "leo" "Regulus" ]
          , numbered: Map.fromFoldable [ Tuple 1 "one", Tuple 23 "twenty-three" ]
          , object: Object.fromFoldable [ Tuple "virgo" "Spica" ]
          }
      it "should index into an array" do
        preview (barlow @"zodiac[1].name") sky `shouldEqual` Just "Leo"
        preview (barlow @"zodiac[5].name") sky `shouldEqual` Nothing
        (over (barlow @"zodiac[0].name") toUpper sky).zodiac `shouldEqual` [ { name: "VIRGO" }, { name: "Leo" } ]
      it "should index into maps and objects" do
        Barlow.preview @"named[virgo]" sky `shouldEqual` Just "Spica"
        Barlow.preview @"numbered[23]" sky `shouldEqual` Just "twenty-three"
        Barlow.preview @"object[virgo]" sky `shouldEqual` Just "Spica"
      it "should insert and delete with [?key]" do
        Barlow.view @"named[?leo]" sky `shouldEqual` Just "Regulus"
        Barlow.view @"named[?cancer]" sky `shouldEqual` Nothing
        (Barlow.set @"named[?cancer]" (Just "Tarf") sky).named
          `shouldEqual` Map.fromFoldable [ Tuple "virgo" "Spica", Tuple "leo" "Regulus", Tuple "cancer" "Tarf" ]
        (Barlow.set @"named[?leo]" Nothing sky).named
          `shouldEqual` Map.fromFoldable [ Tuple "virgo" "Spica" ]
        Barlow.preview @"named[?leo]?" sky `shouldEqual` Just "Regulus"
      it "should allow special characters in record fields via [field]" do
        let
          weird = { "alpha.centauri": { "a+b": "Rigil Kentaurus" } }
        view (barlow @"[alpha.centauri][a+b]") weird `shouldEqual` "Rigil Kentaurus"
        Barlow.over @"[alpha.centauri][a+b]" toUpper weird `shouldEqual` { "alpha.centauri": { "a+b": "RIGIL KENTAURUS" } }

    describe "{fields}" do
      let
        sky = { zodiac: { virgo: "Spica", leo: "Regulus", cancer: "Tarf", count: 3 } }
      it "should view a sub-record" do
        Barlow.view @"zodiac.{virgo, leo}" sky `shouldEqual` { virgo: "Spica", leo: "Regulus" }
        Barlow.view @"zodiac.{count}" sky `shouldEqual` { count: 3 }
      it "should set a sub-record" do
        Barlow.set @"zodiac.{virgo,count}" { virgo: "α Vir", count: 4 } sky
          `shouldEqual` { zodiac: { virgo: "α Vir", leo: "Regulus", cancer: "Tarf", count: 4 } }
      it "should continue the path after a sub-record" do
        Barlow.over @"zodiac.{virgo,leo}+" toUpper sky
          `shouldEqual` { zodiac: { virgo: "SPICA", leo: "REGULUS", cancer: "Tarf", count: 3 } }

    describe "+ on records" do
      it "should traverse all fields of a homogeneous record" do
        let
          stars = { virgo: { alpha: "Spica", beta: "Zavijava" } }
        Barlow.toListOf @"virgo+" stars `shouldEqual` ("Spica" : "Zavijava" : Nil)
        Barlow.over @"virgo+" toUpper stars `shouldEqual` { virgo: { alpha: "SPICA", beta: "ZAVIJAVA" } }

    describe "tuples" do
      it "should view and change types in tuples" do
        let
          pair = { pair: Tuple "Spica" 1 }
        view (barlow @"pair.%1") pair `shouldEqual` "Spica"
        view (barlow @"pair.%2") pair `shouldEqual` 1
        set (barlow @"pair.%2") "one" pair `shouldEqual` { pair: Tuple "Spica" "one" }
        over (barlow @"pair.%1") (const 42) pair `shouldEqual` { pair: Tuple 42 1 }
      it "should go into nested tuples" do
        let
          triple = Tuple "a" (Tuple { name: "b" } true)
        Barlow.view @"%2.%1.name" triple `shouldEqual` "b"
      it "should still support other generic products" do
        Barlow.view @"%Star.%2" (Star "Spica" 1) `shouldEqual` 1

    describe "more helpers" do
      let
        sky = { zodiac: [ { name: "Virgo" }, { name: "Leo" } ] }
      it "should provide fold helpers" do
        Barlow.firstOf @"zodiac+.name" sky `shouldEqual` Just "Virgo"
        Barlow.lastOf @"zodiac+.name" sky `shouldEqual` Just "Leo"
        Barlow.lengthOf @"zodiac+" sky `shouldEqual` 2
        Barlow.has @"zodiac[1]" sky `shouldEqual` true
        Barlow.hasn't @"zodiac[2]" sky `shouldEqual` true
        Barlow.toListOf @"zodiac+.name" sky `shouldEqual` ("Virgo" : "Leo" : Nil)
      it "should provide traverseOf" do
        Barlow.traverseOf @"zodiac+.name" (\n -> if n == "" then Nothing else Just (toUpper n)) sky
          `shouldEqual` Just { zodiac: [ { name: "VIRGO" }, { name: "LEO" } ] }

    describe "product indices above 9" do
      it "should zoom into the 10th and later arguments" do
        Barlow.view @"%Catalogue.%10" catalogue `shouldEqual` "ten"
        Barlow.view @"%Catalogue.%11.name" catalogue `shouldEqual` "eleven"
        Barlow.view @"%Catalogue.%12" catalogue `shouldEqual` 12
        Barlow.view @"%Catalogue.%1" catalogue `shouldEqual` 1
      it "should modify the 11th argument" do
        Barlow.view @"%Catalogue.%11.name" (Barlow.set @"%Catalogue.%11.name" "XI" catalogue) `shouldEqual` "XI"
