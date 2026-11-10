module Data.Lens.Barlow.NewtypeSpec where

import Prelude

import Data.Lens (view)
import Data.Lens.Barlow (barlow)
import Data.Lens.Barlow.Helpers as Barlow
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.String (toUpper)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

newtype Star = Star { name :: String, magnitude :: Number }

derive instance Newtype Star _
derive newtype instance Eq Star
derive newtype instance Show Star

newtype Virgo = Virgo { alpha :: Star, stars :: Array Star }

derive instance Newtype Virgo _
derive newtype instance Eq Virgo
derive newtype instance Show Virgo

-- a newtype around a newtype around a record
newtype Brightest = Brightest Star

derive instance Newtype Brightest _

spica :: Star
spica = Star { name: "Spica", magnitude: 0.97 }

porrima :: Star
porrima = Star { name: "Porrima", magnitude: 2.74 }

sky :: { zodiac :: { virgo :: Maybe Virgo } }
sky = { zodiac: { virgo: Just (Virgo { alpha: spica, stars: [ spica, porrima ] }) } }

spec :: Spec Unit
spec =
  describe "Data.Lens.Barlow" do
    describe "newtypes around records" do
      it "should unwrap newtypes automatically when selecting a field" do
        Barlow.preview @"zodiac.virgo?.alpha.name" sky `shouldEqual` Just "Spica"
        view (barlow @"name") spica `shouldEqual` "Spica"
      it "should still support an explicit `!`" do
        Barlow.preview @"zodiac.virgo?!.alpha!.name" sky `shouldEqual` Just "Spica"
      it "should modify through newtypes" do
        Barlow.preview @"zodiac.virgo?.alpha.name" (Barlow.over @"zodiac.virgo?.alpha.name" toUpper sky)
          `shouldEqual` Just "SPICA"
        Barlow.toArrayOf @"zodiac.virgo?.stars+.name" (Barlow.over @"zodiac.virgo?.stars+.name" toUpper sky)
          `shouldEqual` [ "SPICA", "PORRIMA" ]
      it "should unwrap nested newtypes" do
        Barlow.view @"name" (Brightest spica) `shouldEqual` "Spica"
      it "should select sub-records of newtypes" do
        Barlow.view @"{name}" spica `shouldEqual` { name: "Spica" }
        Barlow.set @"{magnitude}" { magnitude: 1.0 } spica `shouldEqual` Star { name: "Spica", magnitude: 1.0 }
