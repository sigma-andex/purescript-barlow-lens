module Data.Lens.BarloSpec where

import Data.Lens (view)
import Data.Lens.Barlow (barlow, key)
import Prelude (Unit)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec =
  describe "Data.Lens.Barlow" do
    describe "barlow" do
      it "should zoom into a record" do
        let
          sky =
            { zodiac:
                { virgo:
                    { alpha: "Spica"
                    }
                }
            }

          actual = view (barlow (key :: _ "zodiac.virgo.alpha")) sky
        actual `shouldEqual` "Spica"
