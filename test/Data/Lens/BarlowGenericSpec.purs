module Data.Lens.BarloGenericSpec where

import Data.Generic.Rep (class Generic)
import Data.Lens (view)
import Data.Lens.Barlow (barlow, key)
import Data.Show (class Show)
import Data.Show.Generic (genericShow)
import Data.Unit (unit)
import Prelude (Unit, discard)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

data Zodiac1
  = Virgo1 

derive instance genericZodiac1 :: Generic Zodiac1 _

instance showZodiac1 :: Show Zodiac1 where
  show = genericShow

data Zodiac2
  = Virgo2 { alpha :: String }

derive instance genericZodiac2 :: Generic Zodiac2 _

instance showZodiac2 :: Show Zodiac2 where
  show = genericShow

data Zodiac3
  = Virgo3 { alpha :: String } { beta :: String }

derive instance genericZodiac3 :: Generic Zodiac3 _

instance showZodiac3 :: Show Zodiac3 where
  show = genericShow

data Zodiac4
  = Virgo4 { alpha :: String } | Carina4 { beta :: String } 

derive instance genericZodiac4 :: Generic Zodiac4 _

instance showZodiac4 :: Show Zodiac4 where
  show = genericShow

data Zodiac5
  = Carina5 | Virgo5 { alpha :: String } { beta :: String } { gamma:: String } { delta :: String }

derive instance genericZodiac5 :: Generic Zodiac5 _

instance showZodiac5 :: Show Zodiac5 where
  show = genericShow


spec :: Spec Unit
spec =
  describe "Data.Lens.Barlow" do
    describe "barlow - generic" do
      it "should view into a constructor with no arguments" do
        let
          sky =
            { zodiac: Virgo1
            }

          actual = view (barlow (key :: _ "zodiac<")) sky
        actual `shouldEqual` unit
      it "should view into a constructor with one argument" do
        let
          sky =
            { zodiac: { 
              virgo : Virgo2 { alpha : "Spica"}
            }
            }

          actual = view (barlow (key :: _ "zodiac.virgo<.alpha")) sky
        actual `shouldEqual` "Spica"
      it "should view into a constructor with multiple arguments using the left most" do
        let
          sky =
            { zodiac: { 
              virgo : Virgo3 { alpha : "Spica"} { beta : "β Vir"}
            }
            }

          actual = view (barlow (key :: _ "zodiac.virgo<.alpha")) sky
        actual `shouldEqual` "Spica"
      it "should view into a sum type left case" do
        let
          sky =
            { zodiac:
                { virgo:
                    Virgo4 { alpha: "Spica"
                    }
                }
            }

          actual = view (barlow (key :: _ "zodiac.virgo<<.alpha")) sky
        actual `shouldEqual` "Spica"
      it "should view into a sum type right case" do
        let
          sky =
            { zodiac:
                { virgo:
                    Carina4 { beta: "β Car"
                    }
                }
            }

          actual = view (barlow (key :: _ "zodiac.virgo><.beta")) sky
        actual `shouldEqual` "β Car"
      it "should view into a sum with product type right case" do
        let
          sky =
            { zodiac:
                { virgo:
                    Virgo5 { alpha : "Spica"} { beta: "β Vir"} { gamma: "γ Vir B"} { delta: "δ Vir"}
                }
            }

          actual = view (barlow (key :: _ "zodiac.virgo>.>>><.delta")) sky
        actual `shouldEqual` "δ Vir"
