module Data.Lens.Barlow.GenericSpec where

import Data.Generic.Rep (class Generic)
import Data.Lens (over, preview, view)
import Data.Lens.Barlow (barlow)
import Data.Maybe (Maybe(..))
import Data.Show (class Show)
import Data.Show.Generic (genericShow)
import Data.String (toUpper)
import Data.Unit (unit)
import Prelude (Unit, discard)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Data.Eq (class Eq)

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
  = Virgo4 { alpha :: String }
  | Carina4 { beta :: String }

derive instance genericZodiac4 :: Generic Zodiac4 _

instance showZodiac4 :: Show Zodiac4 where
  show = genericShow

data Zodiac5
  = Carina5
  | Virgo5 { alpha :: String } { beta :: String } { gamma :: String } { delta :: String }

derive instance genericZodiac5 :: Generic Zodiac5 _

instance showZodiac5 :: Show Zodiac5 where
  show = genericShow

data Zodiac6
  = Carina6
  | Virgo6
  | CanisMaior6
  | Cassiopeia6
  | Centaurus6 String

derive instance genericZodiac6 :: Generic Zodiac6 _

instance showZodiac6 :: Show Zodiac6 where
  show = genericShow

data Zodiac7
  = Carina7 String
  | Virgo7 String
  | CanisMaior7 String

derive instance genericZodiac7 :: Generic Zodiac7 _

instance showZodiac7 :: Show Zodiac7 where
  show = genericShow

data Zodiac8
  = Virgo8 { alpha :: String } { beta :: String } { gamma :: String } { delta :: String }

derive instance genericZodiac8 :: Generic Zodiac8 _

instance showZodiac8 :: Show Zodiac8 where
  show = genericShow

data Zodiac9
  = Carina9 { alpha :: String }
  | Virgo9 { alpha :: String } { beta :: String } { gamma :: String } { delta :: String }
  | CanisMaior9 String

derive instance genericZodiac9 :: Generic Zodiac9 _

derive instance eqZodiac9 :: Eq Zodiac9

instance showZodiac9 :: Show Zodiac9 where
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

          actual = view (barlow @"zodiac.%Virgo1") sky
        actual `shouldEqual` unit
      it "should view into a constructor with one argument" do
        let
          sky =
            { zodiac:
                { virgo: Virgo2 { alpha: "Spica" }
                }
            }

          actual = view (barlow @"zodiac.virgo.%Virgo2.alpha") sky
        actual `shouldEqual` "Spica"
      it "should view into a constructor with multiple arguments using the left most" do
        let
          sky =
            { zodiac:
                { virgo: Virgo3 { alpha: "Spica" } { beta: "β Vir" }
                }
            }

          actual = view (barlow @"zodiac.virgo.%Virgo3.%1.alpha") sky
        actual `shouldEqual` "Spica"
      it "should view into a sum type left case" do
        let
          sky =
            { zodiac:
                { virgo:
                    Virgo4
                      { alpha: "Spica"
                      }
                }
            }

          actual = preview (barlow @"zodiac.virgo.%Virgo4.alpha") sky
        actual `shouldEqual` (Just "Spica")
      it "should view into a sum type right case" do
        let
          sky =
            { zodiac:
                { virgo:
                    Carina4
                      { beta: "β Car"
                      }
                }
            }

          actual = preview (barlow @"zodiac.virgo.%Carina4.beta") sky
        actual `shouldEqual` (Just "β Car")
      it "should view into a sum with product type right case" do
        let
          sky =
            { zodiac:
                { virgo:
                    Virgo5 { alpha: "Spica" } { beta: "β Vir" } { gamma: "γ Vir B" } { delta: "δ Vir" }
                }
            }

          actual = preview (barlow @"zodiac.virgo.%Virgo5.%4.delta") sky
        actual `shouldEqual` (Just "δ Vir")
      it "should view into a sum with product type right case" do
        let
          sky =
            { zodiac: Centaurus6 "Rigil Kentaurus"
            }

          actual = preview (barlow @"zodiac.%Centaurus6") sky
        actual `shouldEqual` (Just "Rigil Kentaurus")
      it "should view into a sum " do
        let
          sky =
            { zodiac: CanisMaior7 "Sirius"
            }

          actual = preview (barlow @"zodiac.%CanisMaior7") sky
        actual `shouldEqual` (Just "Sirius")
      it "should view into a product type" do
        let
          sky =
            { zodiac:
                { virgo:
                    Virgo8 { alpha: "Spica" } { beta: "β Vir" } { gamma: "γ Vir B" } { delta: "δ Vir" }
                }
            }

          actual = preview (barlow @"zodiac.virgo.%Virgo8.%4.delta") sky
        actual `shouldEqual` (Just "δ Vir")
      it "should view into a sum with product type right case" do
        let
          sky =
            { zodiac:
                Virgo9 { alpha: "Spica" } { beta: "β Vir" } { gamma: "γ Vir B" } { delta: "δ Vir" }
            }

          actual = over (barlow @"zodiac.%Virgo9.%4.delta") toUpper sky
        actual
          `shouldEqual`
            { zodiac:
                Virgo9 { alpha: "Spica" } { beta: "β Vir" } { gamma: "γ Vir B" } { delta: "Δ VIR" }
            }
