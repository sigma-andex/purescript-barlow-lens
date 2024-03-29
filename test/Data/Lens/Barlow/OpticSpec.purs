module Data.Lens.Barlow.OpticSpec where

import Data.Either (Either(..))
import Data.Lens (over)
import Data.Lens.Barlow (barlow)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Prelude (Unit, discard, show, (<>), (>>>))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

newtype AlphaR
  = AlphaR { alpha :: String }

instance alphaRNT :: Newtype AlphaR { alpha :: String }

newtype Alpha
  = Alpha String

instance alphaNT :: Newtype Alpha String

spec :: Spec Unit
spec =
  describe "Data.Lens.Barlow" do
    describe "barlow" do
      it "should modify a record with changed types" do
        let
          sky =
            { zodiac:
                { sagittarius:
                    [ { id: 1
                      , name: "Rukbat"
                      }
                    , { id: 2
                      , name: "Arkab"
                      }
                    , { id: 3
                      , name: "Nash"
                      }
                    ]
                }
            }

          expected =
            { zodiac:
                { sagittarius:
                    [ { id: "1"
                      , name: "Rukbat"
                      }
                    , { id: "2"
                      , name: "Arkab"
                      }
                    , { id: "3"
                      , name: "Nash"
                      }
                    ]
                }
            }

          actual = over (barlow @"zodiac.sagittarius+.id") show sky
        actual `shouldEqual` expected
      it "should modify a record with changed types (2)" do
        let
          sky :: { zodiac :: Either { sagittarius :: Maybe { rukbat :: Either String { id :: Int } } } Boolean }
          sky =
            { zodiac:
                Left { sagittarius: Just { rukbat: Right { id: 1 } } }
            }

          expected =
            { zodiac:
                Left { sagittarius: Just { rukbat: Right { id: "1 !!!" } } }
            }

          actual = over (barlow @"zodiac<.sagittarius?.rukbat>.id") (show >>> (_ <> " !!!")) sky
        actual `shouldEqual` expected
      it "should modify a record with changed types (3)" do
        let
          sky :: { zodiac :: { sagittarius :: { alpha :: String } } }
          sky = { zodiac: { sagittarius: { alpha: "Rukbat" } } }

          expected :: { zodiac :: { sagittarius :: { first :: String } } }
          expected = { zodiac: { sagittarius: { first: "Rukbat" } } }

          actual = over (barlow @"zodiac.sagittarius") (\{ alpha } -> { first: alpha }) sky
        actual `shouldEqual` expected
