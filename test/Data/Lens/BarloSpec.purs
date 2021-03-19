module Data.Lens.BarloSpec where

import Data.Lens (over, preview, view)
import Data.Lens.Barlow (barlow, key)
import Data.Maybe (Maybe(..))
import Data.String (toUpper)
import Prelude (Unit, discard)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec =
  describe "Data.Lens.Barlow" do
    describe "barlow" do
      it "should view into a record" do
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
      it "should modify a record" do
        let
          sky =
            { zodiac:
                { virgo:
                    { alpha: "Spica"
                    }
                }
            }

          expected =
            { zodiac:
                { virgo:
                    { alpha: "SPICA"
                    }
                }
            }

          actual = over (barlow (key :: _ "zodiac.virgo.alpha")) toUpper sky
        actual `shouldEqual` expected
      it "should view into a record with Maybe" do
        let
          sky =
            { zodiac:
                Just
                  { virgo:
                      Just
                        { alpha: Just "Spica"
                        }
                  }
            }

          actual = preview (barlow (key :: _ "zodiac?.virgo?.alpha?")) sky
        actual `shouldEqual` (Just "Spica")
      it "should modify a record with Maybe" do
        let
          sky =
            { zodiac:
                Just
                  { virgo:
                      Just
                        { alpha: Just "Spica"
                        }
                  }
            }

          expected =
            { zodiac:
                Just
                  { virgo:
                      Just
                        { alpha: Just "SPICA"
                        }
                  }
            }

          actual = over (barlow (key :: _ "zodiac?.virgo?.alpha?")) toUpper sky
        actual `shouldEqual` expected
      it "should view into a mixed record with Maybe" do
        let
          sky =
            { zodiac:
                Just
                  { virgo:
                      { alpha: "Spica"
                      }
                  }
            }

          actual = preview (barlow (key :: _ "zodiac?.virgo.alpha")) sky
        actual `shouldEqual` (Just "Spica")
