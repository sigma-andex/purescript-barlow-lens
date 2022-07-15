module Data.Lens.Barlow.HelpersSpec where

import Prelude

import Data.Either (Either(..))
import Data.Lens.Barlow.Helpers as Barlow
import Data.Maybe (Maybe(..))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec =
  describe "Data.Lens.Barlow.Helpers" do
    describe "viewB" do
      it "should view into a record" do
        let
          sky =
            { zodiac:
                { sagittarius:
                    { alpha: "Rukbat"
                    }
                }
            }
        Barlow.view @"zodiac.sagittarius.alpha" sky `shouldEqual` "Rukbat"
      it "should previewB into a record" do
        let
          sky =
            { zodiac:
                Left
                  { sagittarius:
                      Just
                        { alpha: Right "Rukbat"
                        }
                  }
            }
        Barlow.preview @"zodiac<.sagittarius?.alpha>" sky `shouldEqual` (Just "Rukbat")
      it "should toArrayOfB into a record" do
        let
          sky =
            { zodiac:
                { sagittarius:
                    { nebula: [ "Lagoon", "Omega", "Trifid", "Red Spider", "NGC 6559" ]
                    }
                }
            }

          expected =
            { zodiac:
                { sagittarius:
                    { nebula: [ "Lagoon nebula", "Omega nebula", "Trifid nebula", "Red Spider nebula", "NGC 6559 nebula" ]
                    }
                }
            }
        Barlow.over @"zodiac.sagittarius.nebula+" (_ <> " nebula") sky `shouldEqual` expected
      it "should toArrayOfB into a record (2)" do
        let
          sky =
            { zodiac:
                [ { name: "sagittarius", nebula: [ "Lagoon", "Omega", "Trifid", "Red Spider", "NGC 6559" ] }
                , { name: "scorpius", nebula: [ "Bug", "Cat" ] }
                ]
            }

          expected = [ "Lagoon", "Omega", "Trifid", "Red Spider", "NGC 6559", "Bug", "Cat" ]
        Barlow.toArrayOf @"zodiac+.nebula+" sky `shouldEqual` expected
