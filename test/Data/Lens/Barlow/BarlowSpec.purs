module Data.Lens.Barlow.BarlowSpec where

import Data.Either (Either(..))
import Data.Lens (over, preview, view)
import Data.Lens.Barlow (barlow, key)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.String (toUpper)
import Prelude (Unit, discard, ($))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

newtype AlphaR = AlphaR { alpha :: String }
instance alphaRNT :: Newtype AlphaR { alpha :: String }

newtype Alpha = Alpha String
instance alphaNT :: Newtype Alpha String

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

          actual = view (barlow @"zodiac.virgo.alpha") sky
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

          actual = over (barlow @"zodiac.virgo.alpha") toUpper sky
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

          actual = preview (barlow @"zodiac?.virgo?.alpha?") sky
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

          actual = over (barlow @"zodiac?.virgo?.alpha?") toUpper sky
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

          actual = preview (barlow @"zodiac?.virgo.alpha") sky
        actual `shouldEqual` (Just "Spica")
      it "should view into parts of a record" do
        let
          sky =
            { zodiac:
                Just
                  { virgo:
                      { alpha: "Spica"
                      }
                  }
            }

          expected =
            { virgo:
                { alpha: "Spica"
                }
            }

          actual = preview (barlow @"zodiac?") sky
        actual `shouldEqual` (Just expected)
      it "should view into parts of a record (2)" do
        let
          sky =
            { zodiac:
                Just
                  { virgo:
                      Just
                        { alpha: "Spica"
                        }
                  }
            }

          expected =
            { alpha: "Spica"
            }

          actual = preview (barlow @"zodiac?.virgo?") sky
        actual `shouldEqual` (Just expected)
      it "should view into a record with Either" do
        let
          sky =
            { zodiac:
                Right
                  { virgo:
                      { alpha: "Spica"
                      }
                  }
            }

          expected =
            { virgo:
                { alpha: "Spica"
                }
            }

          actual = preview (barlow @"zodiac>") sky
        actual `shouldEqual` (Just expected)
      it "should view into a record with Either (2)" do
        let
          sky =
            { zodiac:
                Right
                  { virgo:
                      Right
                        { alpha: Right "Spica"
                        }
                  }
            }

          actual = preview (barlow @"zodiac>.virgo>.alpha>") sky
        actual `shouldEqual` (Just "Spica")
      it "should view into a record with mixed Just and Either" do
        let
          sky =
            { zodiac:
                Right
                  { virgo:
                      Just
                        { alpha: Left "Spica"
                        }
                  }
            }

          actual = preview (barlow @"zodiac>.virgo?.alpha<") sky
        actual `shouldEqual` (Just "Spica")
      it "should modify a record with mixed Just and Either" do
        let
          -- type annotation necessary in this test for Show
          sky :: { zodiac :: Either String { virgo :: Maybe { alpha :: Either String Int } } }
          sky =
            { zodiac:
                Right
                  { virgo:
                      Just
                        { alpha: Left "Spica"
                        }
                  }
            }

          expected =
            { zodiac:
                Right
                  { virgo:
                      Just
                        { alpha: Left "SPICA"
                        }
                  }
            }

          actual = over (barlow @"zodiac>.virgo?.alpha<") toUpper sky
        actual `shouldEqual` expected
      it "should modify a record with mixed Just and Either (2)" do
        let
          -- type annotation necessary in this test for Show
          sky :: { zodiac :: Either String (Maybe { virgo :: Maybe (Either Int { alpha :: Maybe (Either String Int) }) }) }
          sky =
            { zodiac:
                Right (
                  Just { virgo:
                      Just
                        (Right { alpha: Just (Left "Spica") })
                  }
                )
            }

          expected =
            { zodiac:
                Right (
                  Just { virgo:
                      Just
                        (Right { alpha: Just (Left "SPICA") })
                  }
                )
            }

          actual = over (barlow @"zodiac>?.virgo?>.alpha?<") toUpper sky
        actual `shouldEqual` expected
      it "should modify a record with Array" do
        let
          sky =
            { zodiac:
                [ { virgo:
                      Just
                        { star: "Spica"
                        }
                  }
                , { virgo:
                      Just
                        { star: "Serpentis"
                        }
                  }
                ]
            }

          expected =
            { zodiac:
                [ { virgo:
                      Just
                        { star: "SPICA"
                        }
                  }
                , { virgo:
                      Just
                        { star: "SERPENTIS"
                        }
                  }
                ]
            }

          actual = over (barlow @"zodiac+.virgo?.star") toUpper sky
        actual `shouldEqual` expected

      it "should modify a record with Array (2)" do
        let
          sky =
            { zodiac:
                [ { virgo:
                      Just
                        { star: Left "Spica"
                        }
                  }
                , { virgo:
                      Just
                        { star: Right "Serpentis"
                        }
                  }
                ]
            }

          expected =
            { zodiac:
                [ { virgo:
                      Just
                        { star: Left "Spica"
                        }
                  }
                , { virgo:
                      Just
                        { star: Right "SERPENTIS"
                        }
                  }
                ]
            }

          actual = over (barlow @"zodiac+.virgo?.star>") toUpper sky
        actual `shouldEqual` expected
      it "should view into a record with Newtype" do
        let
          sky =
            { zodiac:
                Just
                  { virgo:
                      AlphaR { alpha: "Spica"
                      }
                  }
            }

          actual = preview (barlow @"zodiac?.virgo!.alpha") sky
        actual `shouldEqual` (Just "Spica")
      it "should view into a record with Newtype (2)" do
        let
          sky =
            { zodiac:
                Just
                  { virgo:
                      { alpha: Alpha "Spica"
                      }
                  }
            }

          actual = preview (barlow @"zodiac?.virgo.alpha!") sky
        actual `shouldEqual` (Just "Spica")
      it "should view into a data type" do
        let
          sky =
            Just $ Right $ { zodiac:
                Just
                  { virgo:
                      { alpha: Alpha "Spica"
                      }
                  }
            }

          actual = preview (barlow @"?>.zodiac?.virgo.alpha!") sky
        actual `shouldEqual` (Just "Spica")
