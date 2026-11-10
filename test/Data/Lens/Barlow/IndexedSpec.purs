module Data.Lens.Barlow.IndexedSpec where

import Prelude

import Data.Lens (IndexedTraversal')
import Data.Lens as L
import Data.Lens.Barlow (ibarlow)
import Data.Lens.Barlow.Helpers as Barlow
import Data.List (List(..), (:))
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

type Sky = { zodiac :: Array { virgo :: Maybe { stars :: Array String } } }

sky :: Sky
sky =
  { zodiac:
      [ { virgo: Just { stars: [ "Spica", "Porrima" ] } }
      , { virgo: Just { stars: [ "Vindemiatrix" ] } }
      ]
  }

-- works with a polymorphic profunctor, too
starsT :: IndexedTraversal' (Tuple Int Int) Sky String
starsT = ibarlow @"zodiac#.virgo?.stars#"

spec :: Spec Unit
spec =
  describe "Data.Lens.Barlow" do
    describe "ibarlow" do
      it "should collect elements with their index" do
        Barlow.itoListOf @"zodiac#.virgo?.stars" sky
          `shouldEqual` (Tuple 0 [ "Spica", "Porrima" ] : Tuple 1 [ "Vindemiatrix" ] : Nil)
      it "should combine nested indices into tuples" do
        Barlow.itoListOf @"zodiac#.virgo?.stars#" sky
          `shouldEqual`
            ( Tuple (Tuple 0 0) "Spica"
                : Tuple (Tuple 0 1) "Porrima"
                : Tuple (Tuple 1 0) "Vindemiatrix"
                : Nil
            )
      it "should work as a polymorphic IndexedTraversal" do
        L.itoListOf starsT sky `shouldEqual` Barlow.itoListOf @"zodiac#.virgo?.stars#" sky
        L.iover starsT (\(Tuple i j) s -> show i <> show j <> s) sky
          `shouldEqual`
            { zodiac:
                [ { virgo: Just { stars: [ "00Spica", "01Porrima" ] } }
                , { virgo: Just { stars: [ "10Vindemiatrix" ] } }
                ]
            }
      it "should use map keys as index and allow changing types" do
        let
          named = { named: Map.fromFoldable [ Tuple "virgo" { alpha: 1 }, Tuple "leo" { alpha: 2 } ] }
        Barlow.iover @"named#.alpha" (\k v -> k <> show v) named
          `shouldEqual` { named: Map.fromFoldable [ Tuple "virgo" { alpha: "virgo1" }, Tuple "leo" { alpha: "leo2" } ] }
      it "should index a homogeneous record by field name" do
        let
          virgo = { stars: { alpha: "Spica", beta: "Zavijava", gamma: "Porrima" } }
        Barlow.itoListOf @"stars#" virgo
          `shouldEqual` (Tuple "alpha" "Spica" : Tuple "beta" "Zavijava" : Tuple "gamma" "Porrima" : Nil)
      it "should support a top-level #" do
        Barlow.ifoldMapOf @"#" (\i s -> show i <> s) [ "a", "b" ] `shouldEqual` "0a1b"
      it "should traverse with index" do
        Barlow.itraverseOf @"zodiac#.virgo?.stars" (\i s -> Just (show i <> show s)) sky
          `shouldEqual`
            Just
              { zodiac:
                  [ { virgo: Just { stars: "0[\"Spica\",\"Porrima\"]" } }
                  , { virgo: Just { stars: "1[\"Vindemiatrix\"]" } }
                  ]
              }
      it "should work after a generic path" do
        let
          t = { pair: Tuple "x" [ 10, 20 ] }
        Barlow.itoListOf @"pair.%2#" t `shouldEqual` (Tuple 0 10 : Tuple 1 20 : Nil)
