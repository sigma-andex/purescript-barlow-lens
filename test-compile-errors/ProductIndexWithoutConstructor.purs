-- expect: needs a constructor first
module CompileError.ProductIndexWithoutConstructor where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Lens.Barlow.Helpers as B
import Data.Map as Map
import Data.Tuple (Tuple(..))

data Constellation = Virgo String Int | Leo | Cancer String

derive instance Generic Constellation _

sky =
  { zodiac: { virgo: { alpha: "Spica" } }
  , stars: [ "Spica", "Porrima" ]
  , flags: Map.singleton true "yes"
  , mixed: { a: 1, b: "x" }
  , constellation: Virgo "Spica" 1
  , pair: Tuple "Spica" 1
  }

x = B.preview @"constellation.%1" sky
