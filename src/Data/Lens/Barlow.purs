module Data.Lens.Barlow where

import Prelude

import Data.Lens (Optic, iso)
import Data.Lens.Barlow.Construction (class ConstructBarlow, constructBarlow)
import Data.Lens.Barlow.Parser (class ParseSymbol)
import Type.Proxy (Proxy(..))

class Barlow (string :: Symbol) p s t a b output | string s -> t a b output where
  -- | Type-safe lens for zooming into a deeply nested record
  -- |
  -- | ```purescript 
  -- | sky = { zodiac: { virgo: { alpha: "Spica" } } }
  -- | view (barlow (key :: _ "zodiac.virgo.alpha")) sky
  -- | -- Spica 
  -- | over (barlow (key :: _ "zodiac.virgo.alpha")) toUpper sky
  -- | -- { zodiac: { virgo: { alpha: "SPICA" } } }
  -- | ```
  barlow :: Proxy string -> (p s output) --  Optic p s t a b ->

instance barlowInstance ::
  ( ParseSymbol string lenses
  , ConstructBarlow lenses (->) s t a b output 
  ) =>
  Barlow string (->) s t a b output where
  barlow _ = constructBarlow (Proxy :: Proxy lenses) 

barlowX proxy = barlow proxy (iso identity identity)
-- | Just an alias for `Proxy` to make selection a bit nicer
key :: forall k. Proxy k
key = Proxy
