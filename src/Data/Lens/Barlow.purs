module Data.Lens.Barlow where

import Data.Lens (Optic', Optic)
import Data.Lens.Barlow.Construction (class ConstructBarlow, constructBarlow)
import Data.Lens.Barlow.Parser (class ParseSymbol)
import Type.Proxy (Proxy(..))

class Barlow (string :: Symbol) p s t a b | string -> s t a b where
  -- | Type-safe lens for zooming into a deeply nested record
  -- |
  -- | ```purescript 
  -- | sky = { zodiac: { virgo: { alpha: "Spica" } } }
  -- | view (barlow (key :: _ "zodiac.virgo.alpha")) sky
  -- | -- Spica 
  -- | over (barlow (key :: _ "zodiac.virgo.alpha")) toUpper sky
  -- | -- { zodiac: { virgo: { alpha: "SPICA" } } }
  -- | ```
  barlow :: Proxy string -> Optic p s t a b

instance barlowInstance ::
  ( ParseSymbol string attributes
  , ConstructBarlow attributes p s t a b
  ) =>
  Barlow string p s t a b where
  barlow _ = constructBarlow (Proxy :: Proxy attributes)

-- | Just an alias for `Proxy` to make selection a bit nicer
key :: forall k. Proxy k
key = Proxy
