module Data.Lens.Barlow where

import Data.Lens (Optic)
import Data.Lens.Barlow.Construction (class ConstructBarlow, constructBarlow)
import Data.Lens.Barlow.Parser (class ParseSymbol)
import Type.Proxy (Proxy(..))
import Data.Symbol (class IsSymbol)

class Barlow (string :: Symbol) p s t a b | string -> s t a b where
  barlowImpl :: Proxy string -> Optic p s t a b

-- | Type-safe lens for zooming into a deeply nested record
-- |
-- | ```purescript
-- | sky = { zodiac: { virgo: { alpha: "Spica" } } }
-- | view (barlow @"zodiac.virgo.alpha") sky
-- | -- Spica
-- | over (barlow @"zodiac.virgo.alpha") toUpper sky
-- | -- { zodiac: { virgo: { alpha: "SPICA" } } }
-- | ```
barlow :: forall @string lenses p s t a b.
  ParseSymbol string lenses =>
  ConstructBarlow lenses p s t a b =>
  IsSymbol string => Optic p s t a b
barlow = barlowImpl (Proxy :: Proxy string)

instance
  ( ParseSymbol string lenses
  , ConstructBarlow lenses p s t a b
  ) =>
  Barlow string p s t a b where
  barlowImpl _ = constructBarlow (Proxy :: Proxy lenses)

-- | Just an alias for `Proxy` to make selection a bit nicer
key :: forall k. Proxy k
key = Proxy
