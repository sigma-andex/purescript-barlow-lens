module Data.Lens.Barlow where

import Data.Lens (IndexedOptic, Optic)
import Data.Lens.Barlow.Indexed (class ConstructIndexed, constructIndexed)
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

-- | Type-safe indexed optic. Every `#` in the path traverses a container and remembers
-- | the position (or key) of each element. Several `#` combine their indices into
-- | nested tuples: `Tuple (Tuple i j) k`.
-- |
-- | ```purescript
-- | sky = { zodiac: [ { stars: [ "Spica", "Porrima" ] }, { stars: [ "Regulus" ] } ] }
-- | itoListOf (ibarlow @"zodiac#.stars") sky
-- | -- (Tuple 0 ["Spica","Porrima"]) : (Tuple 1 ["Regulus"]) : Nil
-- | itoListOf (ibarlow @"zodiac#.stars#") sky
-- | -- (Tuple (Tuple 0 0) "Spica") : (Tuple (Tuple 0 1) "Porrima") : (Tuple (Tuple 1 0) "Regulus") : Nil
-- | ```
ibarlow :: forall @string lenses p i s t a b.
  ParseSymbol string lenses =>
  ConstructIndexed lenses p i s t a b =>
  IndexedOptic p i s t a b
ibarlow = constructIndexed (Proxy :: Proxy lenses)

-- | Just an alias for `Proxy` to make selection a bit nicer
key :: forall k. Proxy k
key = Proxy
