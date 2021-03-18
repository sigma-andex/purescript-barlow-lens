module Data.Lens.Barlow where

import Prelude
import Data.Lens (Lens')
import Data.Lens.Record (prop)
import Prim.Row as Row
import Prim.Symbol as Symbol
import Type.Prelude (class IsSymbol)
import Type.Proxy (Proxy(..))

-- typelevel list
data TList

foreign import data TNil :: TList

foreign import data TCons :: forall k. k  -> TList -> TList

-- typelevel element 
data TElem

foreign import data QMark :: TElem 
foreign import data Field :: Symbol -> TElem


class ParseSymbol (string :: Symbol) (attributes :: TList) | string -> attributes

class Parse1Symbol (head :: Symbol) (tail :: Symbol) (out :: TList) | head tail -> out

instance parse1Nil :: Parse1Symbol a "" (TCons (Field a) TNil)
else instance parse1Pc ::
  ( ParseSymbol s rest
    ) =>
  Parse1Symbol "." s (TCons (Field "") rest)

else instance parse1PcQ ::
  ( ParseSymbol s rest
    ) =>
  Parse1Symbol "?" s (TCons (Field "") (TCons QMark rest))


else instance parse1Other ::
  ( ParseSymbol s (TCons (Field acc) r)
  , Symbol.Cons o acc rest
  ) =>
  Parse1Symbol o s (TCons (Field rest) r)

instance parseNil ::
  ParseSymbol "" (TCons (Field "") TNil)
else instance parseCons ::
  ( Symbol.Cons h t string 
  , Parse1Symbol h t fl
  ) =>
  ParseSymbol string fl

class ConstructBarlow (attributes :: TList) input output | attributes -> input output where
  constructBarlow :: Proxy attributes -> Lens' input output

instance constructBarlowNil ::
  ( IsSymbol sym
  , Row.Cons sym output rc x
  ) =>
  ConstructBarlow (TCons (Field sym) TNil) (Record x) output where
  constructBarlow proxy = prop (Proxy :: Proxy sym)
else instance constructBarlowCons ::
  ( IsSymbol sym
  , ConstructBarlow rest restR output
  , Row.Cons sym restR rb rl
  ) =>
  ConstructBarlow (TCons (Field sym) rest) { | rl } output where
  constructBarlow proxy = prop (Proxy :: Proxy sym) <<< constructBarlow (Proxy :: Proxy rest)

class Barlow (string :: Symbol) input output | string -> input output where
  -- | Type-safe lens for zooming into a deeply nested record
  -- |
  -- | ```purescript 
  -- | sky = { zodiac: { virgo: { alpha: "Spica" } } }
  -- | view (barlow (key :: _ "zodiac.virgo.alpha")) sky
  -- | -- Spica 
  -- | over (barlow (key :: _ "zodiac.virgo.alpha")) toUpper sky
  -- | -- { zodiac: { virgo: { alpha: "SPICA" } } }
  -- | ```
  barlow :: Proxy string -> Lens' input output

instance barlowInstance ::
  ( ParseSymbol string attributes
  , ConstructBarlow attributes { | input } output
  ) =>
  Barlow string { | input } output where
  barlow _ = constructBarlow (Proxy :: Proxy attributes)

key :: forall k. Proxy k
key = Proxy
