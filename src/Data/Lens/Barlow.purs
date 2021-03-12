module Data.Lens.Barlow where

import Prelude
import Data.Lens (Lens')
import Data.Lens.Record (prop)
import Prim.Row as Row
import Prim.Symbol as Symbol
import Type.Prelude (class IsSymbol)
import Type.Proxy (Proxy(..))

-- typelevel element 
data TElem

foreign import data Field :: Symbol -> TElem

-- typelevel list
data TList

foreign import data TNil :: TList

foreign import data TCons :: TElem -> TList -> TList

class Parse (string :: Symbol) (attributes :: TList) | string -> attributes

class Parse1 (head :: Symbol) (tail :: Symbol) (out :: TList) | head tail -> out

instance parse1Nil :: Parse1 a "" (TCons (Field a) TNil)
else instance parse1Pc ::
  ( Parse s rest
    ) =>
  Parse1 "." s (TCons (Field "") rest)
else instance parse1Other ::
  ( Parse s (TCons (Field acc) r)
  , Symbol.Cons o acc rest
  ) =>
  Parse1 o s (TCons (Field rest) r)

instance parseNil ::
  Parse "" (TCons (Field "") TNil)
else instance parseCons ::
  ( Symbol.Cons h t string
  , Parse1 h t fl
  ) =>
  Parse string fl

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
  barlow :: Proxy string -> Lens' input output

instance barlowInstance ::
  ( Parse string attributes
  , ConstructBarlow attributes { | input } output
  ) =>
  Barlow string { | input } output where
  barlow _ = constructBarlow (Proxy :: Proxy attributes)

key :: forall k. Proxy k
key = Proxy
