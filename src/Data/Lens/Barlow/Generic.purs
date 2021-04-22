module Data.Lens.Barlow.Generic where

import Data.Either (Either(..))
import Data.Generic.Rep (class Generic, Argument(..), Constructor(..), Product(..), Sum(..), from, to)
import Data.Lens (Iso', Lens', Prism', iso, lens', prism, united)
import Data.Profunctor.Strong (class Strong)
import Data.Tuple (Tuple(..))
import Data.Unit (Unit)

_SumRight :: forall l r. Prism' (Sum l r) r
_SumRight =
  prism Inr case _ of
    Inr r -> Right r
    anotherCase -> Left anotherCase

_SumLeft :: forall l r. Prism' (Sum l r) l
_SumLeft =
  prism Inl case _ of
    Inl l -> Right l
    anotherCase -> Left anotherCase

_ProductLeft :: forall a b. Lens' (Product a b) a
_ProductLeft =
  lens' \(Product first second) ->
    Tuple
      first
      (\a -> Product a second)

_ProductRight :: forall a b. Lens' (Product a b) b
_ProductRight =
  lens' \(Product first second) ->
    Tuple
      second
      (\b -> Product first b)

_Constructor :: forall a sym. Iso' (Constructor sym a) a
_Constructor = iso unwrapC wrapC
  where
  unwrapC (Constructor a) = a

  wrapC a = Constructor a

_Argument :: forall a. Iso' (Argument a) a
_Argument = iso unwrapA wrapA
  where
  unwrapA (Argument a) = a

  wrapA a = Argument a

_NoArguments :: forall a p. Strong p => p Unit Unit -> p a a
_NoArguments = united

_ToGeneric :: forall input output. Generic input output => Iso' input output
_ToGeneric = iso from to
