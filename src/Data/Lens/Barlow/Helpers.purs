module Data.Lens.Barlow.Helpers where

import Prelude

import Data.Lens (Forget, IndexedOptic)
import Data.Lens as Lens
import Data.Lens.Barlow (barlow, ibarlow)
import Data.Lens.Barlow.Construction (class ConstructBarlow)
import Data.Lens.Barlow.Indexed (class ConstructIndexed)
import Data.Lens.Barlow.Parser (class ParseSymbol)
import Data.Lens.Fold as Fold
import Data.List (List)
import Data.Maybe (Maybe)
import Data.Maybe.First (First)
import Data.Maybe.Last (Last)
import Data.Monoid.Additive (Additive)
import Data.Monoid.Conj (Conj)
import Data.Monoid.Disj (Disj)
import Data.Monoid.Endo (Endo)
import Data.Profunctor.Star (Star)
import Data.Tuple (Tuple)
import Data.Symbol (class IsSymbol)

view
  :: forall s t a b @sym lenses
   . IsSymbol sym 
  => ParseSymbol sym lenses
  => ConstructBarlow lenses (Forget a) s t a b
  => s
  -> a
view = Lens.view $ barlow @sym

preview
  :: forall s t a b @sym lenses
   . IsSymbol sym 
  => ParseSymbol sym lenses
  => ConstructBarlow lenses (Forget (First a)) s t a b
  => s
  -> Maybe a
preview = Lens.preview $ barlow @sym

foldOf
  :: forall s t a b @sym lenses
   . IsSymbol sym 
  => ParseSymbol sym lenses
  => ConstructBarlow lenses (Forget a) s t a b
  => s
  -> a
foldOf = Lens.foldOf $ barlow @sym

over
  :: forall s t a b @sym lenses
   . IsSymbol sym 
  => ParseSymbol sym lenses
  => ConstructBarlow lenses Function s t a b
  => (a -> b)
  -> s
  -> t
over = Lens.over $ barlow @sym

set
  :: forall s t a b @sym lenses
   . IsSymbol sym 
  => ParseSymbol sym lenses
  => ConstructBarlow lenses Function s t a b
  => b
  -> s
  -> t
set = Lens.set $ barlow @sym

toArrayOf
  :: forall s t a b @sym lenses
   . IsSymbol sym 
  => ParseSymbol sym lenses
  => ConstructBarlow lenses (Forget (Endo Function (List a))) s t a b
  => s
  -> Array a
toArrayOf = Lens.toArrayOf $ barlow @sym

toListOf
  :: forall s t a b @sym lenses
   . IsSymbol sym
  => ParseSymbol sym lenses
  => ConstructBarlow lenses (Forget (Endo Function (List a))) s t a b
  => s
  -> List a
toListOf = Lens.toListOf $ barlow @sym

firstOf
  :: forall s t a b @sym lenses
   . IsSymbol sym
  => ParseSymbol sym lenses
  => ConstructBarlow lenses (Forget (First a)) s t a b
  => s
  -> Maybe a
firstOf = Fold.firstOf $ barlow @sym

lastOf
  :: forall s t a b @sym lenses
   . IsSymbol sym
  => ParseSymbol sym lenses
  => ConstructBarlow lenses (Forget (Last a)) s t a b
  => s
  -> Maybe a
lastOf = Fold.lastOf $ barlow @sym

lengthOf
  :: forall s t a b @sym lenses
   . IsSymbol sym
  => ParseSymbol sym lenses
  => ConstructBarlow lenses (Forget (Additive Int)) s t a b
  => s
  -> Int
lengthOf = Fold.lengthOf $ barlow @sym

-- | `true` if the path focuses on at least one value.
has
  :: forall s t a b @sym lenses
   . IsSymbol sym
  => ParseSymbol sym lenses
  => ConstructBarlow lenses (Forget (Disj Boolean)) s t a b
  => s
  -> Boolean
has = Fold.has $ barlow @sym

-- | `true` if the path focuses on no value at all.
hasn't
  :: forall s t a b @sym lenses
   . IsSymbol sym
  => ParseSymbol sym lenses
  => ConstructBarlow lenses (Forget (Conj Boolean)) s t a b
  => s
  -> Boolean
hasn't = Fold.hasn't $ barlow @sym

traverseOf
  :: forall f s t a b @sym lenses
   . IsSymbol sym
  => ParseSymbol sym lenses
  => ConstructBarlow lenses (Star f) s t a b
  => (a -> f b)
  -> s
  -> f t
traverseOf = Lens.traverseOf $ barlow @sym

------------------------------------------------------------------------------
-- Indexed helpers, for paths containing `#`

iover
  :: forall i s t a b @sym lenses
   . IsSymbol sym
  => ParseSymbol sym lenses
  => ConstructIndexed lenses Function i s t a b
  => (i -> a -> b)
  -> s
  -> t
iover = Lens.iover (ibarlow @sym :: IndexedOptic Function i s t a b)

itoListOf
  :: forall i s t a b @sym lenses
   . IsSymbol sym
  => ParseSymbol sym lenses
  => ConstructIndexed lenses (Forget (Endo Function (List (Tuple i a)))) i s t a b
  => s
  -> List (Tuple i a)
itoListOf = Fold.itoListOf (ibarlow @sym :: IndexedOptic (Forget (Endo Function (List (Tuple i a)))) i s t a b)

ifoldMapOf
  :: forall r i s t a b @sym lenses
   . IsSymbol sym
  => ParseSymbol sym lenses
  => ConstructIndexed lenses (Forget r) i s t a b
  => (i -> a -> r)
  -> s
  -> r
ifoldMapOf = Fold.ifoldMapOf (ibarlow @sym :: IndexedOptic (Forget r) i s t a b)

itraverseOf
  :: forall f i s t a b @sym lenses
   . IsSymbol sym
  => ParseSymbol sym lenses
  => ConstructIndexed lenses (Star f) i s t a b
  => (i -> a -> f b)
  -> s
  -> f t
itraverseOf = Lens.itraverseOf (ibarlow @sym :: IndexedOptic (Star f) i s t a b)
