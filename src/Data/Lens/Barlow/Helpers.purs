module Data.Lens.Barlow.Helpers where

import Prelude

import Data.Lens (Forget)
import Data.Lens as Lens
import Data.Lens.Barlow (barlow, barlowImpl)
import Data.Lens.Barlow.Construction (class ConstructBarlow)
import Data.Lens.Barlow.Parser (class ParseSymbol)
import Data.List (List)
import Data.Maybe (Maybe)
import Data.Maybe.First (First)
import Data.Monoid.Endo (Endo)
import Data.Symbol (class IsSymbol)
import Type.Proxy (Proxy(..))

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
