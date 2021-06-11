module Data.Lens.Barlow.Helpers where

import Prelude
import Data.Lens (Forget)
import Data.Lens as Lens
import Data.Lens.Barlow (barlow)
import Data.Lens.Barlow.Construction (class ConstructBarlow)
import Data.Lens.Barlow.Parser (class ParseSymbol)
import Data.List (List)
import Data.Maybe (Maybe)
import Data.Maybe.First (First)
import Data.Monoid.Endo (Endo)
import Type.Proxy (Proxy)

view ::
  forall s t a b sym lensTypes.
  ParseSymbol sym lensTypes =>
  ConstructBarlow lensTypes (Forget a) s t a b =>
  Proxy sym -> s -> a
view = Lens.view <<< barlow

preview ::
  forall s t a b sym lensTypes.
  ParseSymbol sym lensTypes =>
  ConstructBarlow lensTypes (Forget (First a)) s t a b =>
  Proxy sym -> s -> Maybe a
preview = Lens.preview <<< barlow

over ::
  forall s t a b sym lensTypes.
  ParseSymbol sym lensTypes =>
  ConstructBarlow lensTypes Function s t a b =>
  Proxy sym -> (a -> b) -> s -> t
over = Lens.over <<< barlow

toArrayOf ::
  forall s t a b sym lenstypes.
  ParseSymbol sym lenstypes =>
  ConstructBarlow lenstypes (Forget (Endo Function (List a))) s t a b =>
  Proxy sym -> s -> Array a
toArrayOf = Lens.toArrayOf <<< barlow
