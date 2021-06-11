module Data.Lens.Barlow.Helpers where

import Prelude
import Data.Lens (Forget, over, preview, toArrayOf, view)
import Data.Lens.Barlow (barlow)
import Data.Lens.Barlow.Construction (class ConstructBarlow)
import Data.Lens.Barlow.Parser (class ParseSymbol)
import Data.List (List)
import Data.Maybe (Maybe)
import Data.Maybe.First (First)
import Data.Monoid.Endo (Endo)
import Type.Proxy (Proxy)

viewB :: forall s t a b sym lensTypes. ParseSymbol sym lensTypes => ConstructBarlow lensTypes (Forget a) s t a b => Proxy sym -> s -> a
viewB = view <<< barlow

previewB :: forall s t a b sym lensTypes. ParseSymbol sym lensTypes => ConstructBarlow lensTypes (Forget (First a)) s t a b => Proxy sym -> s -> Maybe a
previewB = preview <<< barlow

overB :: forall s t a b sym lensTypes. ParseSymbol sym lensTypes => ConstructBarlow lensTypes Function s t a b => Proxy sym -> (a -> b) -> s -> t
overB = over <<< barlow

toArrayOfB :: forall s t a b sym lenstypes. ParseSymbol sym lenstypes => ConstructBarlow lenstypes (Forget (Endo Function (List a))) s t a b => Proxy sym -> s -> Array a
toArrayOfB = toArrayOf <<< barlow
