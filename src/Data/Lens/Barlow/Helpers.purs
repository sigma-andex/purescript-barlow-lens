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

viewB = view <<< barlow

previewB = preview <<< barlow

overB = over <<< barlow

toArrayOfB = toArrayOf <<< barlow
