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

viewB ::
  forall input output sym attribs.
  ParseSymbol sym attribs =>
  ConstructBarlow attribs (Forget output) input output =>
  Proxy sym -> input -> output
viewB = view <<< barlow

previewB ::
  forall input output sym attribs.
  ParseSymbol sym attribs =>
  ConstructBarlow attribs (Forget (First output)) input output =>
  Proxy sym -> input -> Maybe output
previewB = preview <<< barlow

overB ::
  forall input inner sym attribs.
  ParseSymbol sym attribs =>
  ConstructBarlow attribs Function input inner =>
  Proxy sym -> (inner -> inner) -> input -> input
overB = over <<< barlow

toArrayOfB :: forall input output sym attribs. ParseSymbol sym attribs => ConstructBarlow attribs (Forget (Endo Function (List output))) input output => Proxy sym -> input -> Array output
toArrayOfB = toArrayOf <<< barlow
