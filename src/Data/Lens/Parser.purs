module Data.Lens.Barlow.Parser where

import Prim.Symbol as Symbol
import Data.Lens.Barlow.Types

class ParseSymbol (string :: Symbol) (attributes :: TList) | string -> attributes

class Parse1Symbol (head :: Symbol) (tail :: Symbol) (out :: TList) | head tail -> out

class ParseRecordField (head :: Symbol) (tail :: Symbol) (out :: Symbol) (rest :: Symbol) | head tail -> out rest

instance parserecordfieldDot :: ParseRecordField "." t "" t
else instance parserecordfieldQuestionMark :: (Symbol.Cons "?" t out) => ParseRecordField "?" t "" out
else instance parserecordfieldLeftArrow :: (Symbol.Cons "<" t out) => ParseRecordField "<" t "" out
else instance parserecordfieldRightArrow :: (Symbol.Cons ">" t out) => ParseRecordField ">" t "" out
else instance parserecordfieldPlus :: (Symbol.Cons "+" t out) => ParseRecordField "+" t "" out
else instance parserecordfieldExclamationMark :: (Symbol.Cons "!" t out) => ParseRecordField "!" t "" out
else instance parserecordfieldEnd :: ParseRecordField h "" h ""
else instance parserecordfieldCons ::
  ( Symbol.Cons th tt t
  , ParseRecordField th tt tout trest
  , Symbol.Cons h tout out
  ) =>
  ParseRecordField h t out trest

instance parse1Nil :: Parse1Symbol a "" (TCons (RecordField a) TNil)
else instance parse1Dot ::
  ( ParseSymbol s rest
    ) =>
  Parse1Symbol "." s rest
else instance parse1QuestionMark ::
  ( ParseSymbol s rest
    ) =>
  Parse1Symbol "?" s (TCons QuestionMark rest)
else instance parse1RightArrow ::
  ( ParseSymbol s rest
    ) =>
  Parse1Symbol ">" s (TCons RightArrow rest)
else instance parse1LeftArrow ::
  ( ParseSymbol s rest
    ) =>
  Parse1Symbol "<" s (TCons LeftArrow rest)
else instance parse1Plus ::
  ( ParseSymbol s rest
    ) =>
  Parse1Symbol "+" s (TCons Plus rest)
else instance parse1ExclamationMark ::
  ( ParseSymbol s rest
    ) =>
  Parse1Symbol "!" s (TCons ExclamationMark rest)
else instance parse1Other ::
  ( Symbol.Cons th tt t
  , ParseRecordField th tt tout trest
  , Symbol.Cons h tout out
  , ParseSymbol trest rest
  ) =>
  Parse1Symbol h t (TCons (RecordField out) rest)

instance parseNilQuestionMark ::
  ParseSymbol "?" (TCons QuestionMark TNil)
else instance parseNilRightArrow ::
  ParseSymbol ">" (TCons RightArrow TNil)
else instance parseNilLeftArrow ::
  ParseSymbol "<" (TCons LeftArrow TNil)
else instance parseNilPlus ::
  ParseSymbol "+" (TCons Plus TNil)
else instance parseNilExclamationMark ::
  ParseSymbol "!" (TCons ExclamationMark TNil)
else instance parseNil ::
  ParseSymbol "" TNil
else instance parseCons ::
  ( Symbol.Cons h t string
  , Parse1Symbol h t fl
  ) =>
  ParseSymbol string fl
