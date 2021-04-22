module Data.Lens.Barlow.Parser where

import Data.Lens.Barlow.Types

import Prim.Symbol as Symbol
import Type.Proxy (Proxy(..))

class ParsePercentage :: forall k. Symbol -> Symbol -> k -> Symbol -> Constraint
class ParsePercentage (head :: Symbol) (tail :: Symbol) out (rest :: Symbol) | head tail -> out rest

class ParsePercentageSymbol (head :: Symbol) (tail :: Symbol) (out :: Symbol) (rest :: Symbol) | head tail -> out rest

instance parsePercentageSymbolDot :: ParsePercentageSymbol "." t "" t
else instance parsePercentageSymbolSpace :: ParsePercentageSymbol " " t "" t
else instance parsePercentageSymbolEnd :: ParsePercentageSymbol h "" h ""
else instance parsePercentageSymbolCons :: (
  Symbol.Cons th tt t 
, ParsePercentageSymbol th tt tout trest 
, Symbol.Cons h tout out
) => ParsePercentageSymbol h t out trest 

instance parsePercentage1 :: ParsePercentage "1" t N1 t 
else instance parsePercentage2 :: ParsePercentage "2" t N2 t
else instance parsePercentage3 :: ParsePercentage "3" t N3 t
else instance parsePercentage4 :: ParsePercentage "4" t N4 t
else instance parsePercentage5 :: ParsePercentage "5" t N5 t
else instance parsePercentage6 :: ParsePercentage "6" t N6 t
else instance parsePercentage7 :: ParsePercentage "7" t N7 t
else instance parsePercentage8 :: ParsePercentage "8" t N8 t
else instance parsePercentage9 :: ParsePercentage "9" t N9 t
else instance parsePercentageEnd :: ParsePercentage h "" h ""
else instance parsePercentagSym :: (
  Symbol.Cons th tt t 
, ParsePercentageSymbol th tt tout trest 
, Symbol.Cons h tout out
) => ParsePercentage h t out trest 


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
else instance parse1Percentage ::
  (
    Symbol.Cons th tt t 
  , ParsePercentage th tt tout trest 
  , ParseSymbol trest rest
  ) => 
  Parse1Symbol "%" t (TCons (Percentage tout) rest)
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


tshow :: forall sym tlist. ParseSymbol sym tlist => Proxy sym -> Proxy tlist 
tshow _ = Proxy 


x :: Proxy (TCons (Percentage (S Z)) (TCons (RecordField "abc") TNil))
x = tshow (Proxy :: Proxy "%1.abc")

y :: Proxy (TCons (RecordField "abc") (TCons (Percentage "Red") (TCons (RecordField "xyz") TNil)))
y = tshow (Proxy :: Proxy "abc.%Red.xyz")
