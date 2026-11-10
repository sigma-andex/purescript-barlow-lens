module Data.Lens.Barlow.Parser where

import Data.Lens.Barlow.Types
import Prim.Symbol as Symbol
import Prim.TypeError (class Fail, Text)

class ParsePercentage :: forall k. Symbol -> Symbol -> k -> Symbol -> Constraint
class ParsePercentage (head :: Symbol) (tail :: Symbol) out (rest :: Symbol) | head tail -> out rest

class ParsePercentageSymbol (head :: Symbol) (tail :: Symbol) (out :: Symbol) (rest :: Symbol) | head tail -> out rest

instance parsePercentageSymbolDot :: ParsePercentageSymbol "." t "" t
else instance parsePercentageSymbolSpace :: ParsePercentageSymbol " " t "" t
else instance parsePercentageSymbolEnd :: ParsePercentageSymbol h "" h ""
else instance parsePercentageSymbolCons ::
  ( Symbol.Cons th tt t
  , ParsePercentageSymbol th tt tout trest
  , Symbol.Cons h tout out
  ) =>
  ParsePercentageSymbol h t out trest

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
else instance parsePercentagSym ::
  ( Symbol.Cons th tt t
  , ParsePercentageSymbol th tt tout trest
  , Symbol.Cons h tout out
  ) =>
  ParsePercentage h t out trest

-- | Parses the inside of `[...]` up to (and consuming) the closing `]`.
class ParseBracket (head :: Symbol) (tail :: Symbol) (out :: Symbol) (rest :: Symbol) | head tail -> out rest

instance parseBracketClose :: ParseBracket "]" t "" t
else instance parseBracketUnterminated ::
  Fail (Text "barlow: unterminated `[` – expected a closing `]`") =>
  ParseBracket h "" out rest
else instance parseBracketCons ::
  ( Symbol.Cons th tt t
  , ParseBracket th tt tout trest
  , Symbol.Cons h tout out
  ) =>
  ParseBracket h t out trest

-- | Decides between `[key]` and `[?key]`.
class ParseBracketOpen (head :: Symbol) (tail :: Symbol) (out :: LensType) (rest :: Symbol) | head tail -> out rest

instance parseBracketOpenAt ::
  ( Symbol.Cons th tt t
  , ParseBracket th tt key rest
  ) =>
  ParseBracketOpen "?" t (BracketAt key) rest
else instance parseBracketOpenIx ::
  ( ParseBracket h t key rest
  ) =>
  ParseBracketOpen h t (Bracket key) rest

-- | Parses the inside of `{...}` up to (and consuming) the closing `}`.
class ParseBrace (head :: Symbol) (tail :: Symbol) (out :: Symbol) (rest :: Symbol) | head tail -> out rest

instance parseBraceClose :: ParseBrace "}" t "" t
else instance parseBraceUnterminated ::
  Fail (Text "barlow: unterminated `{` – expected a closing `}`") =>
  ParseBrace h "" out rest
else instance parseBraceCons ::
  ( Symbol.Cons th tt t
  , ParseBrace th tt tout trest
  , Symbol.Cons h tout out
  ) =>
  ParseBrace h t out trest

-- | Splits `a, b,c` into a type-level list of field names, ignoring spaces.
class SplitFields (string :: Symbol) (current :: Symbol) (out :: TList) | string current -> out

instance splitFieldsEmpty :: SplitFields "" "" TNil
else instance splitFieldsLast :: SplitFields "" current (TCons current TNil)
else instance splitFieldsCons ::
  ( Symbol.Cons h t string
  , SplitFieldsStep h t current out
  ) =>
  SplitFields string current out

class SplitFieldsStep (head :: Symbol) (tail :: Symbol) (current :: Symbol) (out :: TList) | head tail current -> out

instance splitFieldsStepEmptyField ::
  Fail (Text "barlow: empty field name in `{...}`") =>
  SplitFieldsStep "," t "" out
else instance splitFieldsStepComma ::
  ( SplitFields t "" rest
  ) =>
  SplitFieldsStep "," t current (TCons current rest)
else instance splitFieldsStepSpace ::
  ( SplitFields t current out
  ) =>
  SplitFieldsStep " " t current out
else instance splitFieldsStepChar ::
  ( Symbol.Append current h current'
  , SplitFields t current' out
  ) =>
  SplitFieldsStep h t current out

class ParseSymbol (string :: Symbol) (attributes :: TList) | string -> attributes

class Parse1Symbol (head :: Symbol) (tail :: Symbol) (out :: TList) | head tail -> out

class ParseRecordField (head :: Symbol) (tail :: Symbol) (out :: Symbol) (rest :: Symbol) | head tail -> out rest

instance parserecordfieldDot :: ParseRecordField "." t "" t
else instance parserecordfieldQuestionMark :: (Symbol.Cons "?" t out) => ParseRecordField "?" t "" out
else instance parserecordfieldLeftArrow :: (Symbol.Cons "<" t out) => ParseRecordField "<" t "" out
else instance parserecordfieldRightArrow :: (Symbol.Cons ">" t out) => ParseRecordField ">" t "" out
else instance parserecordfieldPlus :: (Symbol.Cons "+" t out) => ParseRecordField "+" t "" out
else instance parserecordfieldHash :: (Symbol.Cons "#" t out) => ParseRecordField "#" t "" out
else instance parserecordfieldBracket :: (Symbol.Cons "[" t out) => ParseRecordField "[" t "" out
else instance parserecordfieldBrace :: (Symbol.Cons "{" t out) => ParseRecordField "{" t "" out
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
else instance parse1Hash ::
  ( ParseSymbol s rest
  ) =>
  Parse1Symbol "#" s (TCons Hash rest)
else instance parse1Bracket ::
  ( Symbol.Cons th tt t
  , ParseBracketOpen th tt lens trest
  , ParseSymbol trest rest
  ) =>
  Parse1Symbol "[" t (TCons lens rest)
else instance parse1Brace ::
  ( Symbol.Cons th tt t
  , ParseBrace th tt inner trest
  , SplitFields inner "" fields
  , ParseSymbol trest rest
  ) =>
  Parse1Symbol "{" t (TCons (Fields fields) rest)
else instance parse1ExclamationMark ::
  ( ParseSymbol s rest
  ) =>
  Parse1Symbol "!" s (TCons ExclamationMark rest)
else instance parse1Percentage ::
  ( Symbol.Cons th tt t
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
else instance parseNilHash ::
  ParseSymbol "#" (TCons Hash TNil)
else instance parseNilBracket ::
  Fail (Text "barlow: unterminated `[` – expected a closing `]`") =>
  ParseSymbol "[" out
else instance parseNilBrace ::
  Fail (Text "barlow: unterminated `{` – expected a closing `}`") =>
  ParseSymbol "{" out
else instance parseNilExclamationMark ::
  ParseSymbol "!" (TCons ExclamationMark TNil)
else instance parseNil ::
  ParseSymbol "" TNil
else instance parseCons ::
  ( Symbol.Cons h t string
  , Parse1Symbol h t fl
  ) =>
  ParseSymbol string fl
