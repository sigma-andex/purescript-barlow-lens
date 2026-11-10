module Data.Lens.Barlow.Parser where

import Data.Lens.Barlow.Types
import Prim.Int as Int
import Prim.Symbol as Symbol
import Prim.TypeError (class Fail, Text)

-- | Parses what follows a `%` into either `ProductIndex n` or `Percentage name`.
class ParsePercentage (head :: Symbol) (tail :: Symbol) (out :: LensType) (rest :: Symbol) | head tail -> out rest

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

-- | `%` followed by digits is a product index (a type-level Int), anything else a constructor name (a Symbol).
instance parsePercentage ::
  ( DigitValue h digit
  , ParsePercentageStart digit h t out rest
  ) =>
  ParsePercentage h t out rest

class ParsePercentageStart (digit :: DigitResult) (head :: Symbol) (tail :: Symbol) (out :: LensType) (rest :: Symbol) | digit head tail -> out rest

instance parsePercentageStartDigit ::
  ( ParseNumber t d n rest
  ) =>
  ParsePercentageStart (IsDigit d) h t (ProductIndex n) rest

instance parsePercentageStartEnd :: ParsePercentageStart NotDigit h "" (Percentage h) ""
else instance parsePercentageStartSym ::
  ( Symbol.Cons th tt t
  , ParsePercentageSymbol th tt tout trest
  , Symbol.Cons h tout out
  ) =>
  ParsePercentageStart NotDigit h t (Percentage out) trest

-- | Reads the remaining digits of a number, leaving everything after the last digit as `rest`.
class ParseNumber (string :: Symbol) (acc :: Int) (out :: Int) (rest :: Symbol) | string acc -> out rest

instance parseNumberEnd :: ParseNumber "" acc acc ""
else instance parseNumberCons ::
  ( Symbol.Cons h t string
  , DigitValue h digit
  , ParseNumberStep digit h t acc out rest
  ) =>
  ParseNumber string acc out rest

class ParseNumberStep (digit :: DigitResult) (head :: Symbol) (tail :: Symbol) (acc :: Int) (out :: Int) (rest :: Symbol) | digit head tail acc -> out rest

instance parseNumberStepDigit ::
  ( Int.Mul acc 10 acc10
  , Int.Add acc10 d acc'
  , ParseNumber t acc' out rest
  ) =>
  ParseNumberStep (IsDigit d) h t acc out rest

instance parseNumberStepOther ::
  ( Symbol.Cons h t rest
  ) =>
  ParseNumberStep NotDigit h t acc acc rest

data DigitResult

foreign import data IsDigit :: Int -> DigitResult

foreign import data NotDigit :: DigitResult

class DigitValue (char :: Symbol) (digit :: DigitResult) | char -> digit

instance DigitValue "0" (IsDigit 0)
else instance DigitValue "1" (IsDigit 1)
else instance DigitValue "2" (IsDigit 2)
else instance DigitValue "3" (IsDigit 3)
else instance DigitValue "4" (IsDigit 4)
else instance DigitValue "5" (IsDigit 5)
else instance DigitValue "6" (IsDigit 6)
else instance DigitValue "7" (IsDigit 7)
else instance DigitValue "8" (IsDigit 8)
else instance DigitValue "9" (IsDigit 9)
else instance DigitValue c NotDigit

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
  , ParsePercentage th tt lens trest
  , ParseSymbol trest rest
  ) =>
  Parse1Symbol "%" t (TCons lens rest)
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
