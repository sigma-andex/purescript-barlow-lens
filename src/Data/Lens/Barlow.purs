module Data.Lens.Barlow where

import Prelude

import Data.Either (Either)
import Data.Generic.Rep (class Generic, Argument, Constructor, NoArguments, Product, Sum)
import Data.Lens (class Wander, Optic', _Just, _Left, _Right, traversed)
import Data.Lens.Barlow.Generic (_Argument, _Constructor, _NoArguments, _ProductLeft, _ProductRight, _SumLeft, _SumRight, _ToGeneric)
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Data.Profunctor.Choice (class Choice)
import Data.Profunctor.Strong (class Strong)
import Data.Traversable (class Traversable)
import Prim.Row as Row
import Prim.Symbol as Symbol
import Type.Prelude (class IsSymbol)
import Type.Proxy (Proxy(..))

-- typelevel list
data TList

foreign import data TNil :: TList

foreign import data TCons :: forall k. k -> TList -> TList

-- ADT representing the different types of lenses at the type level
data LensType

foreign import data QuestionMark :: LensType

foreign import data RightArrow :: LensType

foreign import data LeftArrow :: LensType

foreign import data Plus :: LensType

foreign import data ExclamationMark :: LensType

foreign import data RecordField :: Symbol -> LensType


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


class ConstructBarlowGeneric (attributes :: TList) p input output | attributes input -> output where
  constructBarlowGeneric :: Proxy attributes -> Optic' p input output

-- Nil instance for left arrow generic with no argument
instance constructBarlowGenericNilLeftArrowNoArguments ::
  ( Strong p
  , Choice p
  ) =>
  ConstructBarlowGeneric
    (TCons LeftArrow TNil)
    p
    NoArguments
    Unit where
  constructBarlowGeneric _ = _NoArguments

-- Nil instance for left arrow generic with no argument
else instance constructBarlowGenericNilLeftArrowConstructorNoArguments ::
  ( Strong p
  , Choice p
  ) =>
  ConstructBarlowGeneric
    (TCons LeftArrow TNil)
    p
    (Constructor sym NoArguments) 
    Unit where
  constructBarlowGeneric _ = _Constructor <<< _NoArguments

-- -- Cons instance for left arrow generic with one argument
else instance constructBarlowGenericLeftArrowArgument ::
  ( ConstructBarlow rest p restR output
  , Strong p
  , Choice p
  ) =>
  ConstructBarlowGeneric
    (TCons LeftArrow rest)
    p
    (Argument restR)
    output where
  constructBarlowGeneric _ = _Argument <<< constructBarlow (Proxy :: Proxy rest)

-- -- Cons instance for left arrow generic with one argument
else instance constructBarlowGenericLeftArrowConstructorArgument ::
  ( ConstructBarlow rest p restR output
  , Strong p
  , Choice p
  ) =>
  ConstructBarlowGeneric
    (TCons LeftArrow rest)
    p
    (Constructor sym (Argument restR))
    output where
  constructBarlowGeneric _ = _Constructor <<< _Argument <<< constructBarlow (Proxy :: Proxy rest)

-- -- Cons instance for left arrow generic with product
else instance constructBarlowGenericLeftArrowConstructorProduct ::
  ( ConstructBarlow rest p restR output
  , Strong p
  , Choice p
  ) =>
  ConstructBarlowGeneric
    (TCons LeftArrow rest)
    p
    (Constructor sym (Product (Argument restR) r))
    output where
  constructBarlowGeneric _ = _Constructor <<< _ProductLeft <<< _Argument <<< constructBarlow (Proxy :: Proxy rest)

-- -- Cons instance for left arrow generic with product
else instance constructBarlowGenericRightArrowConstructorProduct ::
  ( ConstructBarlowGeneric rest p restR output
  , Strong p
  , Choice p
  ) =>
  ConstructBarlowGeneric
    (TCons RightArrow rest)
    p
    (Product l restR)
    output where
  constructBarlowGeneric _ = _ProductRight <<< constructBarlowGeneric (Proxy :: Proxy rest)

-- Cons instance for left arrow generic with one argument
else instance constructBarlowConsLeftArrowSum ::
  ( ConstructBarlowGeneric rest p restR output
  , Strong p
  , Choice p
  ) =>
  ConstructBarlowGeneric
    (TCons LeftArrow rest)
    p
    (Sum (Constructor sym restR) r)
    output where
  constructBarlowGeneric _ = _SumLeft <<< _Constructor <<< constructBarlowGeneric (Proxy :: Proxy rest)

-- Cons instance for left arrow generic with one argument
else instance constructBarlowConsRightArrowSum ::
  ( ConstructBarlowGeneric rest p restR output
  , Strong p
  , Choice p
  ) =>
  ConstructBarlowGeneric
    (TCons RightArrow rest)
    p
    (Sum l (Constructor sym restR))
    output where
  constructBarlowGeneric _ = _SumRight <<< _Constructor <<< constructBarlowGeneric (Proxy :: Proxy rest)


class ConstructBarlow (attributes :: TList) p input output | attributes input -> output where
  constructBarlow :: Proxy attributes -> Optic' p input output


-- Nil instance for question mark 
instance constructBarlowNilQuestionMark ::
  ( Choice p
    ) =>
  ConstructBarlow (TCons QuestionMark TNil) p (Maybe output) output where
  constructBarlow _ = _Just
-- Nil instance for right arrow
else instance constructBarlowNilRightArrow ::
  ( Choice p
    ) =>
  ConstructBarlow (TCons RightArrow TNil) p (Either l output) output where
  constructBarlow _ = _Right
-- Nil instance for left arrow
else instance constructBarlowNilLeftArrow ::
  ( Choice p
    ) =>
  ConstructBarlow (TCons LeftArrow TNil) p (Either output r) output where
  constructBarlow _ = _Left
-- Nil instance for plus
else instance constructBarlowNilPlus ::
  ( Wander p
  , Traversable t
  ) =>
  ConstructBarlow (TCons Plus TNil) p (t output) output where
  constructBarlow _ = traversed
-- Nil instance for exclamation mark  
else instance constructBarlowNilExclamationMark ::
  ( Choice p
  , Newtype nt output
  ) =>
  ConstructBarlow (TCons ExclamationMark TNil) p nt output where
  constructBarlow _ = _Newtype
-- Nil instance for record selector
else instance constructBarlowNil ::
  ( IsSymbol sym
  , Row.Cons sym output rc x
  , Strong p
  ) =>
  ConstructBarlow (TCons (RecordField sym) TNil) p (Record x) output where
  constructBarlow _ = prop (Proxy :: Proxy sym)


-- Cons instance for question mark
else instance constructBarlowConsQuestionMark ::
  ( ConstructBarlow rest p restR output
  , Strong p
  , Choice p
  ) =>
  ConstructBarlow
    (TCons QuestionMark rest)
    p
    (Maybe restR)
    output where
  constructBarlow _ = _Just <<< constructBarlow (Proxy :: Proxy rest)
-- Cons instance for right arrow
else instance constructBarlowConsRightArrow ::
  ( ConstructBarlow rest p restR output
  , Strong p
  , Choice p
  ) =>
  ConstructBarlow
    (TCons RightArrow rest)
    p
    (Either l restR)
    output where
  constructBarlow _ = _Right <<< constructBarlow (Proxy :: Proxy rest)
-- Cons instance for left arrow
else instance constructBarlowConsLeftArrow ::
  ( ConstructBarlow rest p restR output
  , Strong p
  , Choice p
  ) =>
  ConstructBarlow
    (TCons LeftArrow rest)
    p
    (Either restR r)
    output where
  constructBarlow _ = _Left <<< constructBarlow (Proxy :: Proxy rest)
-- Cons instance for plus
else instance constructBarlowConsPlus ::
  ( ConstructBarlow rest p restR output
  , Strong p
  , Wander p
  , Traversable t
  ) =>
  ConstructBarlow
    (TCons Plus rest)
    p
    (t restR)
    output where
  constructBarlow _ = traversed <<< constructBarlow (Proxy :: Proxy rest)
-- Cons instance for Newtype
else instance constructBarlowConsExclamationMark ::
  ( ConstructBarlow rest p restR output
  , Newtype nt restR
  , Strong p
  , Choice p
  ) =>
  ConstructBarlow
    (TCons ExclamationMark rest)
    p
    nt
    output where
  constructBarlow _ = _Newtype <<< constructBarlow (Proxy :: Proxy rest)
-- Cons instance for record selector
else instance constructBarlowCons ::
  ( IsSymbol sym
  , ConstructBarlow rest p restR output
  , Row.Cons sym restR rb rl
  , Strong p
  ) =>
  ConstructBarlow (TCons (RecordField sym) rest) p { | rl } output where
  constructBarlow _ = prop (Proxy :: Proxy sym) <<< constructBarlow (Proxy :: Proxy rest)


-- Cons instance for generic
else instance constructBarlowNilGeneric ::
  ( Generic input rep 
  , ConstructBarlowGeneric tlist p rep output
  , Strong p
  , Choice p
  ) =>
  ConstructBarlow
    tlist
    p
    input
    output where
  constructBarlow _ = _ToGeneric <<< constructBarlowGeneric (Proxy :: Proxy tlist)



class Barlow (string :: Symbol) p input output | string -> input output where
  -- | Type-safe lens for zooming into a deeply nested record
  -- |
  -- | ```purescript 
  -- | sky = { zodiac: { virgo: { alpha: "Spica" } } }
  -- | view (barlow (key :: _ "zodiac.virgo.alpha")) sky
  -- | -- Spica 
  -- | over (barlow (key :: _ "zodiac.virgo.alpha")) toUpper sky
  -- | -- { zodiac: { virgo: { alpha: "SPICA" } } }
  -- | ```
  barlow :: Proxy string -> Optic' p input output

instance barlowInstance ::
  ( ParseSymbol string attributes
  , ConstructBarlow attributes p input output
  ) =>
  Barlow string p input output where
  barlow _ = constructBarlow (Proxy :: Proxy attributes)

-- | Just an alias for `Proxy` to make selection a bit nicer
key :: forall k. Proxy k
key = Proxy
