module Data.Lens.Barlow.Construction where

import Prelude (Unit, (<<<))
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
import Type.Prelude (class IsSymbol)
import Type.Proxy (Proxy(..))
import Data.Lens.Barlow.Types (ExclamationMark, LeftArrow, Plus, QuestionMark, RecordField, RightArrow, TCons, TList, TNil)

class ConstructBarlowGeneric (attributes :: TList) p input output | attributes input -> output where
  constructBarlowGeneric :: Proxy attributes -> Optic' p input output

{-
These are examples of generic representations. Keeping them here fore reference.
(Constructor @"A1" NoArguments)
(Constructor @"A2" (Argument "hallo"))
(Constructor @"A3" (Argument Red))
(Constructor @"A4" (Product (Argument "hello") (Argument 9)))
(Constructor @"A9" (Product (Argument "buenas") (Product (Argument 14) (Argument true))))
(Inl (Constructor @"A5" NoArguments))
(Inl (Constructor @"A6" (Argument "Wurst")))
(Inl (Constructor @"A8" (Product (Argument "hola") (Argument 11))))
(Inl (Constructor @"A10" (Argument 15)))
(Inl (Constructor @"A7" NoArguments))
(Inr (Constructor @"B5" NoArguments))
(Inr (Constructor @"B6" (Argument 10)))
(Inr (Constructor @"B7" (Argument Yellow)))
(Inr (Inl (Constructor @"B8" (Argument 12))))
(Inr (Inr (Constructor @"C8" NoArguments)))
(Inr (Inl (Constructor @"B10" (Product (Argument Yellow) (Product (Argument "adios") (Argument 16))))))
(Inr (Inr (Constructor @"C10" (Product (Argument "uno") (Product (Argument "dos") (Argument "tres"))))))
-}
instance cbgNilLeftArrowConstructorNoArguments ::
  ( Strong p
    ) =>
  ConstructBarlowGeneric
    (TCons LeftArrow TNil)
    p
    (Constructor sym NoArguments)
    Unit where
  constructBarlowGeneric _ = _Constructor <<< _NoArguments
-- -- Cons instance for left arrow generic with one argument
else instance cbgNilLeftArrowConstructorArgument ::
  ( Strong p
    ) =>
  ConstructBarlowGeneric
    (TCons LeftArrow TNil)
    p
    (Constructor sym (Argument output))
    output where
  constructBarlowGeneric _ = _Constructor <<< _Argument
else instance cbgConsLeftArrowConstructorArgument ::
  ( ConstructBarlow rest p restR output
  , Strong p
  ) =>
  ConstructBarlowGeneric
    (TCons LeftArrow rest)
    p
    (Constructor sym (Argument restR))
    output where
  constructBarlowGeneric _ = _Constructor <<< _Argument <<< constructBarlow (Proxy :: Proxy rest)
else instance cbgConstructorProduct ::
  ( ConstructBarlowGeneric rest p restR output
  , Strong p
  ) =>
  ConstructBarlowGeneric
    rest
    p
    (Constructor sym restR)
    output where
  constructBarlowGeneric _ = _Constructor <<< constructBarlowGeneric (Proxy :: Proxy rest)
else instance cbgConsLeftArrowSum ::
  ( ConstructBarlowGeneric rest p restR output
  , Choice p
  ) =>
  ConstructBarlowGeneric
    (TCons LeftArrow rest)
    p
    (Sum restR r)
    output where
  constructBarlowGeneric _ = _SumLeft <<< constructBarlowGeneric (Proxy :: Proxy rest)
else instance cbgConsRightArrowSum ::
  ( ConstructBarlowGeneric rest p restR output
  , Choice p
  ) =>
  ConstructBarlowGeneric
    (TCons RightArrow rest)
    p
    (Sum l restR)
    output where
  constructBarlowGeneric _ = _SumRight <<< constructBarlowGeneric (Proxy :: Proxy rest)
else instance cbgNilLeftArrowProduct ::
  ( Strong p
    ) =>
  ConstructBarlowGeneric
    (TCons LeftArrow TNil)
    p
    (Product (Argument output) r)
    output where
  constructBarlowGeneric _ = _ProductLeft <<< _Argument
else instance cbgConsLeftArrowProduct ::
  ( ConstructBarlow rest p restR output
  , Strong p
  ) =>
  ConstructBarlowGeneric
    (TCons LeftArrow rest)
    p
    (Product (Argument restR) r)
    output where
  constructBarlowGeneric _ = _ProductLeft <<< _Argument <<< constructBarlow (Proxy :: Proxy rest)
else instance cbgNilRightArrowProductArgument ::
  ( Strong p
    ) =>
  ConstructBarlowGeneric
    (TCons RightArrow TNil)
    p
    (Product l (Argument output))
    output where
  constructBarlowGeneric _ = _ProductRight <<< _Argument
else instance cbgConsRightArrowProductArgument ::
  ( ConstructBarlow rest p restR output
  , Strong p
  ) =>
  ConstructBarlowGeneric
    (TCons RightArrow rest)
    p
    (Product l (Argument restR))
    output where
  constructBarlowGeneric _ = _ProductRight <<< _Argument <<< constructBarlow (Proxy :: Proxy rest)
else instance cbgRightArrowProductRecursive ::
  ( ConstructBarlowGeneric rest p restR output
  , Strong p
  ) =>
  ConstructBarlowGeneric
    (TCons RightArrow rest)
    p
    (Product l restR)
    output where
  constructBarlowGeneric _ = _ProductRight <<< constructBarlowGeneric (Proxy :: Proxy rest)

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
-- Instance for generic
else instance constructBarlowConsNilGeneric ::
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
