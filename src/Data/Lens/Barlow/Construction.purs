module Data.Lens.Barlow.Construction where

import Data.Either (Either)
import Data.Generic.Rep (class Generic, Argument, Constructor, NoArguments, Product, Sum)
import Data.Lens (class Wander, Optic', Optic, _Just, _Left, _Right, traversed)
import Data.Lens.Barlow.Generic (_Argument, _Constructor, _NoArguments, _ProductLeft, _ProductRight, _SumLeft, _SumRight, _ToGeneric)
import Data.Lens.Barlow.Types (ExclamationMark, LeftArrow, N1, Percentage, Plus, QuestionMark, RecordField, RightArrow, S, TCons, TList, TNil)
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Data.Profunctor (class Profunctor)
import Data.Profunctor.Choice (class Choice)
import Data.Profunctor.Strong (class Strong)
import Data.Traversable (class Traversable)
import Prelude (Unit, (<<<))
import Prim.Row as Row
import Type.Prelude (class IsSymbol)
import Type.Proxy (Proxy(..))

-- Type class for barlow lens construction of generic representations. 
-- This type class is never instantiated immediately, but rather if ConstructBarlow fails.
class ConstructBarlowGeneric (attributes :: TList) p s t a b | attributes s -> t a b where
  constructBarlowGeneric :: Proxy attributes -> Optic p s t a b

{-
These are examples of generic representations. Keeping them here for reference.
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
--------- PERCENTAGE SINGLETON CONSTRUCTOR -----------
instance 
  ( Strong p
    ) =>
  ConstructBarlowGeneric
    (TCons (Percentage sym) TNil)
    p
    (Constructor sym NoArguments)
    (Constructor sym NoArguments)
    Unit Unit where
  constructBarlowGeneric _ = _Constructor <<< _NoArguments
else instance cbgNilPercentageOneArgumentConstructor ::
  ( Profunctor p
    ) =>
  ConstructBarlowGeneric
    (TCons (Percentage sym) TNil)
    p
    (Constructor sym (Argument a))
    (Constructor sym (Argument a))
    a a where
  constructBarlowGeneric _ = _Constructor <<< _Argument
else instance cbgConsPercentageOneArgumentConstructor ::
  ( ConstructBarlow rest p restA restA a a
  , Profunctor p
  ) =>
  ConstructBarlowGeneric
    (TCons (Percentage sym) rest)
    p
    (Constructor sym (Argument restA))
    (Constructor sym (Argument restA))
    a a where
  constructBarlowGeneric _ = _Constructor <<< _Argument <<< constructBarlow (Proxy :: Proxy rest)
else instance cbgConsPercentageMultiArgumentConstructor ::
  ( ConstructBarlowGeneric rest p restA restA a a
  , Profunctor p
  ) =>
  ConstructBarlowGeneric
    (TCons (Percentage sym) rest)
    p
    (Constructor sym restA)
    (Constructor sym restA)
    a a where
  constructBarlowGeneric _ = _Constructor <<< constructBarlowGeneric (Proxy :: Proxy rest)
--------- PERCENTAGE LEFT SUM -----------------------
else instance cbgNilPercentageZeroArgumentLeftSum ::
  ( Choice p
  , Strong p
  ) =>
  ConstructBarlowGeneric
    (TCons (Percentage sym) TNil)
    p
    (Sum (Constructor sym NoArguments) r)
    (Sum (Constructor sym NoArguments) r)
    Unit Unit where
  constructBarlowGeneric _ = _SumLeft <<< _Constructor <<< _NoArguments
else instance cbgNilPercentageOneArgumentLeftSum ::
  ( Choice p
  ) =>
  ConstructBarlowGeneric
    (TCons (Percentage sym) TNil)
    p
    (Sum (Constructor sym (Argument a)) r)
    (Sum (Constructor sym (Argument a)) r)
    a a where
  constructBarlowGeneric _ = _SumLeft <<< _Constructor <<< _Argument
else instance cbgConsPercentageOneArgumentLeftSum ::
  ( ConstructBarlow rest p restA restA a a
  , Choice p
  ) =>
  ConstructBarlowGeneric
    (TCons (Percentage sym) rest)
    p
    (Sum (Constructor sym (Argument restA)) r)
    (Sum (Constructor sym (Argument restA)) r)
    a a where
  constructBarlowGeneric _ = _SumLeft <<< _Constructor <<< _Argument <<< constructBarlow (Proxy :: Proxy rest)
else instance cbgConsPercentageMultiArgumentLeftSum ::
  ( ConstructBarlowGeneric rest p restA restA a a
  , Choice p
  ) =>
  ConstructBarlowGeneric
    (TCons (Percentage sym) rest)
    p
    (Sum (Constructor sym restA) r)
    (Sum (Constructor sym restA) r)
    a a where
  constructBarlowGeneric _ = _SumLeft <<< _Constructor <<< constructBarlowGeneric (Proxy :: Proxy rest)
--------- PERCENTAGE RIGHT SUM -----------------------
else instance cbgNilPercentageZeroArgumentRightSum ::
  ( Choice p
  , Strong p
  ) =>
  ConstructBarlowGeneric
    (TCons (Percentage sym) TNil)
    p
    (Sum l (Constructor sym NoArguments))
    (Sum l (Constructor sym NoArguments))
    Unit Unit where
  constructBarlowGeneric _ = _SumRight <<< _Constructor <<< _NoArguments
else instance cbgNilPercentageOneArgumentRightSum ::
  ( Choice p
  ) =>
  ConstructBarlowGeneric
    (TCons (Percentage sym) TNil)
    p
    (Sum l (Constructor sym (Argument a)))
    (Sum l (Constructor sym (Argument a)))
    a a where
  constructBarlowGeneric _ = _SumRight <<< _Constructor <<< _Argument
else instance cbgConsRightPercentageOneSum ::
  ( ConstructBarlow rest p restA restA a a
  , Choice p
  ) =>
  ConstructBarlowGeneric
    (TCons (Percentage sym) rest)
    p
    (Sum l (Constructor sym (Argument restA)))
    (Sum l (Constructor sym (Argument restA)))
    a a where
  constructBarlowGeneric _ = _SumRight <<< _Constructor <<< _Argument <<< constructBarlow (Proxy :: Proxy rest)
else instance cbgConsRightPercentageManySum ::
  ( ConstructBarlowGeneric (TCons (Percentage sym) rest) p restA restA a a
  , Choice p
  ) =>
  ConstructBarlowGeneric
    (TCons (Percentage sym) rest)
    p
    (Sum l restA)
    (Sum l restA)
    a a where
  constructBarlowGeneric _ = _SumRight <<< constructBarlowGeneric (Proxy :: Proxy (TCons (Percentage sym) rest))
--------- PERCENTAGE LEFT PRODUCT -----------------------
else instance cbgNilPercentageOneArgumentLeftProduct ::
  ( Strong p
    ) =>
  ConstructBarlowGeneric
    (TCons (Percentage N1) TNil)
    p
    (Product (Argument a) r)
    (Product (Argument a) r)
    a a where
  constructBarlowGeneric _ = _ProductLeft <<< _Argument
else instance cbgConsPercentageOneArgumentLeftProduct ::
  ( ConstructBarlow rest p restA restA a a
  , Strong p
  ) =>
  ConstructBarlowGeneric
    (TCons (Percentage N1) rest)
    p
    (Product (Argument restA) r)
    (Product (Argument restA) r)
    a a where
  constructBarlowGeneric _ = _ProductLeft <<< _Argument <<< constructBarlow (Proxy :: Proxy rest)
--------- PERCENTAGE RIGHT PRODUCT -----------------------
else instance cbgNilPercentageOneArgumentRightProduct ::
  ( Profunctor p
    ) =>
  ConstructBarlowGeneric
    (TCons (Percentage N1) TNil)
    p
    (Argument a)
    (Argument a)
    a a where
  constructBarlowGeneric _ = _Argument
else instance cbgConsPercentageOneArgumentRightProduct ::
  ( ConstructBarlow rest p restA restA a a
  , Profunctor p
  ) =>
  ConstructBarlowGeneric
    (TCons (Percentage N1) rest)
    p
    (Argument restA)
    (Argument restA)
    a a where
  constructBarlowGeneric _ = _Argument <<< constructBarlow (Proxy :: Proxy rest)
else instance cbgPercentageRightProduct ::
  ( ConstructBarlowGeneric (TCons (Percentage k) rest) p restA restA a a
  , Strong p
  ) =>
  ConstructBarlowGeneric
    (TCons (Percentage (S k)) rest)
    p
    (Product l restA)
    (Product l restA)
    a a where
  constructBarlowGeneric _ = _ProductRight <<< constructBarlowGeneric (Proxy :: Proxy (TCons (Percentage k) rest))

-- Typeclass for constructing a barlow lenses from a typelevel list and an input type.
class ConstructBarlow (attributes :: TList) p s t a b | attributes s -> t a b where
  constructBarlow :: Proxy attributes -> Optic p s t a b

-- Nil instance for question mark 
instance constructBarlowNilQuestionMark ::
  ( Choice p
    ) =>
  ConstructBarlow (TCons QuestionMark TNil) p (Maybe a) (Maybe b) a b where
  constructBarlow _ = _Just
-- Nil instance for right arrow
else instance constructBarlowNilRightArrow ::
  ( Choice p
    ) =>
  ConstructBarlow (TCons RightArrow TNil) p (Either l a) (Either l b) a b where
  constructBarlow _ = _Right
-- Nil instance for left arrow
else instance constructBarlowNilLeftArrow ::
  ( Choice p
    ) =>
  ConstructBarlow (TCons LeftArrow TNil) p (Either a r) (Either b r) a b where
  constructBarlow _ = _Left
-- Nil instance for plus
else instance constructBarlowNilPlus ::
  ( Wander p
  , Traversable t
  ) =>
  ConstructBarlow (TCons Plus TNil) p (t a) (t b) a b where
  constructBarlow _ = traversed
-- Nil instance for exclamation mark  
else instance constructBarlowNilExclamationMark ::
  ( Profunctor p
  , Newtype nt a
  ) =>
  ConstructBarlow (TCons ExclamationMark TNil) p nt nt a a where
  constructBarlow _ = _Newtype
-- Nil instance for record selector
else instance constructBarlowNil ::
  ( IsSymbol sym
  , Row.Cons sym a rc x
  , Row.Cons sym b rc y
  , Strong p
  ) =>
  ConstructBarlow (TCons (RecordField sym) TNil) p (Record x) (Record y) a b where
  constructBarlow _ = prop (Proxy :: Proxy sym)
-- Cons instance for question mark
else instance constructBarlowConsQuestionMark ::
  ( ConstructBarlow rest p restA restB a b
  , Choice p
  ) =>
  ConstructBarlow
    (TCons QuestionMark rest)
    p
    (Maybe restA)
    (Maybe restB)
    a b where
  constructBarlow _ = _Just <<< constructBarlow (Proxy :: Proxy rest)
-- Cons instance for right arrow
else instance constructBarlowConsRightArrow ::
  ( ConstructBarlow rest p restA restB a b
  , Choice p
  ) =>
  ConstructBarlow
    (TCons RightArrow rest)
    p
    (Either l restA)
    (Either l restB)
    a b where
  constructBarlow _ = _Right <<< constructBarlow (Proxy :: Proxy rest)
-- Cons instance for left arrow
else instance constructBarlowConsLeftArrow ::
  ( ConstructBarlow rest p restA restB a b
  , Choice p
  ) =>
  ConstructBarlow
    (TCons LeftArrow rest)
    p
    (Either restA r)
    (Either restB r)
    a b where
  constructBarlow _ = _Left <<< constructBarlow (Proxy :: Proxy rest)
-- Cons instance for plus
else instance constructBarlowConsPlus ::
  ( ConstructBarlow rest p restA restB a b
  , Wander p
  , Traversable t
  ) =>
  ConstructBarlow
    (TCons Plus rest)
    p
    (t restA)
    (t restB)
    a b where
  constructBarlow _ = traversed <<< constructBarlow (Proxy :: Proxy rest)
-- Cons instance for Newtype
else instance constructBarlowConsExclamationMark ::
  ( ConstructBarlow rest p restA restA a a
  , Newtype nt restA
  , Profunctor p
  ) =>
  ConstructBarlow
    (TCons ExclamationMark rest)
    p
    nt
    nt
    a a where
  constructBarlow _ = _Newtype <<< constructBarlow (Proxy :: Proxy rest)
-- Cons instance for record selector
else instance constructBarlowCons ::
  ( IsSymbol sym
  , ConstructBarlow rest p restA restB a b
  , Row.Cons sym restA thru rx
  , Row.Cons sym restB thru ry
  , Strong p
  ) =>
  ConstructBarlow (TCons (RecordField sym) rest) p { | rx } { | ry } a b where
  constructBarlow _ = prop (Proxy :: Proxy sym) <<< constructBarlow (Proxy :: Proxy rest)
-- Instance for generic
else instance constructBarlowConsNilGeneric ::
  ( Generic s repS
  , ConstructBarlowGeneric tlist p repS repS a a
  , Profunctor p
  ) =>
  ConstructBarlow
    tlist
    p
    s s
    a a where
  constructBarlow _ = _ToGeneric <<< constructBarlowGeneric (Proxy :: Proxy tlist)
