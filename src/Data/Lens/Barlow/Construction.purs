module Data.Lens.Barlow.Construction where

import Data.Either (Either)
import Data.Generic.Rep (class Generic, Argument, Constructor, NoArguments, Product, Sum)
import Data.Lens (class Wander, Optic, _1, _2, _Just, _Left, _Right, traversed)
import Data.Lens.At (class At, at)
import Data.Lens.Index (class Index, ix)
import Data.Lens.Barlow.Internal (class FieldOf, class IndexKey, class SelectFields, class RecordElem, class TraverseFields, fieldLens, indexKey, selectFields, traverseRecord, class Unreachable, unreachable)
import Data.Tuple (Tuple)
import Prim.Int as Int
import Prim.RowList as RL
import Prim.Boolean (False, True)
import Prim.TypeError (class Fail, Above, Beside, Doc, Quote, QuoteLabel, Text)
import Type.Data.Boolean (class Or)
import Data.Lens.Barlow.Generic (_Argument, _Constructor, _NoArguments, _ProductLeft, _ProductRight, _SumLeft, _SumRight, _ToGeneric)
import Data.Lens.Barlow.Types (Bracket, BracketAt, ExclamationMark, Fields, LeftArrow, Percentage, Plus, ProductIndex, QuestionMark, RecordField, RightArrow, TCons, TList, TNil)
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Data.Profunctor (class Profunctor)
import Data.Profunctor.Choice (class Choice)
import Data.Profunctor.Strong (class Strong)
import Data.Traversable (class Traversable)
import Prelude (Unit, identity, (<<<))
import Prim.Row as Row
import Type.Prelude (class IsSymbol)
import Type.Proxy (Proxy(..))

-- Type class for barlow lens construction of generic representations. 
-- This type class is never instantiated immediately, but rather if ConstructBarlow fails.
class ConstructBarlowGeneric (lenses :: TList) p s t a b | lenses s -> t a b where
  constructBarlowGeneric :: Proxy lenses -> Optic p s t a b

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
    Unit
    Unit where
  constructBarlowGeneric _ = _Constructor <<< _NoArguments
else instance
  ( Profunctor p
  ) =>
  ConstructBarlowGeneric
    (TCons (Percentage sym) TNil)
    p
    (Constructor sym (Argument a))
    (Constructor sym (Argument a))
    a
    a where
  constructBarlowGeneric _ = _Constructor <<< _Argument
else instance
  ( ConstructBarlow rest p restA restA a a
  , Profunctor p
  ) =>
  ConstructBarlowGeneric
    (TCons (Percentage sym) rest)
    p
    (Constructor sym (Argument restA))
    (Constructor sym (Argument restA))
    a
    a where
  constructBarlowGeneric _ = _Constructor <<< _Argument <<< constructBarlow (Proxy :: Proxy rest)
else instance
  ( ConstructBarlowGeneric rest p restA restA a a
  , Profunctor p
  ) =>
  ConstructBarlowGeneric
    (TCons (Percentage sym) rest)
    p
    (Constructor sym restA)
    (Constructor sym restA)
    a
    a where
  constructBarlowGeneric _ = _Constructor <<< constructBarlowGeneric (Proxy :: Proxy rest)
--------- PERCENTAGE LEFT SUM -----------------------
else instance
  ( Choice p
  , Strong p
  ) =>
  ConstructBarlowGeneric
    (TCons (Percentage sym) TNil)
    p
    (Sum (Constructor sym NoArguments) r)
    (Sum (Constructor sym NoArguments) r)
    Unit
    Unit where
  constructBarlowGeneric _ = _SumLeft <<< _Constructor <<< _NoArguments
else instance
  ( Choice p
  ) =>
  ConstructBarlowGeneric
    (TCons (Percentage sym) TNil)
    p
    (Sum (Constructor sym (Argument a)) r)
    (Sum (Constructor sym (Argument a)) r)
    a
    a where
  constructBarlowGeneric _ = _SumLeft <<< _Constructor <<< _Argument
else instance
  ( ConstructBarlow rest p restA restA a a
  , Choice p
  ) =>
  ConstructBarlowGeneric
    (TCons (Percentage sym) rest)
    p
    (Sum (Constructor sym (Argument restA)) r)
    (Sum (Constructor sym (Argument restA)) r)
    a
    a where
  constructBarlowGeneric _ = _SumLeft <<< _Constructor <<< _Argument <<< constructBarlow (Proxy :: Proxy rest)
else instance
  ( ConstructBarlowGeneric rest p restA restA a a
  , Choice p
  ) =>
  ConstructBarlowGeneric
    (TCons (Percentage sym) rest)
    p
    (Sum (Constructor sym restA) r)
    (Sum (Constructor sym restA) r)
    a
    a where
  constructBarlowGeneric _ = _SumLeft <<< _Constructor <<< constructBarlowGeneric (Proxy :: Proxy rest)
--------- PERCENTAGE RIGHT SUM -----------------------
else instance
  ( Choice p
  , Strong p
  ) =>
  ConstructBarlowGeneric
    (TCons (Percentage sym) TNil)
    p
    (Sum l (Constructor sym NoArguments))
    (Sum l (Constructor sym NoArguments))
    Unit
    Unit where
  constructBarlowGeneric _ = _SumRight <<< _Constructor <<< _NoArguments
else instance
  ( Choice p
  ) =>
  ConstructBarlowGeneric
    (TCons (Percentage sym) TNil)
    p
    (Sum l (Constructor sym (Argument a)))
    (Sum l (Constructor sym (Argument a)))
    a
    a where
  constructBarlowGeneric _ = _SumRight <<< _Constructor <<< _Argument
else instance
  ( ConstructBarlow rest p restA restA a a
  , Choice p
  ) =>
  ConstructBarlowGeneric
    (TCons (Percentage sym) rest)
    p
    (Sum l (Constructor sym (Argument restA)))
    (Sum l (Constructor sym (Argument restA)))
    a
    a where
  constructBarlowGeneric _ = _SumRight <<< _Constructor <<< _Argument <<< constructBarlow (Proxy :: Proxy rest)
else instance
  ( ConstructBarlowGeneric (TCons (Percentage sym) rest) p restA restA a a
  , Choice p
  ) =>
  ConstructBarlowGeneric
    (TCons (Percentage sym) rest)
    p
    (Sum l restA)
    (Sum l restA)
    a
    a where
  constructBarlowGeneric _ = _SumRight <<< constructBarlowGeneric (Proxy :: Proxy (TCons (Percentage sym) rest))
--------- PERCENTAGE PRODUCT -----------------------
else instance
  ( ConstructProduct n n rest p restA a
  ) =>
  ConstructBarlowGeneric
    (TCons (ProductIndex n) rest)
    p
    restA
    restA
    a
    a where
  constructBarlowGeneric _ = constructProduct (Proxy :: Proxy n) (Proxy :: Proxy n) (Proxy :: Proxy rest)

-- | Selects the `n`-th argument of a product with `%n`. `original` is the index the user wrote, for error messages.
class ConstructProduct (original :: Int) (n :: Int) (lenses :: TList) p s a | n lenses s -> a where
  constructProduct :: Proxy original -> Proxy n -> Proxy lenses -> Optic p s s a a

instance
  ( Fail (Text "barlow: `%0` is not valid – product indices start at `%1`")
  , Unreachable
  ) =>
  ConstructProduct o 0 rest p s a where
  constructProduct _ _ _ = unreachable
else instance
  ( ConstructBarlow rest p x x a a
  , Strong p
  ) =>
  ConstructProduct o 1 rest p (Product (Argument x) r) a where
  constructProduct _ _ _ = _ProductLeft <<< _Argument <<< constructBarlow (Proxy :: Proxy rest)
else instance
  ( Int.Add k 1 n
  , ConstructProduct o k rest p r a
  , Strong p
  ) =>
  ConstructProduct o n rest p (Product l r) a where
  constructProduct o _ rest = _ProductRight <<< constructProduct o (Proxy :: Proxy k) rest
else instance
  ( ConstructBarlow rest p x x a a
  , Profunctor p
  ) =>
  ConstructProduct o 1 rest p (Argument x) a where
  constructProduct _ _ _ = _Argument <<< constructBarlow (Proxy :: Proxy rest)
else instance
  ( Int.Add k 1 n
  , Int.Add arity k o
  , Fail
      ( Beside (Text "barlow: `%")
          (Beside (Quote o) (Beside (Text "` is out of range – this constructor only has ") (Beside (Quote arity) (Text " arguments"))))
      )
  , Unreachable
  ) =>
  ConstructProduct o n rest p (Argument x) a where
  constructProduct _ _ _ = unreachable
else instance
  ( Fail (Beside (Text "barlow: `%") (Beside (Quote o) (Text "` cannot be used here – this constructor has no arguments")))
  , Unreachable
  ) =>
  ConstructProduct o n rest p NoArguments a where
  constructProduct _ _ _ = unreachable
else instance
  ( Fail
      ( Beside (Text "barlow: `%")
          ( Beside (Quote o)
              (Beside (Text "` needs a constructor first – use `%") (Beside (Text name) (Beside (Text ".%") (Beside (Quote o) (Text "`")))))
          )
      )
  , Unreachable
  ) =>
  ConstructProduct o n rest p (Constructor name x) a where
  constructProduct _ _ _ = unreachable
else instance
  ( Fail (Beside (Text "barlow: `%") (Beside (Quote o) (Text "` needs a constructor first – select one with `%Name`")))
  , Unreachable
  ) =>
  ConstructProduct o n rest p s a where
  constructProduct _ _ _ = unreachable

-- Typeclass for constructing a barlow lenses from a typelevel list and an input type.
class ConstructBarlow (lenses :: TList) p s t a b | lenses s -> t a b where
  constructBarlow :: Proxy lenses -> Optic p s t a b

-- Empty path: identity. Lets new lens types only define a cons instance.
instance ConstructBarlow TNil p s t s t where
  constructBarlow _ = identity
-- Nil instance for question mark 
else instance
  ( Choice p
  ) =>
  ConstructBarlow (TCons QuestionMark TNil) p (Maybe a) (Maybe b) a b where
  constructBarlow _ = _Just
-- Nil instance for right arrow
else instance
  ( Choice p
  ) =>
  ConstructBarlow (TCons RightArrow TNil) p (Either l a) (Either l b) a b where
  constructBarlow _ = _Right
-- Nil instance for left arrow
else instance
  ( Choice p
  ) =>
  ConstructBarlow (TCons LeftArrow TNil) p (Either a r) (Either b r) a b where
  constructBarlow _ = _Left
-- Cons instance for `+` on a homogeneous record: traverses all fields
else instance
  ( RL.RowToList r rl
  , RecordElem rl restA
  , TraverseFields rl r restA
  , ConstructBarlow rest p restA restA a a
  , Wander p
  ) =>
  ConstructBarlow (TCons Plus rest) p { | r } { | r } a a where
  constructBarlow _ = traverseRecord @restA (Proxy :: Proxy rl) <<< constructBarlow (Proxy :: Proxy rest)
-- Nil instance for plus
else instance
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
else instance
  ( IsSymbol sym
  , FieldOf sym x a rc
  , Row.Cons sym b rc y
  , Strong p
  ) =>
  ConstructBarlow (TCons (RecordField sym) TNil) p (Record x) (Record y) a b where
  constructBarlow _ = fieldLens (Proxy :: Proxy sym)
-- Cons instance for question mark
else instance
  ( ConstructBarlow rest p restA restB a b
  , Choice p
  ) =>
  ConstructBarlow
    (TCons QuestionMark rest)
    p
    (Maybe restA)
    (Maybe restB)
    a
    b where
  constructBarlow _ = _Just <<< constructBarlow (Proxy :: Proxy rest)
-- Cons instance for right arrow
else instance
  ( ConstructBarlow rest p restA restB a b
  , Choice p
  ) =>
  ConstructBarlow
    (TCons RightArrow rest)
    p
    (Either l restA)
    (Either l restB)
    a
    b where
  constructBarlow _ = _Right <<< constructBarlow (Proxy :: Proxy rest)
-- Cons instance for left arrow
else instance
  ( ConstructBarlow rest p restA restB a b
  , Choice p
  ) =>
  ConstructBarlow
    (TCons LeftArrow rest)
    p
    (Either restA r)
    (Either restB r)
    a
    b where
  constructBarlow _ = _Left <<< constructBarlow (Proxy :: Proxy rest)
-- Cons instance for plus
else instance
  ( ConstructBarlow rest p restA restB a b
  , Wander p
  , Traversable t
  ) =>
  ConstructBarlow
    (TCons Plus rest)
    p
    (t restA)
    (t restB)
    a
    b where
  constructBarlow _ = traversed <<< constructBarlow (Proxy :: Proxy rest)
-- Cons instance for Newtype
else instance
  ( ConstructBarlow rest p restA restA a a
  , Newtype nt restA
  , Profunctor p
  ) =>
  ConstructBarlow
    (TCons ExclamationMark rest)
    p
    nt
    nt
    a
    a where
  constructBarlow _ = _Newtype <<< constructBarlow (Proxy :: Proxy rest)
-- Cons instance for record selector
else instance
  ( IsSymbol sym
  , FieldOf sym rx restA thru
  , ConstructBarlow rest p restA restB a b
  , Row.Cons sym restB thru ry
  , Strong p
  ) =>
  ConstructBarlow (TCons (RecordField sym) rest) p { | rx } { | ry } a b where
  constructBarlow _ = fieldLens (Proxy :: Proxy sym) <<< constructBarlow (Proxy :: Proxy rest)
-- `[key]` on a record: a field, which may contain special characters
else instance
  ( IsSymbol sym
  , FieldOf sym rx restA thru
  , ConstructBarlow rest p restA restB a b
  , Row.Cons sym restB thru ry
  , Strong p
  ) =>
  ConstructBarlow (TCons (Bracket sym) rest) p { | rx } { | ry } a b where
  constructBarlow _ = fieldLens (Proxy :: Proxy sym) <<< constructBarlow (Proxy :: Proxy rest)
-- `[key]` on anything with an `Index` instance (Array, List, Map, Object, ...)
else instance
  ( Index s k v
  , IndexKey sym k
  , ConstructBarlow rest p v v a a
  , Wander p
  ) =>
  ConstructBarlow (TCons (Bracket sym) rest) p s s a a where
  constructBarlow _ = ix (indexKey (Proxy :: Proxy sym)) <<< constructBarlow (Proxy :: Proxy rest)
-- `[?key]` on anything with an `At` instance (Map, Object, ...)
else instance
  ( At s k v
  , IndexKey sym k
  , ConstructBarlow rest p (Maybe v) (Maybe v) a a
  , Strong p
  ) =>
  ConstructBarlow (TCons (BracketAt sym) rest) p s s a a where
  constructBarlow _ = at (indexKey (Proxy :: Proxy sym)) <<< constructBarlow (Proxy :: Proxy rest)
-- `{a,b}`: a lens onto a sub-record
else instance
  ( SelectFields fields r sub
  , ConstructBarlow rest p { | sub } { | sub } a a
  , Strong p
  ) =>
  ConstructBarlow (TCons (Fields fields) rest) p { | r } { | r } a a where
  constructBarlow _ = selectFields (Proxy :: Proxy fields) <<< constructBarlow (Proxy :: Proxy rest)
-- `%1` / `%2` on a Tuple: type-changing `_1` / `_2`
else instance
  ( ConstructBarlow rest p restA restB a b
  , Strong p
  ) =>
  ConstructBarlow (TCons (ProductIndex 1) rest) p (Tuple restA c) (Tuple restB c) a b where
  constructBarlow _ = _1 <<< constructBarlow (Proxy :: Proxy rest)
else instance
  ( ConstructBarlow rest p restA restB a b
  , Strong p
  ) =>
  ConstructBarlow (TCons (ProductIndex 2) rest) p (Tuple c restA) (Tuple c restB) a b where
  constructBarlow _ = _2 <<< constructBarlow (Proxy :: Proxy rest)
else instance
  ( Fail (Beside (Text "barlow: `%") (Beside (Quote n) (Text "` is out of range – a Tuple only has `%1` and `%2`")))
  , Unreachable
  ) =>
  ConstructBarlow (TCons (ProductIndex n) rest) p (Tuple x y) t a b where
  constructBarlow _ = unreachable
-- Readable errors for lenses applied to the wrong type
else instance
  ( Fail (Above (Text "barlow: `?` expects a Maybe, but got") (Quote s))
  , Unreachable
  ) =>
  ConstructBarlow (TCons QuestionMark rest) p s t a b where
  constructBarlow _ = unreachable
else instance
  ( Fail (Above (Text "barlow: `>` expects an Either, but got") (Quote s))
  , Unreachable
  ) =>
  ConstructBarlow (TCons RightArrow rest) p s t a b where
  constructBarlow _ = unreachable
else instance
  ( Fail (Above (Text "barlow: `<` expects an Either, but got") (Quote s))
  , Unreachable
  ) =>
  ConstructBarlow (TCons LeftArrow rest) p s t a b where
  constructBarlow _ = unreachable
else instance
  ( Fail (Above (Text "barlow: `+` expects a Traversable or a record, but got") (Quote s))
  , Unreachable
  ) =>
  ConstructBarlow (TCons Plus rest) p s t a b where
  constructBarlow _ = unreachable
-- Selecting a field (or `{...}`) on something that isn't a record:
-- unwrap it if it's a newtype, fail with a readable error if it's a built-in type
else instance
  ( IsBuiltin s builtin
  , ConstructNonRecord builtin (TCons (RecordField sym) rest) p s t a b
  ) =>
  ConstructBarlow (TCons (RecordField sym) rest) p s t a b where
  constructBarlow _ = constructNonRecord (Proxy :: Proxy builtin) (Proxy :: Proxy (TCons (RecordField sym) rest))
else instance
  ( IsBuiltin s builtin
  , ConstructNonRecord builtin (TCons (Fields fields) rest) p s t a b
  ) =>
  ConstructBarlow (TCons (Fields fields) rest) p s t a b where
  constructBarlow _ = constructNonRecord (Proxy :: Proxy builtin) (Proxy :: Proxy (TCons (Fields fields) rest))
-- Instance for generic
else instance
  ( Generic s repS
  , CheckConstructor tlist s repS
  , ConstructBarlowGeneric tlist p repS repS a a
  , Profunctor p
  ) =>
  ConstructBarlow
    tlist
    p
    s
    s
    a
    a where
  constructBarlow _ = _ToGeneric <<< constructBarlowGeneric (Proxy :: Proxy tlist)

-- | Fails with a readable message (listing the available constructors) if `%Name` names a constructor that doesn't exist.
class CheckConstructor (lenses :: TList) (s :: Type) (rep :: Type)

instance
  ( HasConstructor sym rep found
  , CheckConstructorFound found sym s rep
  ) =>
  CheckConstructor (TCons (Percentage sym) rest) s rep
else instance CheckConstructor lenses s rep

class HasConstructor (sym :: Symbol) (rep :: Type) (found :: Boolean) | sym rep -> found

instance HasConstructor sym (Constructor sym x) True
else instance HasConstructor sym (Constructor other x) False
else instance
  ( HasConstructor sym l foundL
  , HasConstructor sym r foundR
  , Or foundL foundR found
  ) =>
  HasConstructor sym (Sum l r) found
else instance HasConstructor sym rep False

class CheckConstructorFound (found :: Boolean) (sym :: Symbol) (s :: Type) (rep :: Type)

instance CheckConstructorFound True sym s rep
instance
  ( ConstructorNames rep names
  , Fail
      ( Above
          (Beside (Text "barlow: the constructor `") (Beside (Text sym) (Beside (Text "` does not exist in ") (Quote s))))
          (Beside (Text "available constructors: ") names)
      )
  ) =>
  CheckConstructorFound False sym s rep

class ConstructorNames (rep :: Type) (names :: Doc) | rep -> names

instance ConstructorNames (Constructor name x) (Text name)
else instance
  ( ConstructorNames l namesL
  , ConstructorNames r namesR
  ) =>
  ConstructorNames (Sum l r) (Beside namesL (Beside (Text ", ") namesR))
else instance ConstructorNames rep (Text "")

-- | `True` for built-in types that can never be newtypes, so selecting a field on them is definitely a mistake.
class IsBuiltin (s :: Type) (builtin :: Boolean) | s -> builtin

instance IsBuiltin String True
else instance IsBuiltin Int True
else instance IsBuiltin Number True
else instance IsBuiltin Boolean True
else instance IsBuiltin Char True
else instance IsBuiltin Unit True
else instance IsBuiltin (Array a) True
else instance IsBuiltin (Maybe a) True
else instance IsBuiltin (Either a b) True
else instance IsBuiltin (Tuple a b) True
else instance IsBuiltin s False

-- | A field step (`name` or `{...}`) on something that isn't a record.
class ConstructNonRecord (builtin :: Boolean) (lenses :: TList) p s t a b | builtin lenses s -> t a b where
  constructNonRecord :: Proxy builtin -> Proxy lenses -> Optic p s t a b

-- not a newtype either: readable error
instance
  ( Fail (Above (Beside (Text "barlow: cannot select the field ") (Beside (QuoteLabel sym) (Text " because this is not a record:"))) (Quote s))
  , Unreachable
  ) =>
  ConstructNonRecord True (TCons (RecordField sym) rest) p s t a b where
  constructNonRecord _ _ = unreachable
else instance
  ( Fail (Above (Text "barlow: `{...}` expects a record, but got") (Quote s))
  , Unreachable
  ) =>
  ConstructNonRecord True lenses p s t a b where
  constructNonRecord _ _ = unreachable

-- possibly a newtype: unwrap it first, as if the path contained a `!`
instance
  ( Newtype s inner
  , ConstructBarlow lenses p inner inner a a
  , Profunctor p
  ) =>
  ConstructNonRecord False lenses p s s a a where
  constructNonRecord _ _ = _Newtype <<< constructBarlow (Proxy :: Proxy lenses)
