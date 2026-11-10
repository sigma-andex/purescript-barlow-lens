module Data.Lens.Barlow.Types where

-- typelevel list
data TList

foreign import data TNil :: TList

foreign import data TCons :: forall k. k -> TList -> TList

data Nat

foreign import data Z :: Nat

foreign import data S :: Nat -> Nat

type N0 = Z

type N1 = S N0

type N2 = S N1

type N3 = S N2

type N4 = S N3

type N5 = S N4

type N6 = S N5

type N7 = S N6

type N8 = S N7

type N9 = S N8

-- ADT representing the different types of lenses at the type level
data LensType

foreign import data QuestionMark :: LensType

foreign import data RightArrow :: LensType

foreign import data LeftArrow :: LensType

foreign import data Plus :: LensType

foreign import data ExclamationMark :: LensType

foreign import data Percentage :: forall k. k -> LensType

-- | `%n` – the n-th argument of a constructor (or `Tuple`)
foreign import data ProductIndex :: Int -> LensType

foreign import data RecordField :: Symbol -> LensType

-- | `#` – indexed traversal
foreign import data Hash :: LensType

-- | `[key]` – `ix key` (or a record field, for records)
foreign import data Bracket :: Symbol -> LensType

-- | `[?key]` – `at key`
foreign import data BracketAt :: Symbol -> LensType

-- | `{a,b}` – a lens onto a sub-record
foreign import data Fields :: TList -> LensType

-- | Result of splitting a lens list at the first `#`
data Split

foreign import data NoHash :: TList -> Split

foreign import data HashAt :: TList -> TList -> Split
