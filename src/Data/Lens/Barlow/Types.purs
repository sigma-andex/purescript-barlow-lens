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

foreign import data RecordField :: Symbol -> LensType
