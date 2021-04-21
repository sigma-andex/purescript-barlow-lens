module Data.Lens.Barlow.Types where

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
