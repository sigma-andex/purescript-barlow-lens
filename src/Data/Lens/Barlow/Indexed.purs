-- | Construction of indexed barlow optics, i.e. paths containing at least one `#`.
-- |
-- | A path is split at its first `#`. The part before it is an ordinary barlow optic,
-- | the `#` itself becomes an indexed traversal, and the remainder is built at the
-- | profunctor `Indexed p i`, so the index is carried all the way to the focus.
-- | Each further `#` adds its index to the right: `Tuple (Tuple i j) k`.
module Data.Lens.Barlow.Indexed
  ( class ConstructIndexed
  , constructIndexed
  , class ConstructIndexedSplit
  , constructIndexedSplit
  , class ConstructIndexedRest
  , constructIndexedRest
  , class ConstructIndexedRestSplit
  , constructIndexedRestSplit
  , class IndexedStep
  , indexedStep
  , class SplitHash
  , class ConsSplit
  ) where

import Prelude

import Data.Lens (class Wander, IndexedOptic)
import Data.Lens.Barlow.Construction (class ConstructBarlow, constructBarlow)
import Data.Lens.Barlow.Internal (class RecordElem, class TraverseFields, itraverseRecord, class Unreachable, unreachable)
import Data.Lens.Barlow.Types (Hash, HashAt, NoHash, Split, TCons, TList, TNil)
import Data.Lens.Indexed (itraversed)
import Data.Lens.Internal.Indexed (Indexed(..))
import Data.Profunctor (lcmap)
import Data.TraversableWithIndex (class TraversableWithIndex)
import Data.Tuple (Tuple(..))
import Prim.RowList as RL
import Prim.TypeError (class Fail, Text)
import Type.Proxy (Proxy(..))

-- | Splits a lens list at its first `#`.
class SplitHash (lenses :: TList) (out :: Split) | lenses -> out

instance SplitHash TNil (NoHash TNil)
else instance SplitHash (TCons Hash rest) (HashAt TNil rest)
else instance (SplitHash rest split, ConsSplit h split out) => SplitHash (TCons h rest) out

class ConsSplit :: forall k. k -> Split -> Split -> Constraint
class ConsSplit h (split :: Split) (out :: Split) | h split -> out

instance ConsSplit h (NoHash l) (NoHash (TCons h l))
instance ConsSplit h (HashAt prefix rest) (HashAt (TCons h prefix) rest)

-- | The optic introduced by `#`: an indexed traversal over a `TraversableWithIndex`,
-- | or over all fields of a homogeneous record (indexed by field name).
class IndexedStep s t a b i | s -> t a b i where
  indexedStep :: forall p. Wander p => IndexedOptic p i s t a b

instance (RL.RowToList r rl, RecordElem rl a, TraverseFields rl r a) => IndexedStep { | r } { | r } a a String where
  indexedStep = itraverseRecord @a (Proxy :: Proxy rl)
else instance (TraversableWithIndex i f) => IndexedStep (f a) (f b) a b i where
  indexedStep = itraversed

-- | Builds an indexed optic from a lens list.
class ConstructIndexed (lenses :: TList) p i s t a b | lenses s -> t a b i where
  constructIndexed :: Proxy lenses -> IndexedOptic p i s t a b

instance (SplitHash lenses split, ConstructIndexedSplit split p i s t a b) => ConstructIndexed lenses p i s t a b where
  constructIndexed _ = constructIndexedSplit (Proxy :: Proxy split)

class ConstructIndexedSplit (split :: Split) p i s t a b | split s -> t a b i where
  constructIndexedSplit :: Proxy split -> IndexedOptic p i s t a b

instance
  ( Fail (Text "barlow: an indexed optic needs at least one `#` in its path")
  , Unreachable
  ) =>
  ConstructIndexedSplit (NoHash lenses) p i s t a b where
  constructIndexedSplit _ = unreachable

instance
  ( ConstructBarlow prefix p s t fx fy
  , IndexedStep fx fy x y j
  , Wander p
  , ConstructIndexedRest rest p j x y a b i
  ) =>
  ConstructIndexedSplit (HashAt prefix rest) p i s t a b where
  constructIndexedSplit _ =
    constructBarlow (Proxy :: Proxy prefix)
      <<< indexedStep
      <<< constructIndexedRest (Proxy :: Proxy rest)

-- | Builds the part of the path after a `#`, where the current index is `j`.
class ConstructIndexedRest (lenses :: TList) p j s t a b i | lenses j s -> t a b i where
  constructIndexedRest :: Proxy lenses -> Indexed p i a b -> Indexed p j s t

instance (SplitHash lenses split, ConstructIndexedRestSplit split p j s t a b i) => ConstructIndexedRest lenses p j s t a b i where
  constructIndexedRest _ = constructIndexedRestSplit (Proxy :: Proxy split)

class ConstructIndexedRestSplit (split :: Split) p j s t a b i | split j s -> t a b i where
  constructIndexedRestSplit :: Proxy split -> Indexed p i a b -> Indexed p j s t

-- no further `#`: an ordinary barlow optic, run at the profunctor `Indexed p j`
instance
  ( ConstructBarlow lenses (Indexed p j) s t a b
  ) =>
  ConstructIndexedRestSplit (NoHash lenses) p j s t a b j where
  constructIndexedRestSplit _ = constructBarlow (Proxy :: Proxy lenses)

-- another `#`: combine its index `k` with the current one into `Tuple j k`
instance
  ( ConstructBarlow prefix (Indexed p j) s t fu fv
  , IndexedStep fu fv u v k
  , Wander p
  , ConstructIndexedRest rest p (Tuple j k) u v a b i
  ) =>
  ConstructIndexedRestSplit (HashAt prefix rest) p j s t a b i where
  constructIndexedRestSplit _ =
    constructBarlow (Proxy :: Proxy prefix)
      <<< indexedStep
      <<< reassoc
      <<< constructIndexedRest (Proxy :: Proxy rest)
    where
    reassoc :: Indexed p (Tuple j k) u v -> Indexed (Indexed p j) k u v
    reassoc (Indexed pjk) = Indexed (Indexed (lcmap (\(Tuple j (Tuple k x)) -> Tuple (Tuple j k) x) pjk))
