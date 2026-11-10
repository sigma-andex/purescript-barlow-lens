-- | Type-level helpers used by the barlow lens construction.
module Data.Lens.Barlow.Internal where

import Prelude

import Data.Lens (IndexedTraversal', Lens', Traversal', lens, wander)
import Data.Lens.Indexed (iwander)
import Data.Lens.Barlow.Types (TCons, TList, TNil)
import Data.Lens.Record (prop)
import Data.Profunctor.Strong (class Strong)
import Data.Reflectable (class Reflectable, reflectType)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Prim.Int as Int
import Prim.Row as Row
import Prim.RowList as RL
import Prim.Symbol as Symbol
import Prim.TypeError (class Fail, Above, Beside, Quote, QuoteLabel, Text)
import Record as Record
import Record.Builder (Builder)
import Record.Builder as Builder
import Type.Proxy (Proxy(..))

-- | A class without instances, so it can only ever be satisfied as a given constraint.
-- | Instances whose context contains a `Fail` (and therefore can never be selected)
-- | also require `Unreachable`, which lets them use `unreachable` as a body.
class Unreachable where
  unreachable :: forall a. a

------------------------------------------------------------------------------
-- Field lookup with a readable error message

-- | `Row.Cons sym a rest r`, but failing with a readable message if the field doesn't exist.
-- | The `Row.Cons` is only solved once the field is known to exist; otherwise the compiler
-- | would report a unification error before ever reaching the custom error.
class FieldOf (sym :: Symbol) (r :: Row Type) a (rest :: Row Type) | sym r -> a rest where
  getField :: Proxy sym -> Record r -> a
  setField :: Proxy sym -> a -> Record r -> Record r
  fieldLens :: forall p b r'. Strong p => Row.Cons sym b rest r' => Proxy sym -> p a b -> p (Record r) (Record r')

instance (RL.RowToList r rl, FieldOfRL sym rl r a rest) => FieldOf sym r a rest where
  getField _ = getFieldRL (Proxy :: Proxy sym) (Proxy :: Proxy rl)
  setField _ = setFieldRL (Proxy :: Proxy sym) (Proxy :: Proxy rl)
  fieldLens _ = fieldLensRL (Proxy :: Proxy sym) (Proxy :: Proxy rl)

class FieldOfRL (sym :: Symbol) (rl :: RL.RowList Type) (r :: Row Type) a (rest :: Row Type) | sym rl r -> a rest where
  getFieldRL :: Proxy sym -> Proxy rl -> Record r -> a
  setFieldRL :: Proxy sym -> Proxy rl -> a -> Record r -> Record r
  fieldLensRL :: forall p b r'. Strong p => Row.Cons sym b rest r' => Proxy sym -> Proxy rl -> p a b -> p (Record r) (Record r')

instance (IsSymbol sym, Row.Cons sym a rest r) => FieldOfRL sym (RL.Cons sym a tail) r a rest where
  getFieldRL sym _ = Record.get sym
  setFieldRL sym _ = Record.set sym
  fieldLensRL sym _ = prop sym
else instance
  ( Fail
      ( Above
          (Beside (Text "barlow: the field ") (Beside (QuoteLabel sym) (Text " does not exist in the record")))
          (Quote (Record r))
      )
  , Unreachable
  ) =>
  FieldOfRL sym RL.Nil r a rest where
  getFieldRL _ _ = unreachable
  setFieldRL _ _ = unreachable
  fieldLensRL _ _ = unreachable
else instance (FieldOfRL sym tail r a rest) => FieldOfRL sym (RL.Cons other b tail) r a rest where
  getFieldRL sym _ = getFieldRL sym (Proxy :: Proxy tail)
  setFieldRL sym _ = setFieldRL sym (Proxy :: Proxy tail)
  fieldLensRL sym _ = fieldLensRL sym (Proxy :: Proxy tail)

------------------------------------------------------------------------------
-- Keys for `[key]` and `[?key]`

-- | Turns the symbol inside `[...]` into a key of the container's key type.
class IndexKey (sym :: Symbol) k where
  indexKey :: Proxy sym -> k

instance (ParseInt sym n, Reflectable n Int) => IndexKey sym Int where
  indexKey _ = reflectType (Proxy :: Proxy n)
else instance (IsSymbol sym) => IndexKey sym String where
  indexKey _ = reflectSymbol (Proxy :: Proxy sym)
else instance
  ( Fail
      ( Beside (Text "barlow: `[")
          ( Beside (Text sym)
              (Beside (Text "]` needs a container with Int or String keys, but the key type is ") (Quote k))
          )
      )
  , Unreachable
  ) =>
  IndexKey sym k where
  indexKey _ = unreachable

-- | Parses a symbol of decimal digits into a type-level Int.
class ParseInt (sym :: Symbol) (n :: Int) | sym -> n

instance (Fail (Text "barlow: empty index `[]`")) => ParseInt "" n
else instance (ParseIntAcc sym sym 0 n) => ParseInt sym n

class ParseIntAcc (original :: Symbol) (sym :: Symbol) (acc :: Int) (n :: Int) | original sym acc -> n

instance ParseIntAcc original "" acc acc
else instance
  ( Symbol.Cons h t sym
  , Digit original h d
  , Int.Mul acc 10 acc10
  , Int.Add acc10 d acc'
  , ParseIntAcc original t acc' n
  ) =>
  ParseIntAcc original sym acc n

class Digit (original :: Symbol) (sym :: Symbol) (d :: Int) | sym -> d

instance Digit o "0" 0
else instance Digit o "1" 1
else instance Digit o "2" 2
else instance Digit o "3" 3
else instance Digit o "4" 4
else instance Digit o "5" 5
else instance Digit o "6" 6
else instance Digit o "7" 7
else instance Digit o "8" 8
else instance Digit o "9" 9
else instance
  ( Fail
      ( Beside (Text "barlow: `[")
          (Beside (Text o) (Text "]` is not a valid index – this container has Int keys, so use digits only"))
      )
  ) =>
  Digit o c d

------------------------------------------------------------------------------
-- `{a,b}` sub-record selection

class SelectFields (fields :: TList) (r :: Row Type) (sub :: Row Type) | fields r -> sub where
  getFields :: Proxy fields -> Record r -> Builder (Record ()) (Record sub)
  setFields :: Proxy fields -> Record sub -> Record r -> Record r

instance SelectFields TNil r () where
  getFields _ _ = identity
  setFields _ _ r = r

instance
  ( IsSymbol f
  , FieldOf f r a rx
  , SelectFields rest r subRest
  , Row.Lacks f subRest
  , Row.Cons f a subRest sub
  ) =>
  SelectFields (TCons f rest) r sub where
  getFields _ r = Builder.insert field (getField field r) <<< getFields (Proxy :: Proxy rest) r
    where
    field = Proxy :: Proxy f
  setFields _ sub r = setField field (Record.get field sub) (setFields (Proxy :: Proxy rest) (Record.delete field sub) r)
    where
    field = Proxy :: Proxy f

-- | A lens onto the sub-record containing only the given fields.
selectFields :: forall fields r sub. SelectFields fields r sub => Proxy fields -> Lens' (Record r) (Record sub)
selectFields fields = lens (\r -> Builder.buildFromScratch (getFields fields r)) (\r sub -> setFields fields sub r)

------------------------------------------------------------------------------
-- Traversing all fields of a homogeneous record

-- | Traverses all fields of a homogeneous record.
traverseRecord :: forall @a rl r. TraverseFields rl r a => Proxy rl -> Traversal' (Record r) a
traverseRecord rl = wander (\f -> traverseFields rl (const f))

-- | Traverses all fields of a homogeneous record, indexed by field name.
itraverseRecord :: forall @a rl r. TraverseFields rl r a => Proxy rl -> IndexedTraversal' String (Record r) a
itraverseRecord rl = iwander (traverseFields rl)

-- | The type of the first field, i.e. the element type of a homogeneous record.
class RecordElem (rl :: RL.RowList Type) (a :: Type) | rl -> a

instance RecordElem (RL.Cons l a tail) a
else instance RecordElem RL.Nil a

-- | Has no functional dependency on purpose, so a field of the wrong type falls through to the custom error.
class TraverseFields (rl :: RL.RowList Type) (r :: Row Type) a where
  traverseFields :: forall f. Applicative f => Proxy rl -> (String -> a -> f a) -> Record r -> f (Record r)

instance TraverseFields RL.Nil r a where
  traverseFields _ _ = pure
else instance
  ( IsSymbol l
  , Row.Cons l a rx r
  , TraverseFields tail r a
  ) =>
  TraverseFields (RL.Cons l a tail) r a where
  traverseFields _ f rec =
    (\v rec' -> Record.set field v rec')
      <$> f (reflectSymbol field) (Record.get field rec)
      <*> traverseFields (Proxy :: Proxy tail) f rec
    where
    field = Proxy :: Proxy l
else instance
  ( Fail
      ( Above
          (Text "barlow: `+` and `#` on a record need all fields to have the same type, but")
          ( Beside (Text "the field ")
              (Beside (QuoteLabel l) (Beside (Text " has type ") (Beside (Quote b) (Beside (Text " instead of ") (Quote a)))))
          )
      )
  ) =>
  TraverseFields (RL.Cons l b tail) r a where
  traverseFields _ _ = pure
