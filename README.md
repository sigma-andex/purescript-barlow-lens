# purescript-barlow-lens 🔭

Barlow lens increases your magnification and let's you see the stars ✨

*In other words,* barlow lens makes creating complex lenses such as record lenses super simple.

## Installation

**Note:** Versions `v1.0.0` and later require Purescript > `v0.15.10`. If you are on an older version, use [`v0.9.0`](https://github.com/sigma-andex/purescript-barlow-lens/tree/v0.9.0) instead or upgrade your Purescript version.

```bash
spago install barlow-lens
```

## tl;dr 

```purescript
import Data.Lens 
import Data.Lens.Barlow
import Data.String (toUpper)

sky = { zodiac: { virgo: { alpha: "Spica" } } }

spica = view (barlow @"zodiac.virgo.alpha") sky
-- "Spica"
upped = over (barlow @"zodiac.virgo.alpha") toUpper sky
-- { zodiac: { virgo: { alpha: "SPICA" } } }
    
-- alfa = view (barlow @"zodiac.virgo.alfa") sky 
-- doesn't compile
```

or use the barlow helpers to make it even shorter:

```purescript
import Data.Lens.Barlow
import Data.Lens.Barlow.Helpers
import Data.String (toUpper)

sky = { zodiac: { virgo: { alpha: "Spica" } } }

spica = view @"zodiac.virgo.alpha" sky
-- "Spica"
upped = over @"zodiac.virgo.alpha" toUpper sky
-- { zodiac: { virgo: { alpha: "SPICA" } } }
```

### Features 
Barlow supports lens creation for the following types:
- 🥇 [`Records`](#tldr)
- 📦🐈 [`Maybe`](#Maybe)
- 🤷🏽‍♀️ [`Either`](#Either)
- 📜 [`Array`](#Array-and-other-Traversables) (and other `Traversable`s)
- 🎁 [`Newtype`](#Newtype)
- 🤖 [`Data types`](#Data-types)
- 👯 [`Tuples`](#Tuples)
- 🔢 [`Indexed access`](#Indexed-access) with `[key]` and `[?key]`
- 🧩 [`Sub-records`](#Sub-records) with `{a,b}`
- 🧭 [`Indexed traversals`](#Indexed-traversals) with `#`

### Deep sky 🌌

#### Maybe 
Use `?` to zoom into a `Maybe`.

```purescript 
import Data.Maybe (Maybe(..))
import Data.Lens.Barlow
import Data.Lens.Barlow.Helpers

sky =
  { zodiac:
      Just
        { virgo:
            Just
              { alpha: Just "Spica"
              }
        }
  }

spica = preview @"zodiac?.virgo?.alpha?" sky
```

#### Either
Use `<` for `Left` and `>` for `Right` to zoom into an `Either`.

```purescript 
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Lens.Barlow
import Data.Lens.Barlow.Helpers
import Data.String (toUpper)

sky =
  { zodiac:
      Right
        { virgo:
            Just
              { alpha: Left "Spica"
              }
        }
  }

spica = preview @"zodiac>.virgo?.alpha<" sky
```


#### Array and other Traversables
Use `+` to zoom into `Traversable`s like `Array`.

```purescript 
import Data.Maybe (Maybe(..))
import Data.Lens.Barlow
import Data.Lens.Barlow.Helpers
import Data.String (toUpper)

sky =
  { zodiac:
      [ { virgo:
            Just
              { star: "Spica"
              }
        }
      , { virgo:
            Just
              { star: "Serpentis"
              }
        }
      ]
  }

upped = over @"zodiac+.virgo?.star" toUpper sky
```

#### Newtype
Newtypes around records are unwrapped automatically when you select a field, so they work just like plain records.

```purescript
import Data.Lens.Barlow
import Data.Lens.Barlow.Helpers
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)

newtype Virgo = Virgo { alpha :: String }
derive instance Newtype Virgo _

sky = { zodiac: Just { virgo: Virgo { alpha: "Spica" } } }

spica = preview @"zodiac?.virgo.alpha" sky
-- Just "Spica"
```

This also works for nested newtypes and for [sub-records](#Sub-records) (`"virgo.{alpha}"`).

Use `!` to zoom into any other newtype explicitly:

```purescript
newtype Name = Name String
derive instance Newtype Name _

sky = { zodiac: Just { virgo: { alpha: Name "Spica" } } }

spica = preview @"zodiac?.virgo.alpha!" sky
-- Just "Spica"
```

Zooming through a newtype can't change the type of the focus. For example, `over` can't turn a `String` field inside a newtype into an `Int`.

#### Data types

Barlow supports zooming into arbitrary sum and product types as long as there is a `Generic` instance. 

Use `%<NAME>` to zoom into sum types, where `<NAME>` is the name of your data constructor. E.g. `%Virgo` for the data constructor `Virgo`. 

Use `%<INDEX>` to zoom into product types, where `<INDEX>` is a positive integer. Note that counting for product types and tuples usually starts with 1 and not 0. So the first element of a product is `%1`.

It is more readable if you separate your sum lens from your product lens with a `.` dot. 

```purescript 
import Data.Lens.Barlow
import Data.Lens.Barlow.Helpers
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.String.Common (toUpper)

data Zodiac
  = Carina { alpha :: String } 
  | Virgo { alpha :: String } { beta :: String } { gamma :: String } { delta :: String } 
  | CanisMaior String 

derive instance Generic Zodiac _

-- Optionally derive a show instance
instance Show Zodiac where
  show = genericShow

sky =
  { zodiac:
      Virgo { alpha : "Spica"} { beta: "β Vir"} { gamma: "γ Vir B"} { delta: "δ Vir"}
  }

upped = over @"zodiac.%Virgo.%4.delta" toUpper sky
-- { zodiac: Virgo { alpha : "Spica"} { beta: "β Vir"} { gamma: "γ Vir B"} { delta: "Δ VIR"} }
```

#### Tuples

Use `%1` and `%2` to zoom into the first and second element of a `Tuple`. Unlike other data types, tuples support changing the type of the focus.

```purescript
sky = { pair: Tuple "Spica" 1 }

view @"pair.%1" sky
-- "Spica"
set @"pair.%2" "one" sky
-- { pair: Tuple "Spica" "one" }
```

Nested tuples are nested optics: in `Tuple a (Tuple b c)`, `c` is at `%2.%2`.

#### Indexed access

Use `[key]` to zoom into a single element of anything with an `Index` instance (`Array`, `List`, `Map`, `Object`, ...). Keys can be numbers (for `Int` keys) or text (for `String` keys).

```purescript
sky =
  { zodiac: [ { name: "Virgo" }, { name: "Leo" } ]
  , named: Map.fromFoldable [ Tuple "virgo" "Spica" ]
  }

preview @"zodiac[1].name" sky
-- Just "Leo"
preview @"named[virgo]" sky
-- Just "Spica"
```

Use `[?key]` to get a lens onto `Maybe` the value (via `At`), which lets you insert or delete entries:

```purescript
set @"named[?leo]" (Just "Regulus") sky   -- inserts
set @"named[?virgo]" Nothing sky          -- deletes
```

On records, `[field]` selects a field, so you can use field names containing special characters such as `.`, `?` or `+`:

```purescript
view @"[alpha.centauri].name" { "alpha.centauri": { name: "Rigil Kentaurus" } }
```

#### Sub-records

Use `{a,b}` to zoom into several fields of a record at once:

```purescript
sky = { virgo: "Spica", leo: "Regulus", cancer: "Tarf" }

view @"{virgo, leo}" sky
-- { virgo: "Spica", leo: "Regulus" }
set @"{virgo, leo}" { virgo: "α Vir", leo: "α Leo" } sky
-- { virgo: "α Vir", leo: "α Leo", cancer: "Tarf" }
```

`+` also works on records whose fields all have the same type, traversing every field:

```purescript
over @"{virgo, leo}+" toUpper sky
-- { virgo: "SPICA", leo: "REGULUS", cancer: "Tarf" }
```

#### Indexed traversals

Use `#` instead of `+` to traverse a container while keeping the position or key of each element. This gives you an indexed optic, created with `ibarlow` or used with the indexed helpers (`iover`, `itoListOf`, `ifoldMapOf`, `itraverseOf`).

```purescript
sky =
  { zodiac:
      [ { stars: [ "Spica", "Porrima" ] }
      , { stars: [ "Regulus" ] }
      ]
  }

itoListOf @"zodiac#.stars" sky
-- (Tuple 0 ["Spica","Porrima"]) : (Tuple 1 ["Regulus"]) : Nil
```

The index comes from `TraversableWithIndex`: positions for `Array` and `List`, keys for `Map`. On a record whose fields all have the same type, the index is the field name.

Several `#` combine their indices into nested tuples:

```purescript
itoListOf @"zodiac#.stars#" sky
-- (Tuple (Tuple 0 0) "Spica") : (Tuple (Tuple 0 1) "Porrima") : (Tuple (Tuple 1 0) "Regulus") : Nil

iover @"zodiac#.stars#" (\(Tuple i j) star -> show i <> "/" <> show j <> " " <> star) sky
```

`ibarlow` also works as a regular `IndexedTraversal`:

```purescript
stars :: IndexedTraversal' (Tuple Int Int) Sky String
stars = ibarlow @"zodiac#.stars#"
```

### Helpers

`Data.Lens.Barlow.Helpers` provides `view`, `preview`, `over`, `set`, `foldOf`, `toArrayOf`, `toListOf`, `firstOf`, `lastOf`, `lengthOf`, `has`, `hasn't` and `traverseOf`, plus the indexed `iover`, `itoListOf`, `ifoldMapOf` and `itraverseOf`.

### Error messages

Common mistakes produce readable compile errors, for example:

```
barlow: the field alfa does not exist in the record
{ alpha :: String
}
```

```
barlow: `?` expects a Maybe, but got
String
```

```
barlow: the constructor `Virgoo` does not exist in Constellation
available constructors: Virgo, Leo, Cancer
```

```
barlow: `%5` is out of range – this constructor only has 4 arguments
```

## Development

The toolchain (`purs`, `spago`, `purs-tidy`) is pinned in `package.json` and installed with [pnpm](https://pnpm.io). `pnpm-workspace.yaml` already allows the `purescript` package to run its install script, which downloads the `purs` compiler.

```bash
pnpm install
pnpm build   # spago build
pnpm test    # spago test
```

The custom compile errors are checked separately, by compiling snippets that must fail (see `test-compile-errors/`):

```bash
bash test-compile-errors/run.sh
```

### Editor setup

For type information, errors and hovers in VS Code, use [Iris](https://iris-lang.com), the successor of purescript-analyzer:

1. Install Iris following the [installation guide](https://iris-lang.com/docs/installation/).
2. Install the Iris extension (`purefunctor.purescript-analyzer`) from the VS Code Marketplace, and disable `nwolverson.ide-purescript` to avoid duplicate diagnostics.
3. Run `pnpm install` first: Iris prepares the workspace with the project's local `spago`, so it fails with "Workspace preparation failed" if `node_modules` is missing or outdated.

See the [language server docs](https://iris-lang.com/docs/language-server/) for other editors.

## Credits

This lib was heavily inspired by this incredible [blog post](https://blog.csongor.co.uk/purescript-safe-printf/#The%20problem). Thanks also to @i-am-the-slime for pushing me to go further and for reviewing my PRs. 
