# purescript-barlow-lens 🔭

Barlow lens increases your magnification and let's you see the stars ✨

*In other words,* barlow lens makes creating complex lenses such as record lenses super simple.

## Installation

```bash
spago install barlow-lens
```

## tl;dr 

```purescript
sky =
  { zodiac:
      { virgo:
          { alpha: "Spica"
          }
      }
  }

view (barlow (key :: _ "zodiac.virgo.alpha")) sky
-- "Spica"
over (barlow (key :: _ "zodiac.virgo.alpha")) toUpper sky
-- { zodiac: { virgo: { alpha: "SPICA" } } }
    
-- view (barlow (key :: _ "zodiac.virgo.alfa")) sky 
-- doesn't compile
```

### Features 
Barlow supports lens creation for the following types:
- 📦🐈 [`Maybe`](#Maybe)
- 🤷🏽‍♀️ [`Either`](#Either)
- 📜 [`Array`](#Array-and-other-Traversables) (and other `Traversable`s)
- 🎁 [`Newtype`](#Newtype)
- 🤖 [`Data types`](#Data-types)

### Deep sky 🌌

#### Maybe 
Use `?` to zoom into a `Maybe`.

```purescript 
sky =
  { zodiac:
      Just
        { virgo:
            Just
              { alpha: Just "Spica"
              }
        }
  }

preview (barlow (key :: _ "zodiac?.virgo?.alpha?")) sky
```

#### Either
Use `<` for `Left` and `>` for `Right` to zoom into an `Either`.

```purescript 
sky =
  { zodiac:
      Right
        { virgo:
            Just
              { alpha: Left "Spica"
              }
        }
  }

preview (barlow (key :: _ "zodiac>.virgo?.alpha<")) sky
```


#### Array and other Traversables
Use `+` to zoom into `Traversable`s like `Array`.

```purescript 

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

over (barlow (key :: _ "zodiac+.virgo?.star")) toUpper sky
```

#### Newtype
Use `!` to zoom into a `Newtype`.

```purescript
newtype Alpha = Alpha { alpha :: String }
instance alphaNT :: Newtype Alpha { alpha :: String }

sky =
  { zodiac:
      Just
        { virgo:
            Alpha { alpha: "Spica"
            }
        }
  }

preview (barlow (key :: _ "zodiac?.virgo!.alpha")) sky
```

#### Data types

Barlow supports zooming into arbitrary sum and product types as long as there is a `Generic` instance. 

Use `%<NAME>` to zoom into sum types, where `<NAME>` is the name of your data constructor. E.g. `%Virgo` for the data constructor `Virgo`. 

Use `%<INDEX>` to zoom into product types, where `<INDEX>` is an integer between 1 and 9. Note that counting for product types and tuples usually starts with 1 and not 0. So the first element of a product is `%1`.

It is more readable if you separate your sum lens from your product lens with a `.` dot. 

```purescript 
data Zodiac
  = Carina { alpha :: String } 
  | Virgo { alpha :: String } { beta :: String } { gamma :: String } { delta :: String } 
  | CanisMaior String 

derive instance genericZodiac :: Generic Zodiac _

instance showZodiac :: Show Zodiac where
  show = genericShow

sky =
  { zodiac:
      Virgo { alpha : "Spica"} { beta: "β Vir"} { gamma: "γ Vir B"} { delta: "δ Vir"}
  }

over (barlow (key :: _ "zodiac.%Virgo.%4.delta")) toUpper sky
-- { zodiac: Virgo { alpha : "Spica"} { beta: "β Vir"} { gamma: "γ Vir B"} { delta: "Δ VIR"} }
```

## Credits

This lib was heavily inspired by this incredible [blog post](https://blog.csongor.co.uk/purescript-safe-printf/#The%20problem). Thanks also to @i-am-the-slime for pushing me to go further and for reviewing my PRs. 
