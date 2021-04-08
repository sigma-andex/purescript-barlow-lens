# purescript-barlow-lens 🔭

Barlow lens increases your magnification and let's you see the stars ✨

*Ehh, wat ?*

Barlow lens is a lens for records that makes it easy to zoom deep into the record.

## Installation

```bash
spago install barlow-lens
```

## Usage 

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

### Deep sky 🌌

Now you can also zoom into `Maybe`s using `?`...

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
... and `Either`s using `<` for `Left` and `>` for `Right`...

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

... and `Array`s (and other `Traversable`s) using `+` ...

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

... and `Newtype`s using `!`

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

## Credits

This lib was heavily inspired by this incredible [blog post](https://blog.csongor.co.uk/purescript-safe-printf/#The%20problem).
