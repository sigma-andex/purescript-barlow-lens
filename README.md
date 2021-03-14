# purescript-barlow-lens 🔭

Barlow lens increases your magnification and let's you see the stars ✨

*Ehh, wat ?*

Barlow lens is a lens for records that makes it easy to zoom deep into the record.

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

## Credits

This lib was heavily inspired by this incredible [blog post](https://blog.csongor.co.uk/purescript-safe-printf/#The%20problem).
