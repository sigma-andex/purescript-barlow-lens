{ name = "barlow-lens"
, dependencies =
  [ "either"
  , "foldable-traversable"
  , "maybe"
  , "newtype"
  , "prelude"
  , "profunctor"
  , "profunctor-lenses"
  , "strings"
  , "tuples"
  , "typelevel-prelude"
  , "aff"
  , "effect"
  , "psci-support"
  , "spec"
  , "spec-discovery"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
, license = "MIT-0"
, repository = "https://github.com/sigma-andex/purescript-barlow-lens.git"
}
