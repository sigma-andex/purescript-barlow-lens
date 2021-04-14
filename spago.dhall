{ name = "barlow-lens"
, dependencies =
  [ "console"
  , "effect"
  , "foldable-traversable"
  , "profunctor-lenses"
  , "psci-support"
  , "spec"
  , "spec-discovery"
  , "typelevel-prelude"
  , "aff"
  , "either"
  , "maybe"
  , "newtype"
  , "prelude"
  , "profunctor"
  , "strings"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
, license = "MIT-0"
, repository = "https://github.com/sigma-andex/purescript-barlow-lens.git"
}
