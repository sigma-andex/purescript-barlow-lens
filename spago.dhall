{ name = "barlow-lens"
, dependencies =
  [ "console"
  , "debug"
  , "effect"
  , "profunctor-lenses"
  , "psci-support"
  , "spec"
  , "spec-discovery"
  , "typelevel-prelude"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
, license = "MIT-0"
, repository = "https://github.com/sigma-andex/purescript-barlow-lens.git"
}
