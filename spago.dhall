{ name = "barlow-lens"
, dependencies =
  [ "either"
  , "foldable-traversable"
  , "lists"
  , "maybe"
  , "newtype"
  , "prelude"
  , "profunctor"
  , "profunctor-lenses"
  , "tuples"
  , "typelevel-prelude"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs"]
, license = "MIT-0"
, repository = "https://github.com/sigma-andex/purescript-barlow-lens.git"
}
