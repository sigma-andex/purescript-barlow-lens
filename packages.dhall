let upstream =
      https://raw.githubusercontent.com/purescript/package-sets/prepare-0.15/src/packages.dhall

in  upstream
  with metadata.version = "v0.15.0-alpha-05"
  with spec =
    { repo = "https://github.com/purescript-spec/purescript-spec.git"
    , version = "master"
    , dependencies =
      [ "aff"
      , "ansi"
      , "avar"
      , "console"
      , "exceptions"
      , "foldable-traversable"
      , "fork"
      , "now"
      , "pipes"
      , "prelude"
      , "strings"
      , "transformers"
      ]
    }
  with spec-discovery =
    { repo =
        "https://github.com/purescript-spec/purescript-spec-discovery.git"
    , version = "master"
    , dependencies =
      [ "aff"
      , "aff-promise"
      , "arrays"
      , "console"
      , "effect"
      , "foldable-traversable"
      , "node-fs"
      , "prelude"
      , "spec"
      ]
    }
