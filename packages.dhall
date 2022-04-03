let upstream =
      https://raw.githubusercontent.com/purescript/package-sets/prepare-0.15/src/packages.dhall sha256:0dc2211060c4f5fac8dc6797b0eb9e192aa19494b07b30952c64c4f7881f4574

in  upstream
  with metadata.version = "v0.15.0-alpha-02"
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
        "https://github.com/working-group-purescript-es/purescript-spec-discovery.git"
    , version = "v0.15.0-update"
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
