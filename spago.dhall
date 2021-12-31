{ name = "elmish-enzyme"
, dependencies =
  [ "aff"
  , "arrays"
  , "console"
  , "debug"
  , "effect"
  , "elmish"
  , "exceptions"
  , "foldable-traversable"
  , "foreign"
  , "functions"
  , "prelude"
  , "psci-support"
  , "transformers"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
, license = "MIT"
, repository = "https://github.com/collegevine/purescript-elmish-enzyme.git"
}
