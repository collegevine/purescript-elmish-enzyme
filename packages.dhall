let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.15.2-20220613/packages.dhall
        sha256:99f976d547980055179de2245e428f00212e36acd55d74144eab8ad8bf8570d8

in  upstream
  with elmish =
    { repo = "https://github.com/collegevine/purescript-elmish.git"
    , version = "v0.8.1"
    , dependencies =
      [ "aff"
      , "argonaut-core"
      , "arrays"
      , "bifunctors"
      , "console"
      , "debug"
      , "effect"
      , "either"
      , "foldable-traversable"
      , "foreign"
      , "foreign-object"
      , "functions"
      , "integers"
      , "js-date"
      , "maybe"
      , "nullable"
      , "partial"
      , "prelude"
      , "refs"
      , "strings"
      , "typelevel-prelude"
      , "unsafe-coerce"
      , "undefined-is-not-a-problem"
      , "web-dom"
      , "web-html"
      ]
    }
  with elmish-html =
    { dependencies = [ "prelude", "record" ]
    , repo = "https://github.com/collegevine/purescript-elmish-html.git"
    , version = "v0.7.1"
    }
