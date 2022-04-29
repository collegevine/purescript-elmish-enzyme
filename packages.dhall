let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.15.0-20220429/packages.dhall
        sha256:03c682bff56fc8f9d8c495ffcc6f524cbd3c89fe04778f965265c08757de8c9d
in  upstream
  with elmish =
    { repo =
        "https://github.com/working-group-purescript-es/purescript-elmish.git"
    , version = "v0.15.0-update"
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
    , version = "v0.6.0"
    }
  with undefined-is-not-a-problem =
    { repo = "https://github.com/working-group-purescript-es/purescript-undefined-is-not-a-problem.git"
    , version = "v0.15.0-update"
    , dependencies =
      [ "assert"
      , "effect"
      , "either"
      , "foreign"
      , "maybe"
      , "prelude"
      , "random"
      , "tuples"
      , "unsafe-coerce"
      ]
    }
  with debug =
    { dependencies = [ "prelude", "functions" ]
    , repo = "https://github.com/working-group-purescript-es/purescript-debug.git"
    , version = "es-modules"
    }
