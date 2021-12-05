let conf = ./spago.dhall

in conf // {
  sources = conf.sources # [ "test/**/*.purs" ],
  dependencies = conf.dependencies # [ "datetime", "elmish-html", "lcg", "maybe", "quickcheck", "spec", "transformers", "tuples" ]
}
