let conf = ./spago.dhall

in conf // {
  sources = conf.sources # [ "example/src/**/*.purs" ],
  dependencies = conf.dependencies # [ "effect", "foldable-traversable", "midi", "web-dom" ]
}
