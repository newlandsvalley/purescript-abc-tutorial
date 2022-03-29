{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "abc-tutorial"
, dependencies =
  [ "abc-melody"
  , "abc-parser"
  , "aff"
  , "arrays"
  , "colors"
  , "const"
  , "css"
  , "either"
  , "halogen"
  , "halogen-components"
  , "halogen-css"
  , "lists"
  , "maybe"
  , "ordered-collections"
  , "prelude"
  , "soundfonts"
  , "string-parsers"
  , "strings"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
