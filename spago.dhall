{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "abc-tutorial"
, dependencies = [ "abc-melody"
                 , "abc-parser"
                 , "console"
                 , "css"
                 , "effect"
                 , "halogen"
                 , "halogen-css"
                 , "halogen-components"
                 , "soundfonts"
                 ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
