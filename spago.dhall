{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
  [ "affjax"
  , "console"
  , "css"
  , "effect"
  , "exceptions"
  , "foldable-traversable"
  , "halogen"
  , "halogen-css"
  , "halogen-vdom"
  , "monad-loops"
  , "profunctor-lenses"
  , "psci-support"
  , "random"
  , "test-unit"
  , "webaudio"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
