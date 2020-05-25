{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
  [ "affjax"
  , "concur-core"
  , "concur-react"
  , "console"
  , "effect"
  , "exceptions"
  , "foldable-traversable"
  , "monad-loops"
  , "profunctor-lenses"
  , "psci-support"
  , "random"
  , "routing"
  , "test-unit"
  , "webaudio"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
