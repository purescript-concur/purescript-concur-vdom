{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "concur-vdom"
, dependencies =
  [ "concur-core"
  , "effect"
  , "foldable-traversable"
  , "halogen-vdom"
  , "maybe"
  , "newtype"
  , "prelude"
  , "refs"
  , "safe-coerce"
  , "web-dom"
  , "web-events"
  , "web-html"
  ]
, license = "MIT"
, repository = "https://github.com/purescript-concur/purescript-concur-vdom"
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
