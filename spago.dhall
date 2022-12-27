{ name = "purescript-zipperm"
, dependencies =
  [ "arrays"
  , "console"
  , "control"
  , "effect"
  , "lists"
  , "maybe"
  , "prelude"
  , "quickcheck"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
