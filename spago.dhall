{ name = "purescript-zipperm"
, dependencies =
  [ "arrays"
  , "console"
  , "control"
  , "effect"
  , "foldable-traversable"
  , "identity"
  , "lazy"
  , "lists"
  , "maybe"
  , "nonempty"
  , "prelude"
  , "quickcheck"
  , "transformers"
  , "tuples"
  , "unfoldable"
  , "unordered-collections"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
