{ name = "purescript-zipperm"
, dependencies =
  [ "arrays"
  , "control"
  , "foldable-traversable"
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
, sources = [ "src/**/*.purs" ]
}
