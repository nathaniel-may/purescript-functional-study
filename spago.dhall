{ name = "purescript-zipperm"
, dependencies =
  [ "arrays", "console", "effect", "maybe", "prelude" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
