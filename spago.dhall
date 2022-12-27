{ name = "my-project"
, dependencies =
  [ "arrays", "console", "effect", "maybe", "prelude", "test-unit" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
-- TODO move test-unit to a new test.dhall
