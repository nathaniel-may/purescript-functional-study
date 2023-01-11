# runs all github actions that would trigger on push locally in docker
.PHONY: ci
ci:
	act

# build everything but the tests
.PHONY: build
build:
	spago -x spago.dhall build

# build and run tests
.PHONY: test
test:
	spago -x test.dhall test

# build and run tests
.PHONY: example
example:
	spago -x test.dhall run -m Test.Examples.RollingCache
