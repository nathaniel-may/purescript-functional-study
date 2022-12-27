# runs all github actions that would trigger on push locally in docker
.PHONY: ci
ci:
	act

# build everything but the tests
.PHONY: build
build:
	spago build --purs-args "--stash" --path src

# build and run tests
.PHONY: test
test:
	spago -x test.dhall test --purs-args "--stash"
