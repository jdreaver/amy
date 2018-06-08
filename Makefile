.PHONY: all
all: build

.PHONY: build
build:
	stack build --pedantic

.PHONY: test
test:
	stack test --pedantic
	(cd integration-tests && stack exec amy-integration-tests)

.PHONY: watch
watch:
	stack test --fast --pedantic --file-watch

.PHONY: clean
clean:
	git clean -xfd
