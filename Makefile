examples_llvm: $(patsubst %.amy,%.ll,$(wildcard examples/*.amy))

.PHONY: all
all: build $(examples_llvm)

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
