.PHONY: all
all: build

.PHONY: build
build:
	stack build --pedantic amy

.PHONY: test
test:
	stack test --pedantic amy
	stack test --pedantic amy-integration-tests
	(cd integration-tests && stack exec amy-integration-tests)

.PHONY: watch
watch:
	stack test --fast --pedantic --file-watch amy

.PHONY: clean
clean:
	git clean -xfd
