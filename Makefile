.PHONY: all
all: build

.PHONY: build
build:
	stack build --pedantic

.PHONY: test
test:
	stack test --pedantic

.PHONY: watch
watch:
	stack test --fast --pedantic --file-watch

.PHONY: clean
clean:
	git clean -xfd
