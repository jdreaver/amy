examples_llvm: $(patsubst %.amy,%.ll,$(wildcard examples/*.amy))

.PHONY: all
all: build $(examples_llvm)

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

examples/%.ll: examples/%.amy build
	@echo "; Generated from $<" > $@
	@echo "" >> $@
	stack exec amy -- $< >> $@
