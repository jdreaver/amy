RTS_LL = rts/rts.ll
RTS_C = rts/rts.c

.PHONY: all
all: build

.PHONY: build
build: $(RTS_LL)
	stack build --pedantic amy

.PHONY: test
test: $(RTS_LL)
	stack test --pedantic amy
	stack test --pedantic amy-integration-tests
	(cd integration-tests && RTS_LL_LOCATION=../rts/rts.ll stack exec amy-integration-tests)

.PHONY: watch
watch:
	stack test --fast --pedantic --file-watch amy

$(RTS_LL): $(RTS_C)
	(cd rts && clang -S -emit-llvm rts.c)

.PHONY: clean
clean:
	git clean -xfd
