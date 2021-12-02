.PHONY: build
build: clean
	stack build

.PHONY: test
test: clean
	stack test

.PHONY: run
run: build
	stack run

.PHONY: clean
clean:
	stack clean