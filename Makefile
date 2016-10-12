#
# Makefile for oed
#

CC=corebuild
PACKAGES=-package re2

all: oed

oed:
	$(CC) $(PACKAGES) oed.byte

clean:
	$(CC) -clean
	@rm -rf test/_build/

mli:
	ocamlc -I _build/ *.mli
	@mv *.cmi _build/

tests:
	$(CC) -Is .,test -package ounit $(PACKAGES) edCommand_test.byte

.PHONY: test

TEST_ARGS=inline-test-runner ocaml-ed

test: tests
	@echo Testing edCommand...
	@./edCommand_test.byte $(TEST_ARGS)
