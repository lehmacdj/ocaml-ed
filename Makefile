#
# Makefile for oed
#

CC=corebuild
PACKAGES=-package re2

all: oed

oed:
	$(CC) $(PACKAGES) oed.byte

clean:
	$(CC) -clean >/dev/null
	@rm -rf test/_build/

INTERFACES=edCommand.cmi editor.cmi fileBuffer.cmi main.cmi types.cmi

mli:
	corebuild $(INTERFACES)

tests:
	$(CC) -Is .,test -package ounit $(PACKAGES) edCommand_test.byte >/dev/null 2>&1

.PHONY: test

test: tests
	@echo Testing edCommand...
	@./edCommand_test.byte
