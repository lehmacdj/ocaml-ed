#
# Makefile for oed
#

CC=ocamlbuild
FLAGS+=-use-ocamlfind
FLAGS+=-tag thread -tag debug -tag bin_annot -tag short_paths
FLAGS+=-cflags "-w A-33-40-41-42-43-34-44-45"
FLAGS+=-cflags -strict-sequence

PACKAGES=-package core -package re2
FLAGS+=$(PACKAGES)

# Verbatim corebuild script
# ocamlbuild \
#     -use-ocamlfind \
#     -pkg core \
#     -tag "ppx(ppx-jane -as-ppx)" \
#     -tag thread \
#     -tag debug \
#     -tag bin_annot \
#     -tag short_paths \
#     -cflags "-w A-4-33-40-41-42-43-34-44" \
#     -cflags -strict-sequence \
#     $@

all: oed

oed:
	$(CC) $(FLAGS) oed.byte

clean:
	$(CC) -clean >/dev/null
	@rm -rf test/_build/

INTERFACES := $(patsubst %.mli, %.cmi, $(wildcard *.mli))

mli:
	corebuild $(INTERFACES)

tests:
	$(CC) $(FLAGS) -Is .,test -package ounit edCommand_test.byte >/dev/null 2>&1

.PHONY: test

test: tests
	@echo Testing edCommand...
	@./edCommand_test.byte
