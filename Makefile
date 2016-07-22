#
# Makefile for oed
#

all: oed

oed:
	ocamlcore oed.byte

clean:
	ocamlcore -clean
