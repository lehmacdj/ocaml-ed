#
# Makefile for oed
#

CC=corebuild
CFLAGS=-package re2 -package async

all: oed

oed:
	$(CC) $(CFLAGS) oed.byte

clean:
	$(CC) -clean

mli:
	ocamlc -I _build/ *.mli
	mv *.cmi _build/
