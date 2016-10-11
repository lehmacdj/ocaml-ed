#
# Makefile for oed
#

CC=corebuild

all: oed tests

oed:
	$(CC) -package re2 -package async -oed.byte

clean:
	$(CC) -clean

mli:
	ocamlc -I _build/ *.mli
	mv *.cmi _build/
