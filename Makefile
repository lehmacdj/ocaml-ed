#
# Makefile for oed
#

all: oed

oed:
	corebuild oed.byte

clean:
	corebuild -clean
