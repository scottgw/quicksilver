#meant to be included from subdirectories

CFLAGS=-O3 -Wall -std=gnu99 `pkg-config libqs --cflags --libs` -g

include ../../common.mk

$(trg): $(trg).o
	$(CC) $(trg).o -o $(trg) $(CFLAGS) -L../.. -Wl,-rpath=../..
