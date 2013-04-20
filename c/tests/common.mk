#meant to be included from subdirectories

INCLUDES=-I../../ -I../../liblfds6.1.1/liblfds611/inc 
CFLAGS=-O3 -Wall -std=gnu99 -L../.. -lrt -lqs -llfds -pthread -g $(INCLUDES) -fPIC

include ../../common.mk

$(trg): $(trg).o ../../libqs.so ../../liblfds.so
	$(CC) $(trg).o -o $(trg) $(CFLAGS) -L../.. -Wl,-rpath=../..
