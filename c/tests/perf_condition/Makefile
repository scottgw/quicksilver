#trg := $(notdir $(CURDIR))

#include ../common.mk

LOCAL=$(HOME)/local

perf_condition: perf_condition.c
	gcc -std=gnu99 -O3 $< -o $@ `pkg-config libqs --cflags --libs`
