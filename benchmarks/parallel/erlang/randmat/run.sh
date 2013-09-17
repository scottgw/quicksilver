#!/bin/bash

erl -noshell -gather -nelts $1 -seed $2 -s main main -s init stop
