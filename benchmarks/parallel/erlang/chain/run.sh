#!/bin/bash

erl -noshell -nelts $1 -seed $2 -percent $3 -winnow_nelts $4 \
   -s main main -s init stop +S $5
