#!/bin/bash

erl -noshell -nelts $1 -seed $2 -s main main -s init stop +S $3
