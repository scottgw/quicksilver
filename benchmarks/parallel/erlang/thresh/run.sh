#!/bin/bash

erl -noshell -gather -nelts $1 -percent $2 -s main main -s init stop
