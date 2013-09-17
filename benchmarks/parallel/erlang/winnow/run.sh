#!/bin/bash

erl -noshell -gather -orignelts $1 -nelts $2 -s main main -s init stop
