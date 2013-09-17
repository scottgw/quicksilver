#!/bin/bash

erl -noshell -gather -nelts $1 -s main main -s init stop
