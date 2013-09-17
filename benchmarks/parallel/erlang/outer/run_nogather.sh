#!/bin/bash

erl -noshell -nelts $1 -s main main -s init stop
