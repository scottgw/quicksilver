#!/bin/bash

erl -noshell -orignelts $1 -nelts $2 -s main main -s init stop +S $3
