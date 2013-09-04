#!/bin/bash

erl -noshell -s main main $@ -s init stop
