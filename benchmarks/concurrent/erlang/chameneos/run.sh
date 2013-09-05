#!/bin/bash

erl -smp enable -noshell -run main main $@
