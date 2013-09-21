#!/bin/bash

GOMAXPROCS=$5 ./main --is_bench ${@:1:4}
