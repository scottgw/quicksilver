#!/bin/bash

GOMAXPROCS=$1 ./main --is_bench ${@:2}
