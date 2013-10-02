#!/bin/bash

# exit on the first error
set -e

OPTIND=1
MAIN=main

while getopts "ql" opt; do
    case "$opt" in
        q)
            MAIN="${MAIN}_noqoq"
            ;;
        l)
            MAIN="${MAIN}_nolift"
            ;;
    esac
done

shift $((OPTIND-1))

LIBQS_EXECS=${@: -1} ./${MAIN}
