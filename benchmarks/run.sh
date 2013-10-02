#!/bin/bash

# exit on the first error
set -e

OPTIND=1
QOQ=YES
LIFT=YES
MAIN=main

while getopts "qlo:" opt; do
    case "$opt" in
        q)
            QOQ=NO
            MAIN="${MAIN}_noqoq"
            ;;
        l)
            LIFT=NO
            MAIN="${MAIN}_nolift"
            ;;
    esac
done

shift $((OPTIND-1))

LIBQS_EXECS=${@: -1} ./${MAIN}
