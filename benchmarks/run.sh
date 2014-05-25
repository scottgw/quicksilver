#!/bin/bash

# exit on the first error
set -e

OPTIND=1
MAIN="./main"

while getopts "qdsn" opt; do
    case "$opt" in
        q)
	    export QS_SYNC_CHECK_DISABLE=1
            MAIN="./${MAIN}_justqoq"
            ;;
        d)
            MAIN="./${MAIN}_none"
            ;;
        s)
	    export QS_SYNC_CHECK_DISABLE=1
            MAIN="./${MAIN}_juststat"
            ;;
        n)
	    export QS_SYNC_CHECK_DISABLE=1
            MAIN="./${MAIN}_none"
            ;;
    esac
done

shift $((OPTIND-1))

LIBQS_EXECS=${@: -1} ${MAIN}
