#!/bin/bash

erl -smp enable -noshell +t 8192 +P $@ +ec +K true +hms 4 +hmbs 1 -s main main $@ -s init stop
