#!/usr/bin/env bash

filename="${@%.*}"
# echo "Compiling and Running $filename  ($PWD)"
stack run -- -c "$@" ; gcc runtime.c -lgc $filename.c
./a.out
rm a.out
# echo "Cleaning $filename"
# ./a.out > ${@}.out 2> ${@}.err
