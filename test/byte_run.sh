#!/usr/bin/env bash

filename="${@%.*}"
stack run -- -m "$@" ; 
./bvm/bvm "$filename.byte"
