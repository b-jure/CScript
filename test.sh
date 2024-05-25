#!/bin/bash

clear
make
for testfile in test/*.sk
do
    if ! ./cript "$testfile" > /dev/null 2>&1; then
        printf "\nTEST -> %s x FAILED" "$testfile"
    else
        printf "\nTEST -> %s + PASSED" "$testfile"
    fi
done
