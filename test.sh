#!/bin/sh

BIN=cscript
TESTDIR=test
TESTS=($(ls $TESTDIR))
NTESTS=${#TESTS[@]}

echo "Running $NTESTS tests:"

failed=0
for t in "${TESTS[@]}"; do
    ./$BIN $TESTDIR/$t &> /dev/null
    if (( $? == 0 )); then
        if [ -t 1 ]; then printf "\x1B[32m%-40s passed" "$t...";
        else printf "%-40s passed" "$t..."; fi
    else
        ((failed++))
        if [ -t 1 ]; then printf "\x1B[31m%-40s failed" "$t...";
        else printf "%-40s failed" "$t..."; fi
    fi
    if [ -t 1 ]; then printf "\x1B[0m\n";
    else printf "\n"; fi
done

if [ -t 1 ]; then
    printf "Total \x1B[32mpassed\x1B[0m tests: %d\n" $((NTESTS - failed));
    printf "Total \x1B[31mfailed\x1B[0m tests: %d\n" $failed;
else
    printf "Total passed tests: %d\n" $((NTESTS - failed))
    printf "Total failed tests: %d\n" $failed
fi

