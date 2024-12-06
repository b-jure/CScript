BIN=cscript
TESTDIR=test
TESTS=($(ls $TESTDIR))
NTESTS=${#TESTS[@]}

echo "Running $NTESTS tests:"

failed=0
for t in "${TESTS[@]}"; do
    printf "$t...\t";
    ./$BIN $TESTDIR/$t &> /dev/null
    if (( $? == 0 )); then
        printf "\x1B[32mpassed";
    else
        ((failed++))
        printf "\x1B[31mfailed";
    fi
    printf "\x1B[0m\n";
done

printf "Total \x1B[32mpassed\x1B[0m tests: %d\n" $((NTESTS - failed));
printf "Total \x1B[31mfailed\x1B[0m tests: %d\n" $failed;
