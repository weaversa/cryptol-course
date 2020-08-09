#!/bin/bash

usage() {
    cat << EOM
$0 - a janky script to run `.icry` tests and compare output against `.icry.stdout` files

Parameters:
FILES/DIRS                    test fileglobs to run
EOM
}


run_tests() {
    ls $@ | while read -r ICRY ; do
        TMP="$ICRY.tmp"
        STDOUT="$ICRY.stdout"
        ACTUAL="$ICRY.actual"
        DELTA="$ICRY.diff"

        echo "$0: Running test for file $ICRY ..."

        echo "$0:   Removing Windows carriage returns in $ICRY and $STDOUT..."
        dos2unix -n "$ICRY" "$TMP" && mv "$TMP" "$ICRY"
        dos2unix -n "$STDOUT" "$TMP" && mv "$TMP" "$STDOUT"

        echo "$0:   Logging interactive Cryptol running $ICRY to $ACTUAL ..."
        cryptol -b $ICRY > $ACTUAL
        echo "$0:   Removing Windows carriage returns in $ACTUAL..."
        dos2unix -n $ACTUAL $TMP && mv $TMP $ACTUAL

        echo "$0:   Logging difference between $STDOUT and $ACTUAL to $DELTA ..."
        diff --minimal $STDOUT $ACTUAL > $DELTA

        if grep -qE "Counterexample|Satisfiable|Q.E.D.|Unsatisfiable" $DELTA; then
            echo "Found Q.E.D. or Unsatisfiable; exiting..."
            exit 1
        fi

        # rm $ICRY $STDOUT $ACTUAL $DELTA
    done
}

shopt -s globstar
run_tests $@
