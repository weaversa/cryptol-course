#!/bin/bash

usage() {
    cat << EOM
$0 - a janky script to run .icry tests and compare output against .icry.stdout files

Usage: $0 COMMAND_LINE_OPTIONS FILES/DIRS
Command line options:
-v         --verbose          echo status messages
-h or -?   --help             display this message and exit

Parameters:
FILES/DIRS                    test fileglobs to run
EOM
}

function log () {
    if [[ $VERBOSE -eq true ]]; then
        echo "$@"
    fi
}

run_tests() {
    if [[ $TRAVIS_JOB_NAME = "Cryptol Docker on Linux Host - Test Snippet Extraction" ]]; then
        CRYPTOL_ALIAS="docker run -v $(pwd):/mnt/cryptol-course --env CRYPTOLPATH=/mnt/cryptol-course galoisinc/cryptol:2.9.1" \
        CRYPTOL_MNT="/mnt/cryptol-course/"
    else
        CRYPTOL_ALIAS="cryptol"
        CRYPTOL_MNT=""
    fi

    # Process command line arguments
    while [ "$1" != "" ]; do
        case $1 in
        (*=*)
            val=${1#*=}
            key=${1%"=$val"}
            if [ $key != --runner ]; then
                TEST_RUNNER_ARGS+=($1)
            fi
            ;;
        (*)
            key=$1
            ;;
        esac

        case $key in
            # echo status messages
            # -v
            # --verbose
            -v | --verbose )
                val=true
                VERBOSE=$val
                ;;

            # display this message
            # -h
            # --help
            -h | -? | --help )
                usage
                exit
                ;;

            * )
                icry_list+=("$1")
        esac

        shift

        key=
        val=
    done

    # Run tests for each .icry file against the corresponding .icry.out, 
    # log batch output to .icry.actual, and log difference to .diff;
    # exit if a keyword indicating a major inconcistency is found.
    ls ${icry_list[@]} | while read -r ICRY ; do
        TMP="$ICRY.tmp"
        STDOUT="$ICRY.stdout"
        ACTUAL="$ICRY.actual"
        DELTA="$ICRY.diff"

        log "$0: Running test for file $ICRY ..."

        log "$0:   Removing Windows carriage returns in $ICRY and $STDOUT..."
        dos2unix -q -n "$ICRY" "$TMP" && mv "$TMP" "$ICRY"
        dos2unix -q -n "$STDOUT" "$TMP" && mv "$TMP" "$STDOUT"

        log "$0:   Logging \"$CRYPTOL_ALIAS\" in batch mode running $ICRY to $ACTUAL ..."
        $CRYPTOL_ALIAS -b $CRYPTOL_MNT$ICRY > $ACTUAL
        log "$0:   Removing Windows carriage returns in $ACTUAL..."
        dos2unix -q -n $ACTUAL $TMP && mv $TMP $ACTUAL

        log "$0:   Logging difference between $STDOUT and $ACTUAL to $DELTA ..."
        diff --minimal $STDOUT $ACTUAL > $DELTA

        if grep -qE "^(Loading .*)|(Q\.E\.D\.)|(Counterexample)|(Satisfiable)|(Unsatisfiable)$" $DELTA; then
            log "$(< $DELTA)"
            log "Found major inconsistency in diff; exiting..."
            exit 1
        fi

        # rm $ICRY $STDOUT $ACTUAL $DELTA
    done
}

shopt -s globstar
run_tests $@
