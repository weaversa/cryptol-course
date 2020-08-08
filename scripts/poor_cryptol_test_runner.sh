#!/bin/bash -x

# Requires (in addition to other requirements of `weaversa/cryptol-course` GitHub repo)
#   * `dos2unix` (to remove newlines)
#   * a Cryptol `test-runner` binary (to run on extracted `.icry` and `.icry.stdout` files)

usage() {
  cat << EOM
$0 - a janky script to extract interactive Cryptol snippets and expected output from Markdown
and pass them to Cryptol's `test-runner`

Usage: $0 COMMAND_LINE_OPTIONS FILES/DIRS
Command line options:
-x PATH    --runner=PATH      path to Cryptol `test-runner` or equivalent
-c PATH    --exe=PATH         path to Cryptol binary
-F STRING  --flag=STRING      add a flag to the test binary
-r PATH    --result-dir=PATH  the result directory for test runs
-p PROG    --diff-prog=PROG   use this diffing program on failures
-T STRING                     add an argument to pass to the test-runner main
-i         --ignore-expected  ignore expected failures
           --ext=STRING       files with this extension are tests
-h or -?   --help             display this message and exit

Parameters:
FILES/DIRS                    Markdown files/directories from which to extract tests
EOM
}

function extract_test_diff {
    # flags to pass to test runner
    TEST_FLAGS=()
    # arguments to pass to test runner
    TEST_RUNNER_ARGS=()
    # files/directories to check for Markdown files
    md_list=()

    # Process argument/environment variable for location of Cryptol `test-runner` in addition to its command line arguments
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
            # poor_cryptol_test_runner.sh CLI

            # path to Cryptol `test-runner` or equivalent
            # -x PATH
            -x )
                shift
                val=$1
                TEST_RUNNER_ARGS+=($key $val)
                ;&
            # --runner=PATH
            --runner )
                CRYPTOL_TEST_RUNNER=$val
                ;;


            # Cryptol `test-runner` CLI

            # path to Cryptol binary
            # -c PATH
            -c )
                shift
                val=$1
                TEST_RUNNER_ARGS+=($key $val)
                ;&
            # --exe=PATH
            --exe )
                CRYPTOL_BINARY=$val
                ;;

            # add a flag to the test binary
            # -F STRING
            -F )
                shift
                val=$1
                TEST_RUNNER_ARGS+=($key $val)
                ;&
            # --flag=STRING
            --flag )
                TEST_FLAGS+=(-F $val)
                ;;

            # the result directory for test runs
            # -r PATH
            -r )
                shift
                val=$1
                TEST_RUNNER_ARGS+=($key $val)
                ;&
            # --result-dir=PATH
            --result-dir )
                TEST_RESULT_DIR=$val
                ;;

            # use this diffing program on failures
            # -p PROG
            -p )
                shift
                val=$1
                TEST_RUNNER_ARGS+=($key $val)
                ;&
            # --diff-prog=PROG
            --diff-prog )
                DIFF_PROG=$val
                ;;

            # add an argument to pass to the test-runner main
            # -T STRING
            -T ) 
                shift
                val=$1
                MAIN_ARG=$val
                TEST_RUNNER_ARGS+=($key $val)
                ;;

            # ignore expected failures
            # -i
            # --ignore-expected
            -i | --ignore-expected )
                val=true
                IGNORE_EXPECTED=$val
                TEST_RUNNER_ARGS+=($key)
                ;;

            # files with this extension are tests
            # --ext=STRING
            --ext )
                TEST_RESULT_EXT=$val
                ;;

            # display this message
            # -h
            # --help
            -h | -? | --help )
                usage
                exit
                ;;

            * )
                md_list+=("$1")
        esac

        shift

        key=
        val=
    done

    CRYPTOL_TEST_RUNNER=${CRYPTOL_TEST_RUNNER:-/cryptol/bin/test-runner}
    CRYPTOL_BINARY=${CRYPTOL_BINARY:-`which cryptol`}
    TEST_RESULT_DIR=${TEST_RESULT_DIR:-$PWD}
    TEST_FLAGS=${TEST_FLAGS[@]:--F v2-run -F -v0 -F exe:cryptol -F -- -F -b}
    TEST_RESULT_EXT="${TEST_RESULT_EXT:-icry}"

    echo "TEST_FLAGS: ${TEST_FLAGS[*]}"
    echo "TEST_RUNNER_ARGS: ${TEST_RUNNER_ARGS[*]}"
    echo "FILES/DIRS: ${md_list[*]}"

    # If no files/dirs are provided, default to the added/modified files in `git status -s`
    if [ "${#md_list[@]}" -eq 0 ]; then
        SAVEIFS=$IFS   # Save current IFS
        IFS=$'\n'      # Change IFS to new line
        md_list=($(git status -s | grep -E '^[AM]\s+"?.*\.md"?$' | sed -nr 's/^[AM]\s+"?(.*\.md)"?$/\1/ p'))
        IFS=$SAVEIFS   # Restore IFS
    fi

    icry_list=()

    for md in "${md_list[@]}"
    do
        base_dir=`dirname "$md"`
        tmp="${md%.md}.tmp"
        icry="${md%.md}.icry"
        expected="${md%.md}.icry.stdout"
        actual=$dirbasename.actual
        delta=$dirbasename.diff

        echo "$0: Processing Markdown file $md ..."

        echo "$0:   Replacing Windows newlines in $md with Linux newlines..."
        dos2unix -n "$md" "$tmp" && mv "$tmp" "$md"

        if grep -q '^```Xcryptol session' "$md"; then
            echo "$0:   Extracting \`\`\`Xcryptol session fences from $md; exporting commands to $icry and output to $expected ..."
            sed -n '/^```Xcryptol session/,/^```/ p' < "$md" | sed '/^```/ d' | grep -E "^[A-Za-z0-9:_']+?>" | sed -r "s/^[A-Za-z0-9:_']+?> ?(.*)$/\1/" > $icry
            sed -n '/^```Xcryptol session/,/^```/ p' < "$md" | sed '/^```/ d' | sed -r "s/^[A-Za-z0-9:_']+?> ?(.*)$//" | sed "/^$/d" | sed -rn '/^(Loading module )|(Counterexample)|(Q.E.D.)|(Satisfiable)|(Unsatisfiable)/ p' > $expected

            echo "$0:   Replacing Windows newlines in $expected with Linux newlines..."
            dos2unix -n "$icry" "$tmp" && mv "$tmp" "$icry"

            echo "$0:   Replacing Windows newlines in $expected with Linux newlines..."
            dos2unix -n "$expected" "$tmp" && mv "$tmp" "$expected"

            echo "$0:   Adding $icry to list of .icry files to test..."
            icry_list+=("$icry")
            echo "$0:   List of .icry files is now: ${icry_list[@]}"

            echo "$0:   Logging interactive Cryptol running $icry to $actual ..."
            cryptol -b $icry > $actual
            dos2unix -n $actual $tmp && mv $tmp $actual
            
            echo "$0:   Logging difference between $expected and $actual to $delta ..."
            diff $expected $actual > $delta

            if grep -qE "Counterexample|Satisfiable|Q.E.D.|Unsatisfiable" $delta; then
                echo "Found Q.E.D. or Unsatisfiable; exiting..."
                exit 1
            fi

            # rm $icry $expected $actual $delta
        else
            echo "$0:   $md did not have any \`\`\`Xcryptol session fences; skipping..."
        fi
    done

    # echo "Running Cryptol test runner on all .icry files..."
    # $CRYPTOL_TEST_RUNNER \
    #     -c $CRYPTOL_BINARY \
    #     -r $TEST_RESULT_DIR \
    #     --ext=$TEST_RESULT_EXT \
    #     $TEST_FLAGS \
    #     "${icry_list[@]}"
}

extract_test_diff $*
