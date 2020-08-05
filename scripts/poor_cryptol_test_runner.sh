#!/bin/bash

# poor_cryptol_test_runner - a janky script to generate, run, and analyze interactive Cryptol

# Requires
#   * `dos2unix` (to remove newlines)
#   * `CRYPTOL_HOME` environment variable set to base directory of Cryptol repo (to run `$CRYPTOL`/cry test` on extracted `.icry` and `.icry.stdout` files)

function extract_test_diff {
    for md in $( ls $1/*.md )
    do
        dirbasename=$1/`basename $md .md`
        tmp=$dirbasename.tmp

        echo "$0: Processing Markdown file $md ..."

        echo "$0:   Replacing Windows newlines in $md with Linux newlines..."
        dos2unix -n $md $tmp && mv $tmp $md

        icry=$dirbasename.icry
        expected=$dirbasename.icry.stdout
        # actual=$dirbasename.actual
        # delta=$dirbasename.diff

        if grep -q '^```icry' $md; then
            echo "$0:   Extracting \`\`\`icry fences from $md; exporting commands to $icry and output to $expected ..."
            sed -n '/^```icry/,/^```/ p' < $md | sed '/^```/ d' | grep -E "^[A-Za-z0-9:_']+?>" | sed -r "s/^[A-Za-z0-9:_']+?> ?(.*)$/\1/" > $icry
            sed -n '/^```icry/,/^```/ p' < $md | sed '/^```/ d' | sed -r "s/^[A-Za-z0-9:_']+?> ?(.*)$//" | sed "/^$/d" | sed -rn '/^(Loading module )|(Counterexample)|(Q.E.D.)|(Satisfiable)|(Unsatisfiable)/ p' > $expected

            echo "$0:   Replacing Windows newlines in $expected with Linux newlines..."
            dos2unix -n $expected $tmp && mv $tmp $expected

            # echo "$0:   Logging interactive Cryptol running $icry to $actual ..."
            # cryptol -b $icry > $actual
            # dos2unix -n $actual $tmp && mv $tmp $actual
            
            # echo "$0:   Logging difference between $expected and $actual to $delta ..."
            # diff $expected $actual > $delta

            # if grep -qE "Counterexample|Satisfiable|Q.E.D.|Unsatisfiable" $delta; then
            #     echo "Found Q.E.D. or Unsatisfiable; exiting..."
            #     exit 1
            # fi

            # rm $icry $expected $actual $delta
        else
            echo "$0:   $md did not have any \`\`\`icry fences; skipping..."
        fi

        echo "Running Cryptol test runner on all .icry files in $1..."
        $CRYPTOL_HOME/cry test -c `which cryptol` -r . $1/*.icry
    done
}

extract_test_diff $1
