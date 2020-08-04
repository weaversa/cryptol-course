#!/bin/bash

# poor_cryptol_test_runner - a janky script to generate, run, and analyze interactive Cryptol

function extract_test_diff {
    for md in $( ls $1/*.md )
    do
        dirbasename=$1/`basename $md .md`
        tmp=$dirbasename.tmp

        echo "$0: Processing Markdown file $md ..."

        echo "$0:   Replacing Windows newlines in $md with Linux newlines..."
        dos2unix -n $md $tmp
        mv $tmp $md

        icry=$dirbasename.icry
        expected=$dirbasename.expected
        actual=$dirbasename.actual
        delta=$dirbasename.diff

        if grep -q '^```icry' $md; then
            echo "$0:   Extracting \`\`\`icry fences from $md; exporting commands to $icry and output to $expected ..."
            sed -n '/^```icry/,/^```/ p' < $md | sed '/^```/ d' | grep -E "^[A-Za-z0-9:_']+?>" | sed -r "s/^[A-Za-z0-9:_']+?> ?(.*)$/\1/" > $icry
            sed -n '/^```icry/,/^```/ p' < $md | sed '/^```/ d' | sed -r "s/^[A-Za-z0-9:_']+?> ?(.*)$//" | sed "/^$/d" > $expected

            echo "$0:   Logging interactive Cryptol running $icry to $actual ..."
            cryptol -b $icry > $actual

            echo "$0:   Replacing Windows newlines in $expected and $actual with Linux newlines..."
            dos2unix -n $expected $tmp && mv $tmp $expected
            dos2unix -n $actual $tmp && mv $tmp $actual

            echo "$0:   Logging difference between $expected and $actual to $delta ..."
            diff $expected $actual > $delta

            if grep -qE "Q.E.D.|Unsatisfiable" $delta; then
                echo "Found Q.E.D. or Unsatisfiable; exiting..."
                exit 1
            fi
        else
            echo "$0:   $md did not have any \`\`\`icry fences; skipping..."
        fi

        # rm $icry $expected $actual $delta
    done
}

extract_test_diff $1
