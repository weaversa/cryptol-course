#!/bin/bash

# poor_cryptol_test_runner - a janky script to generate, run, and analyze interactive Cryptol

function extract_test_diff {
    for md in $( ls $1/*.md )
    do
        basename=`basename $md .md`

        echo "Processing Markdown file $md ..."

        icry=$1/$basename.icry
        expected=$1/$basename.expected
        actual=$1/$basename.actual
        delta=$1/$basename.diff

        echo "  Extracting \`\`\`icry fences from $md; exporting commands to $icry and output to $expected ..."
        sed -n '/^```icry/,/^```/ p' < $md | sed '/^```/ d' | grep -E "^[A-Za-z0-9:_']+?>" | sed -r "s/^[A-Za-z0-9:_']+?> ?(.*)$/\1/" > $icry
        sed -n '/^```icry/,/^```/ p' < $md | sed '/^```/ d' | sed -r "s/^[A-Za-z0-9:_']+?> ?(.*)$//" | sed "/^$/d" > $expected

        echo "  Logging interactive Cryptol running $icry to $actual ..."
        cryptol -b $icry > $actual

        echo "  Logging difference between $expected and $actual to $delta ..."
        diff $expected $actual > $delta
    done
}

extract_test_diff $1
