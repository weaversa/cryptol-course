#!/bin/bash

usage() {
    cat << EOM
$0 - a janky script to extract interactive Cryptol snippets and expected output from Markdown

Parameters:
FILES/DIRS                    Markdown files/directories from which to extract snippets
EOM
}

extract_snippets() {
    egrep -lI '^```Xcryptol session' "$@" | while read -r FILE ; do
        tmp="$FILE.tmp"
        icry="$FILE.icry"
        expected="$FILE.icry.stdout"

        echo "$0: Processing file $FILE ..."
        echo "$0:   Extracting \`\`\`Xcryptol session fences from $FILE; exporting commands to $icry and output to $expected ..."
        sed -n '/^```Xcryptol session/,/^```/ p' < "$FILE" | sed '/^```/ d' | grep -E "^[A-Za-z0-9:_']+?>" | sed -r "s/^[A-Za-z0-9:_']+?> ?(.*)$/\1/" > $icry
        sed -n '/^```Xcryptol session/,/^```/ p' < "$FILE" | sed '/^```/ d' | sed -r "s/^[A-Za-z0-9:_']+?> ?(.*)$//" | sed "/^$/d" | sed -rn '/^(Loading module )|(Counterexample)|(Q.E.D.)|(Satisfiable)|(Unsatisfiable)/ p' > $expected
    done
}

shopt -s globstar
extract_snippets $@
