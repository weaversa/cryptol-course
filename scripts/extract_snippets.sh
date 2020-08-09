#!/bin/bash

usage() {
    cat << EOM
$0 - a janky script to extract interactive Cryptol snippets and expected output from Markdown

Parameters:
FILES/DIRS                    Markdown fileglobs from which to extract snippets
EOM
}

extract_snippets() {
    egrep -lI '^```Xcryptol session' "$@" | while read -r FILE ; do
        ICRY="$FILE.icry"
        EXPECTED="$FILE.icry.stdout"

        echo "$0: Processing file $FILE ..."
        echo "$0:   Extracting \`\`\`Xcryptol session fences from $FILE; exporting commands to $ICRY and output to $EXPECTED ..."
        sed -n '/^```Xcryptol session/,/^```/ p' < "$FILE" | sed '/^```/ d' | grep -E "^[A-Za-z0-9:_']+?>" | sed -r "s/^[A-Za-z0-9:_']+?> ?(.*)$/\1/" > $ICRY
        sed -n '/^```Xcryptol session/,/^```/ p' < "$FILE" | sed '/^```/ d' | sed -r "s/^[A-Za-z0-9:_']+?> ?(.*)$//" | sed "/^$/d" | sed -rn '/^(Loading module )|(Counterexample)|(Q.E.D.)|(Satisfiable)|(Unsatisfiable)/ p' > $EXPECTED
    done
}

shopt -s globstar
extract_snippets $@
