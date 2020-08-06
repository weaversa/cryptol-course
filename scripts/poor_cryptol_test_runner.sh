#!/bin/bash -x

# poor_cryptol_test_runner - a janky script to generate, run, and analyze interactive Cryptol

# Requires
#   * `dos2unix` (to remove newlines)
#   * `CRYPTOL_HOME` environment variable set to base directory of Cryptol repo (to run `$CRYPTOL`/cry test` on extracted `.icry` and `.icry.stdout` files)

function extract_test_diff {
    SAVEIFS=$IFS   # Save current IFS
    IFS=$'\n'      # Change IFS to new line
    md_list=($(git status -s | grep -E '^[AM]\s+"?.*\.md"?$' | sed -nr 's/^[AM]\s+"?(.*\.md)"?$/\1/ p'))
    IFS=$SAVEIFS   # Restore IFS
    icry_list=()

    for md in "${md_list[@]}"
    do
        echo "Trying and failing miserably to process Markdown file $md ..."

        base_dir=`dirname "$md"`
        tmp="${md%.md}.tmp"
        icry="${md%.md}.icry"
        expected="${md%.md}.icry.stdout"
        # actual=$dirbasename.actual
        # delta=$dirbasename.diff

        echo "$0: Processing Markdown file $md ..."

        echo "$0:   Replacing Windows newlines in $md with Linux newlines..."
        dos2unix -n "$md" "$tmp" && mv "$tmp" "$md"

        if grep -q '^```icry' "$md"; then
            echo "$0:   Extracting \`\`\`icry fences from $md; exporting commands to $icry and output to $expected ..."
            sed -n '/^```icry/,/^```/ p' < "$md" | sed '/^```/ d' | grep -E "^[A-Za-z0-9:_']+?>" | sed -r "s/^[A-Za-z0-9:_']+?> ?(.*)$/\1/" > $icry
            sed -n '/^```icry/,/^```/ p' < "$md" | sed '/^```/ d' | sed -r "s/^[A-Za-z0-9:_']+?> ?(.*)$//" | sed "/^$/d" | sed -rn '/^(Loading module )|(Counterexample)|(Q.E.D.)|(Satisfiable)|(Unsatisfiable)/ p' > $expected

            echo "$0:   Replacing Windows newlines in $expected with Linux newlines..."
            dos2unix -n "$expected" "$tmp" && mv "$tmp" "$expected"

            echo "$0:   Adding $icry to list of .icry files to test..."
            icry_list+=("$icry")
            echo "$0:   List of .icry files is now: ${icry_list[@]}"

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
    done

    echo "Running Cryptol test runner on all .icry files..."
    ./bin/test-runner -c `which cryptol` -r ../cryptol-course --ext=icry --exe=cabal -F v2-run -F -v0 -F exe:cryptol -F -- -F -b "${icry_list[@]}"
}

extract_test_diff

# ls $1/*.md
# git status -s | grep -E '^[AM]\s+"?.*\.md"?$' | sed -nr 's/^[AM]\s+("?.*\.md"?)$/\1/ p' | tr '\n' ' '