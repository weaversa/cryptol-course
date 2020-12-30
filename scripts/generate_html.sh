rm -rf html

FILESMD=`find . -name "*.md"`

for NAMEMD in $FILESMD; do
    NAME=`cut -d . -f 2 <<< "$NAMEMD"`; # remove extension
    NAMEHTML=$PWD"/html"$NAME".html";
    DIRNAME=`dirname "$NAMEHTML"`;
    mkdir -p "$DIRNAME";
    gh-md-to-html $NAMEMD -n $NAMEHTML -c "html/css" -m True -i "html/images"
    CONTENT=`cat $NAMEHTML`
    echo $CONTENT | sed 's/.md\">/.html\">/g' > $NAMEHTML # fix links, change .md to .html
done;
