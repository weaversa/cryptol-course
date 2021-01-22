#!/bin/bash
echo "Generating...?"
# rm -rf ${GITHUB_WORKSPACE}/docs/ ${GITHUB_WORKSPACE}/gh-pages/
cd ${GITHUB_WORKSPACE}/scripts/layercake
poetry run poetry install
poetry run python3 layercake/gen_md.py
poetry run python3 layercake/gen_svgs.py
cp -r ${GITHUB_WORKSPACE}/docs/ ${GITHUB_WORKSPACE}/gh-pages/
shopt -s extglob
sed -i "s/\.md/\.html/" ${GITHUB_WORKSPACE}/gh-pages/misc/*.@(gv|svg)
# [ -d ${GITHUB_WORKSPACE}/docs/ ] && echo "adding docs/" && git add ${GITHUB_WORKSPACE}/docs/
# [ -d ${GITHUB_WORKSPACE}/gh-pages/ ] && echo "adding gh-pages/" && git add ${GITHUB_WORKSPACE}/gh-pages/
