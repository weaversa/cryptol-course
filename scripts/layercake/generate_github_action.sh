#!/bin/bash
shopt -s extglob
cd ${GITHUB_WORKSPACE}/scripts/layercake
poetry run poetry install
poetry run python3 layercake/gen_md.py
poetry run python3 layercake/gen_svgs.py
cp -r ${GITHUB_WORKSPACE}/docs/ ${GITHUB_WORKSPACE}/gh-pages/
sed -i "s/\.md/\.html/" ${GITHUB_WORKSPACE}/gh-pages/misc/*.@(gv|svg)
