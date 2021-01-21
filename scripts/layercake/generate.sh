#!/bin/bash
shopt -s extglob
cd ${TRAVIS_BUILD_DIR}/scripts/layercake
poetry run poetry install
poetry run python3 layercake/gen_md.py
poetry run python3 layercake/gen_svgs.py
cp -r ${TRAVIS_BUILD_DIR}/docs/ ${TRAVIS_BUILD_DIR}/gh-pages/
sed -i "s/\.md/\.html/" ${TRAVIS_BUILD_DIR}/gh-pages/misc/*.@(gv|svg)
