#!/bin/bash
# rm -rf ${GITHUB_WORKSPACE}/docs/ ${GITHUB_WORKSPACE}/gh-pages/
cd ${GITHUB_WORKSPACE}/scripts/layercake
poetry run poetry install
poetry run python3 layercake/gen_md.py
poetry run python3 layercake/gen_svgs.py
