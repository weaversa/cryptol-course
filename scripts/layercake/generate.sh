#!/bin/bash
cd ${GITHUB_WORKSPACE}/scripts/layercake
poetry install
poetry run python3 layercake/gen_md.py
poetry run python3 layercake/gen_svgs.py
