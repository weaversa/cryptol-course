![Build/Deploy](https://github.com/weaversa/cryptol-course/workflows/Build%20and%20Deploy/badge.svg)
![Broken Links](https://github.com/weaversa/cryptol-course/workflows/Check%20for%20Broken%20Links/badge.svg)

# Cryptol Course Development

This is the development branch of an online course on the
[Cryptol](https://github.com/GaloisInc/Cryptol)
language and interpreter for cryptographic algorithm specifications.
The course itself is rendered from `/templates` and deployed to the
[`docs`](https://github.com/weaversa/cryptol-course/tree/docs)
branch (for downloading course materials), and the
[`gh-pages`](https://github.com/weaversa/cryptol-course/tree/gh-pages)
branch for the accompanying
[GitHub Pages site](https://weaversa.github.io/cryptol-course).

## Generation/Testing/Deployment Workflow

This Cryptol course comprises a series of "labs" (each with one or
more "modules"), where labs depend on completion of prior labs and
introduce new Cryptol concepts. These are documented under
`/templates`, which includes Markdown files with template fields
(specific to this project) and non-Markdown files retained as-is.
Markdown files have
[Jinja](https://jinja.palletsprojects.com/en/2.11.x/) tags that are
processed by the Python script `gen_md.py`, which processes the
following template fields, and writes the rendered Markdown and
copies non-Markdown files into `/docs`:

  * `{{ graphical-view }}`
    Includes and links to a Scalable Vector Graphics (SVG) file
    that diagrams and links to the recommended (path) and optional
    (branches) modules of a lab (or the entire course)
  * `{{ navlinks }}`
    Represents a table with navigational links:
    * (`^`) "up" to an overview of modules in a lab, which
      recommends an order in which to visit its modules and shows any
      optional modules branching from a recommended one
      (the entire course is a top-level lab)
    * (`v`) "down" to the first recommended module in a lab
    * (`<`) "back" to the prior recommended module in a lab
    * (`>`) "forward" to the next recommended module in a lab
    * (`+`) "branch" into an optional module
    * (`-`) "return" to the recommended module from which this one
      branches
  * `{{ solicitation }}`
    boilerplate indicating where to post issues with the course

Another script (`gen_svgs.py`) generates the SVGs with graphical
views of each multi-module lab (including the entire course) into
`/docs/misc`.

These scripts import metadata from `deps.yml`.

The scripts are run in [GitHub Actions](https://github.com/actions)
workflows, which also test the generated `/docs` on various platforms
(Docker, Ubuntu, etc.), copy `/docs` to `/gh-pages` and replace `.md`
URLs in SVGs with `.html`, and deploy these directories to branches
with the same names.

## Running Scripts

`/docs` can be generated locally (assuming the main branch of
`cryptol-course` has been cloned or downloaded) by installing
[Python 3.9+](https://python.org) and
[Poetry](https://python-poetry.org/), setting the `GITHUB_WORKSPACE`
environment variable to the folder containing `cryptol-course` and
running `generate.sh` (`GITHUB_WORKSPACE` is a default environment
variable for GitHub Actions):

```
cryptol-course> GITHUB_WORKSPACE=$(pwd) scripts/layercake/generate.sh
```

From here (assuming `cryptol` has been installed or aliased), the
test script can be run after setting CRYPTOLPATH to the generated
`/docs` folder:

```
cryptol-course> CRYPTOLPATH=$(pwd)/docs scripts/test_cryptol_modules.sh
```
