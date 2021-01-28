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
environment variable to the folder containing `cryptol-course`, and
running `generate.sh` (`GITHUB_WORKSPACE` is a default environment
variable for GitHub Actions; these instructions assume a Linux or
Mac OS host):

```
cryptol-course> GITHUB_WORKSPACE=$(pwd) scripts/layercake/generate.sh
```

From here (assuming `cryptol` has been installed or aliased), the
test script can be run after setting CRYPTOLPATH to the generated
`/docs` folder:

```
cryptol-course> CRYPTOLPATH=$(pwd)/docs scripts/test_cryptol_modules.sh
```

## Adding a New Lab/Module

Suppose we wish to introduce a new Literate Cryptol module
"Frobnicator" to the course, as part of a new lab "Frobnication".  We
would start by writing a Markdown file consistent with other modules
in the course, with at least the following snippet...

````gfm
```cryptol
module Frobnication::Frobnicator where
```
````

...and subsequent Cryptol definitions, often accompanied by exercises
for course participants, e.g.

````gfm
**EXERCISE**: Define a function `frobnicate` that frobnicates the
characters of a given `String n` `w` using a `Frobnifier` `f`.
(Replace `undefined` with your definition.)

```cryptol
frobnicate: Frobnifier -> String n -> String n
frobnicate f w = undefined
```
````

This Markdown file would be located at
`/templates/labs/Frobnication/Frobnicator.md`.  If it has
`**EXERCISE**`s, a corresponding file with solutions would be located
at `/templates/labs/Frobnication/FrobnicatorAnswers.md`:

````gfm
**EXERCISE**: Define a function `frobnicate` that frobnicates the
characters of a given `String n` `w` using a `Frobnifier` `f`.
(Replace `undefined` with your definition.)

```cryptol
frobnicate:
    {n}
    fin n =>
    Frobnifier -> String n -> String n
frobnicate f w = last ws
  where
    ws = [ w ]
        # [ updates wx [i, j] (wx @@ [j, i])
          | (i, j) <- r`{n}
          | wx <- ws
          ]

    r:
        {m}
        fin m =>
        [m-(min 1 m)](Integer, Integer)
    r =
        [ (i, (x % (`m - i)) + i)
        | (i, x) <- enumerate (random f : [m - (min 1 m)]Integer)
        ]
```
````

### Metadata

Recommended course modules are to be taken in a certain order, and
optional ones are suggested to be taken after a recommended one, each
meeting prerequisites necessary to successfully complete a given
module.  These are represented in the metadata file `deps.yml`, which
specifies a recommended path and optional branches from recommended
modules using
[Yet Another Markup Language (YAML)](https://yaml.org/).

For our frobnicating example, we would add an entry to `urls` with a
label and the location of the file with unanswered exercises:

```yaml
urls:
  ...
  Frobnicators:
    labs/Frobnication/Frobnicator.md
```

This example would be optional, so a branch would be added from a
recommended module, perhaps `Cryptographic Properties`:

```yaml
branches:
  ...
  Cryptographic Properties:
    - ...
    - Frobnicators
```

To add a multi-module lab with recommended content (e.g.
`Block Modes` with modules for `ECB`, `CBC`, etc.), the lab would be
added to the recommended path for `Cryptol Course`, and a recommended
path would be specified for the new lab:

```yaml
urls:
  ...
  Block Modes:
    labs/BlockModes/Overview.md
  ECB:
    labs/BlockModes/ECB.md
  CBC:
    labs/BlockModes/CBC.md
  ...

paths:
  Cryptol Course:
    - ...
    - Block Modes
    - ...
  ...
  Block Modes:
    - ECB
    - CBC
    ...
```

The overview Markdown could include a `{{ graphical_view }}` template
field, which the deployment scripts would replace with GraphViz and
SVG files and a link from the overview to the SVG file.  Each module
(including the overview) could have a field `{{ solicitation }}`
replaced with standard boilerplate inviting participants to submit
feedback via GitHub Issues, and a field `{{ navlinks }}` that would
be replaced with a table of links to related modules.  These would be
deployed to the `docs` branch that can be downloaded for local
browsing, and the `gh-pages` branch which can be viewed at the
[GitHub Pages for the course](https://weaversa.github.io/cryptol-course).

There's more opportunity for CI scripting and templating.  For
example, we have written a script to extract code blocks with the
`Xcryptol-session` info string and generate unit tests to confirm
that Cryptol behaves as documented in each module.  We could also
sanity-check pre/post-requisites for modules, e.g. checking that a
module includes a `:prove` example if it says it does, or that a
module's listed prerequisites are included among postrequisites of
its dependencies.

# Go Forth and Educate

Happy authoring!
