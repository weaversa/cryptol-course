# How to Install Cryptol and SAW

## Option 1: Docker

[Docker](https://www.docker.com) images are available for both [Cryptol]
(https://hub.docker.com/r/galoisinc/cryptol) and [SAW]
(https://hub.docker.com/r/galoisinc/saw).  If Docker has been [installed]
(https://docs.docker.com/get-docker), it is easy to `pull` and `run` these
images:

```sh
> docker pull galoisinc/saw:0.5
...
> docker run -it galoisinc/saw:0.5
 ┏━━━┓━━━┓━┓━┓━┓
 ┃ ━━┓ ╻ ┃ ┃ ┃ ┃
 ┣━━ ┃ ╻ ┃┓ ╻ ┏┛
 ┗━━━┛━┛━┛┗━┛━┛ version 0.5 (<non-dev-build>)
sawscript> :h sat
Description
-----------

    sat : ProofScript SatResult -> Term -> TopLevel SatResult

Use the given proof script to attempt to prove that a term is
satisfiable (true for any input). Returns a proof result that can
be analyzed with 'caseSatResult' to determine whether it represents
a satisfiying assignment or an indication of unsatisfiability.
sawscript> ^d

> docker pull galoisinc/cryptol:2.8.0
...
> docker run -it galoisinc/cryptol:2.8.0
┏━╸┏━┓╻ ╻┏━┓╺┳╸┏━┓╻
┃  ┣┳┛┗┳┛┣━┛ ┃ ┃ ┃┃
┗━╸╹┗╸ ╹ ╹   ╹ ┗━┛┗━╸
version 2.8.0

Loading module Cryptol
Cryptol> :sat \(x:[4]) -> (x + 1 < x)
(\(x : [4]) -> (x + 1 < x)) 0xf = True
(Total Elapsed Time: 0.006s, using Z3)
Cryptol> ^d
```

`-it` indicates that the commands are to be run in an interactive
TTY. Tags `0.5` and `2.8.0` are needed because a `latest` alias has
not been set for these images. If you are currently in the root of
this repository, you can use `-v` and `--env` to mount the repository
in the docker image and set the `CRYPTOLPATH` environment variable for
access to this repository's Cryptol modules. For example,

```sh
[cryptol-course]$ docker run -v $(pwd):/mnt/cryptol-course --env CRYPTOLPATH=/mnt/cryptol-course -it galoisinc/cryptol:2.8.0
┏━╸┏━┓╻ ╻┏━┓╺┳╸┏━┓╻
┃  ┣┳┛┗┳┛┣━┛ ┃ ┃ ┃┃
┗━╸╹┗╸ ╹ ╹   ╹ ┗━┛┗━╸
version 2.8.0

Loading module Cryptol
Cryptol> :m specs::Misc::Sudoku
Loading module specs::Misc::Sudoku
specs::Misc::Sudoku>
Cryptol> ^d
```

## Option 2: Downloading pre-built Cryptol and SAW binaries

Galois (the maintainers of Cryptol and SAW) provide a server with
nightly builds of SAW for CentOS, Ubuntu, and OSX. Notice that there
are no builds for the Windows operating system. Cryptol comes bundled
with SAW, so if your computer is running one of these operating
systems, you should download a recent version from here:

https://saw.galois.com/builds/nightly/

If you are running Windows, or you _only_ want Cryptol, you can find
nightly builds here:

https://cryptol.net/builds/nightly/

Note that this markdown file does not currently provide any details on
how to install Cryptol on Windows.

The `bin` directory (containing `cryptol` and/or `saw`) of the archive
you downloaded should be placed in your system path.

For CentOS, Ubuntu, or OSX, the whole process would look something
like (pending on which OS build and version you download):

```
$ curl -fsSL https://saw.galois.com/builds/nightly/saw-0.4.0.99-2020-05-21-OSX-64.tar.gz -o saw-0.4.0.99-2020-05-21-OSX-64.tar.gz
$ tar -xzf saw-0.4.0.99-2020-05-21-OSX-64.tar.gz
$ export PATH=$PATH:$(pwd)/saw-0.4.0.99-2020-05-21-OSX-64/bin
```

It may behoove you at this point to add this path to your user profile
by adding an `export PATH=...` line to your `.bashrc`, `.profile`,
etc. file.


### Downloading Z3

Both Cryptol and SAW require a tool called Z3. Unfortunately, this
tool is not bundled with Cryptol or SAW, so it must be install
separately.

Pre-built binaries for Z3 can be found here:

https://github.com/Z3Prover/z3/releases

Z3 can also be compiled from source without much, if any trouble. The
source is available here:

https://github.com/Z3Prover/z3

The base directory (containing `z3` and more) of the zip file you
downloaded should be place in your system path.

For CentOS, Ubuntu, or OSX, the whole process would look something
like (pending on which OS build and version you download):

```
$ curl -fsSL https://github.com/Z3Prover/z3/releases/download/z3-4.8.8/z3-4.8.8-x64-osx-10.14.6.zip -o z3-4.8.8-x64-osx-10.14.6.zip
$ unzip -j z3-4.8.8-x64-osx-10.14.6.zip -d z3-4.8.8
$ export PATH=$PATH:$(pwd)/z3-4.8.8
```

It may behoove you at this point to add this path to your user profile
by adding an `export PATH=...` line to your `.bashrc`, `.profile`,
etc. file.


## Checking the Installation

To verify that Cryptol and SAW work, simply run their interpreters and
see that they report no errors.

```
$ saw
 ┏━━━┓━━━┓━┓━┓━┓
 ┃ ━━┓ ╻ ┃ ┃ ┃ ┃
 ┣━━ ┃ ╻ ┃┓ ╻ ┏┛
 ┗━━━┛━┛━┛┗━┛━┛ version 0.4.0.99 (eeef9a13)

sawscript> :q
```
```
$ cryptol
┏━╸┏━┓╻ ╻┏━┓╺┳╸┏━┓╻  
┃  ┣┳┛┗┳┛┣━┛ ┃ ┃ ┃┃  
┗━╸╹┗╸ ╹ ╹   ╹ ┗━┛┗━╸
version 2.8.1 (e914cef)
https://cryptol.net  :? for help

Loading module Cryptol
Cryptol> :q
```

## Help

For more help installing Cryptol or SAW, please refer to the
documentation provided in their respective repositories, found here:

https://github.com/GaloisInc/cryptol
https://github.com/GaloisInc/saw-script
