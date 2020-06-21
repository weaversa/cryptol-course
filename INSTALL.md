# Installing Cryptol and SAW

This course currently focuses on learning to program in Cryptol. As
such, you will greatly benefit from installing the Cryptol
interpreter.

Some challenge labs make use of SAW, a companion verification tool
associated with Cryptol. However, SAW is not a requirement for success
here. Also, [Galois](https://galois.com) (the maintainer of Cryptol
and SAW) does not currently provide a Windows installer for SAW and
Homebrew ([Option 2](#option-2-homebrew)) doesn't provide SAW
installers on any platform. So, if you want to use SAW, we recommend
installing SAW via docker ([Option 1](#option-1-docker)) as it is
platform agnostic and easy to install and use.

## Option 1: Docker

[Docker](https://www.docker.com) images are available for both
[Cryptol](https://hub.docker.com/r/galoisinc/cryptol) and
[SAW](https://hub.docker.com/r/galoisinc/saw). If Docker has been
[installed](https://docs.docker.com/get-docker), it is easy to `pull`
and `run` these images. *(Note that this docker approach may require
`sudo` privileges. If so, and you don't have such privileges, follow
the steps in [Option 2](#option-2-homebrew) or [Option
3](#option-3-downloading-pre-built-cryptol-and-saw-binaries) for
user-mode solutions.)*

```
$ docker pull galoisinc/cryptol:2.8.0
...
$ docker run -it galoisinc/cryptol:2.8.0
┏━╸┏━┓╻ ╻┏━┓╺┳╸┏━┓╻
┃  ┣┳┛┗┳┛┣━┛ ┃ ┃ ┃┃
┗━╸╹┗╸ ╹ ╹   ╹ ┗━┛┗━╸
version 2.8.0

Loading module Cryptol
Cryptol> :sat \(x:[4]) -> (x + 1 < x)
(\(x : [4]) -> (x + 1 < x)) 0xf = True
(Total Elapsed Time: 0.006s, using Z3)
Cryptol> :quit
```

```sh
$ docker pull galoisinc/saw:0.5
...
$ docker run -it galoisinc/saw:0.5
 ┏━━━┓━━━┓━┓━┓━┓
 ┃ ━━┓ ╻ ┃ ┃ ┃ ┃
 ┣━━ ┃ ╻ ┃┓ ╻ ┏┛
 ┗━━━┛━┛━┛┗━┛━┛ version 0.5 (<non-dev-build>)
sawscript> :help sat
Description
-----------

    sat : ProofScript SatResult -> Term -> TopLevel SatResult

Use the given proof script to attempt to prove that a term is
satisfiable (true for any input). Returns a proof result that can
be analyzed with 'caseSatResult' to determine whether it represents
a satisfying assignment or an indication of unsatisfiability.
sawscript> :quit
```

Details:
- Instructions for installing `docker` on your system can be found at
[https://docs.docker.com/get-docker](https://docs.docker.com/get-docker).
- `docker run -it` indicates that the commands are to be run in an interactive
TTY.
- Version tags `0.5` and `2.8.0` are needed because a `latest` alias
has not been set for these images.
- If you are currently in the root of this repository, you can use
`-v` and `--env` to mount the repository in the docker image and set
the `CRYPTOLPATH` environment variable for access to this repository's
Cryptol modules. This environment variable is used by both Cryptol and
SAW. For example:

```sh
$ docker run -v $(pwd):/mnt/cryptol-course --env CRYPTOLPATH=/mnt/cryptol-course -it galoisinc/cryptol:2.8.0
┏━╸┏━┓╻ ╻┏━┓╺┳╸┏━┓╻
┃  ┣┳┛┗┳┛┣━┛ ┃ ┃ ┃┃
┗━╸╹┗╸ ╹ ╹   ╹ ┗━┛┗━╸
version 2.8.0

Loading module Cryptol
Cryptol> :module specs::Misc::Sudoku
Loading module specs::Misc::Sudoku
specs::Misc::Sudoku> :quit
```


## Option 2: Homebrew

[Homebrew](https://brew.sh) is a package manager for OSX, Linux, and
Windows Subsystem for Linux. Instructions for installing Homebrew can
be found on Homebrew's website [https://brew.sh](https://brew.sh), and
consist of pasting a simple command into a shell prompt.

Once Homebrew is installed, Cryptol (along with it's `z3` dependency)
can be installed via:

```sh
brew update && brew install cryptol
```

Unfortunately, SAW is not available via Homebrew.


## Option 3: Downloading pre-built Cryptol and SAW binaries

### Downloading Cryptol and SAW

Galois provide a server with nightly builds of SAW for CentOS, Ubuntu,
and OSX. Cryptol comes bundled with SAW, so if your computer is
running one of these operating systems, you should download a recent
version from https://saw.galois.com/builds/nightly. *(Note that the
Ubuntu files indicate Ubuntu14.04, but they work on later versions of
Ubuntu as well.)*

The `bin` directory (containing `cryptol` and/or `saw`) of the archive
you downloaded should be placed in your system path.

For CentOS, Ubuntu, or OSX, the whole process would look something
like (depending on the date and which OS build and version you download):

```
$ curl -fsSL https://saw.galois.com/builds/nightly/saw-0.4.0.99-2020-05-21-OSX-64.tar.gz -o saw-0.4.0.99-2020-05-21-OSX-64.tar.gz
$ tar -xzf saw-0.4.0.99-2020-05-21-OSX-64.tar.gz
$ export PATH=$(pwd)/saw-0.4.0.99-2020-05-21-OSX-64/bin:${PATH}
```

*If you are running Windows, or you _only_ want Cryptol, you can find
nightly builds at https://cryptol.net/builds/nightly/.  Note that
these instructions do not currently provide any details on how to
install Cryptol on Windows.*

### Downloading Z3

Both Cryptol and SAW require a tool called `z3`. This tool is not
bundled with Cryptol or SAW, so it must be installed separately.
*(Note that the version of `z3` available via `apt` is old and
incompatible with this course.)*

Pre-built binaries for Z3 can be found at
https://github.com/Z3Prover/z3/releases.  Z3 can also be compiled from
source without much, if any, trouble. The source is available at
https://github.com/Z3Prover/z3.  The base directory (containing `z3`
and more) of the zip file you download or built should be placed in
your system path.

For CentOS, Ubuntu, or OSX, the whole process would look something
like (depending on which OS build and version you download):

```
$ curl -fsSL https://github.com/Z3Prover/z3/releases/download/z3-4.8.8/z3-4.8.8-x64-osx-10.14.6.zip -o z3-4.8.8-x64-osx-10.14.6.zip
$ unzip -j z3-4.8.8-x64-osx-10.14.6.zip -d z3-4.8.8
$ export PATH=$(pwd)/z3-4.8.8:${PATH}
```

It may behoove you at this point to add these paths to your user profile
by adding an `export PATH=...` line to your `.bashrc`, `.profile`,
etc. file to ensure future access to the tools.


## Checking the Installation

To verify that Cryptol and SAW work, simply run their interpreters and
see that they report no errors.

```
$ cryptol
┏━╸┏━┓╻ ╻┏━┓╺┳╸┏━┓╻  
┃  ┣┳┛┗┳┛┣━┛ ┃ ┃ ┃┃  
┗━╸╹┗╸ ╹ ╹   ╹ ┗━┛┗━╸
version 2.8.1 (e914cef)
https://cryptol.net  :? for help

Loading module Cryptol
Cryptol> :quit
```

```
$ saw
 ┏━━━┓━━━┓━┓━┓━┓
 ┃ ━━┓ ╻ ┃ ┃ ┃ ┃
 ┣━━ ┃ ╻ ┃┓ ╻ ┏┛
 ┗━━━┛━┛━┛┗━┛━┛ version 0.4.0.99 (eeef9a13)

sawscript> :quit
```

## Help

For more help installing Cryptol or SAW, please refer to the
documentation provided in their respective repositories, found here:

* Cryptol: https://github.com/GaloisInc/cryptol
* SAW: https://github.com/GaloisInc/saw-script

-----

# Running Cryptol

To load a literate document into Cryptol, change to your
`cryptol-course` directory in a terminal (Linux) or command prompt
(Windows), then run Cryptol via a locally installed binary or Docker
image. We'll use the
[labs/Demos/OneTimePad.md](labs/Demos/OneTimePad.md) as an example.

### Linux

```sh
.../cryptol-course> cryptol labs/Demos/OneTimePad.md
...

Loading module Cryptol
Loading module labs::Demos::OneTimePad
labs::Demos::OneTimePad> 
```

Alternatively, you can use the `:m` or `:l` command from within 
Cryptol to load this document. (To avoid errors, make sure Cryptol 
was started in the `cryptol-course` directory). 

```sh
.../cryptol-course> cryptol
...

Loading module Cryptol
Cryptol> :m labs::Demos::OneTimePad
Loading module labs::Demos::OneTimePad
labs::Demos::OneTimePad> 
```

### Windows

```sh
.../cryptol-course> cryptol labs/Demos/OneTimePad.md
...

Loading module Cryptol
Loading module labs::Demos::OneTimePad
labs::Demos::OneTimePad> 
```

Alternatively, you can use the `:m` or `:l` command from within 
Cryptol to load this document. (To avoid errors, make sure Cryptol 
was started in the `cryptol-course` directory). 

```sh
...\cryptol-course> cryptol
...

Loading module Cryptol
Cryptol> :m labs::Demos::OneTimePad
Loading module labs::Demos::OneTimePad
labs::Demos::OneTimePad> 
```

### Docker (Linux)
```sh
.../cryptol-course> docker run --read-only --mount type=bind,src=$(pwd),dst=/mnt/cryptol-course --env CRYPTOLPATH=/mnt/cryptol-course -it cryptolcourse/cryptol
Loading module Cryptol
Cryptol> :m labs::Demos::OneTimePad
Loading module labs::Demos::OneTimePad
labs::Demos::OneTimePad> 
```

### Docker (Windows)
```sh
...\cryptol-course> docker run --read-only --mount type=bind,src=%CD%,dst=/mnt/cryptol-course --env CRYPTOLPATH=/mnt/cryptol-course -it cryptolcourse/cryptol
Loading module Cryptol
Cryptol> :m labs::Demos::OneTimePad
Loading module labs::Demos::OneTimePad
labs::Demos::OneTimePad> 
```