# Acquiring the Cryptol course material

All of the Cryptol course material (presentations, labs, supporting
data files) is available on
[GitHub](https://github.com/weaversa/cryptol-course). You can clone or
download the files using the green `clone` button on the GitHub page,
or you can use the command line to acquire a copy by ensuring you're
in a writable working directory and issuing `git clone
https://github.com/weaversa/cryptol-course.git` *(no password or keys
required)*, or if you don't have `git` installed, `curl -L
-ocryptol-course.zip
https://github.com/weaversa/cryptol-course/archive/master.zip && unzip
cryptol-course.zip`.

The presentation material is formatted in Markdown (.md) files. These
can be viewed directly in most browsers by installing a Markdown
viewer extension *(an exercise left to the reader)*, or by accessing
the material directly from the GitHub website.

-----

# Installing Cryptol and SAW

This course currently focuses on learning to program in Cryptol. As
such, you will need to be able to run the Cryptol
interpreter. Supported platforms include **up-to-date** CentOS,
Ubuntu, MacOS, and Windows 10.

Some challenge labs make use of SAW, a companion verification tool
associated with Cryptol. However, SAW is not a requirement for success
here.

## Quickstart

1. Download this repository and unzip it somewhere on your computer.
2. Install Docker:
   [https://docs.docker.com/get-docker](https://docs.docker.com/get-docker)
3. Open a terminal window (for Windows use Command Prompt or
   PowerShell, but not PowerShell ISE). Run
   ```shell
   > docker pull cryptolcourse/cryptol
   ```
4. Install VS Code:
   [https://code.visualstudio.com](https://code.visualstudio.com)
5. In VS Code,
   * click "File" -> "Open Workspace", and select the directory where
   you unziped this repository.
   * In the "Explorer" pane, select `README.md` (or any lab you want
     to work through).
   * Click "Terminal" -> "Run Task" -> "cryptol-docker".

If a terminal window appears with the Cryptol logo, you're done. The
lab you selected should be loaded into the interpreter. Start using VS
Code to work through the course. And, enjoy learning Cryptol!

More involved instructions follow for those powerusers that are
familiar with installing and using terminal-based tools and that want
a more customized environment in which to program Cryptol.

## Option 1: Docker (*Preferred Installation Method*)

[Docker](https://www.docker.com) containers are available for both
[Cryptol](https://hub.docker.com/repository/docker/cryptolcourse/cryptol)
and
[SAW](https://hub.docker.com/repository/docker/cryptolcourse/saw). To
use these Docker containers you'll *first* need to install Docker on
your computer. Instructions for installing Docker on Linux, MacOS, and
Windows 10 can be found at
[https://docs.docker.com/get-docker](https://docs.docker.com/get-docker). Once
Docker has been [installed](https://docs.docker.com/get-docker), it is
easy to `pull` and `run` these containers. *(Note that this docker
approach may require `sudo` privileges. If so, and you don't have such
privileges, follow the steps in [Option 2](#option-2-homebrew) or
[Option 3](#option-3-downloading-pre-built-cryptol-and-saw-binaries)
for user-mode solutions.)*

The Cryptol and *optional* SAW docker images can be downloaded by
issuing the following Docker commands in your computer's terminal.

```shell
$ docker pull cryptolcourse/cryptol
...
$ docker pull cryptolcourse/saw
...
```

You should now test your installation by running Cryptol. Instructions
can be found by scrolling down to the "Running Cryptol" section.

## Option 2: Homebrew

[Homebrew](https://brew.sh) is a package manager for MacOS, Linux, and
Windows Subsystem for Linux. Instructions for installing Homebrew can
be found on [Homebrew's website](https://brew.sh), and consist of
pasting a simple command into a shell prompt.

Once Homebrew is installed, Cryptol (along with its `z3` dependency)
can be installed via:

```shell
brew update && brew install cryptol
```

Unfortunately, SAW is not available via Homebrew.

You should now test your installation by running Cryptol. Instructions
can be found by scrolling down to the "Running Cryptol" section.

## Option 3: Downloading pre-built Cryptol and SAW binaries

### Downloading Cryptol and SAW

Galois provides releases of Cryptol at
https://cryptol.net/downloads.html and releases of SAW at
https://saw.galois.com/downloads.html. For Linux variants, Cryptol
comes bundled with SAW, so you will only need to install SAW to get
both tools. *(Note that the Ubuntu files indicate Ubuntu14.04, but
they work on later versions of Ubuntu as well.)*

The `bin` directory (containing `cryptol` and/or `saw`) of the archive
you downloaded should be placed in your system path.

For CentOS, Ubuntu, or MacOS, the whole process would look something
like (depending on the which OS variant you have):

```shell
$ curl -fsSL https://github.com/GaloisInc/saw-script/releases/download/v0.5/saw-0.5-Ubuntu14.04-64.tar.gz | tar -xz
$ export PATH=$(pwd)/saw-0.5-Ubuntu14.04-64/bin:${PATH}
```

*If you are running Windows 10, or you _only_ want Cryptol, you can
find an installer at https://cryptol.net/downloads.html. Note that
these instructions do not currently provide any details on how to
install Cryptol on Windows 10, though the installer is self
explanatory.*

*Prebuilt SAW binaries for CentOS, Ubuntu, and MacOS can be found
here: https://saw.galois.com/*

### Downloading Z3

Both Cryptol and SAW require a tool called `z3`. This tool is not
bundled with Cryptol or SAW, so it must be installed separately.
*(Note that the version of `z3` available via default `apt` repos is
old and incompatible with this course.)*

Pre-built binaries for Z3 can be found at
https://github.com/Z3Prover/z3/releases.  Z3 can also be compiled from
source without much, if any, trouble. The source is available at
https://github.com/Z3Prover/z3.  The base directory (containing `z3`
and more) of the zip file you download or built should be placed in
your system path.

For CentOS, Ubuntu, or MacOS, the whole process would look something
like (depending on which OS build and version you download):

```shell
$ curl -fsSL https://github.com/Z3Prover/z3/releases/download/z3-4.8.8/z3-4.8.8-x64-osx-10.14.6.zip -o z3-4.8.8-x64-osx-10.14.6.zip
$ unzip -j z3-4.8.8-x64-osx-10.14.6.zip -d z3-4.8.8
$ export PATH=$(pwd)/z3-4.8.8:${PATH}
```

It may behoove you at this point to add these paths to your user
profile by adding an `export PATH=...` line to your `.bashrc`,
`.profile`, etc. file to ensure future access to the tools.

You should now test your installation by running Cryptol. Instructions
can be found by scrolling down to the "Running Cryptol" section.

-----

# Installation Help

For more help installing Cryptol or SAW, please refer to the
documentation provided in their respective repositories, found here:

* Cryptol: https://github.com/GaloisInc/cryptol
* SAW: https://github.com/GaloisInc/saw-script

-----

# Running Cryptol

To load a literate document into Cryptol, change to your
`cryptol-course` directory in a terminal (Linux) or command prompt
(Windows 10), then run Cryptol via a locally installed binary or
Docker container. We'll use
[labs/Demos/OneTimePad.md](labs/Demos/OneTimePad.md) as an
example. That markdown file is a literate Cryptol module (as are all
the labs) and as such can be loaded into the Cryptol interpreter.

## Running from a Docker Install

The Cryptol Docker container needs access to the course materials in
your `cryptol-course` directory. To do this, run `docker` from within
the `cryptol-course` directory and as follows:

Details:
- `docker run --rm -it --read-only` indicates that the commands are to
be run in a read-only interactive TTY, and the newly generated
container will be removed upon exit.
- If you are currently in the root of the `cryptol-course` directory,
you can use `--mount`, and `--env` to mount the directory in the
docker container and set the `CRYPTOLPATH` environment variable for
access to the directory's Cryptol modules. This environment variable
is used by both Cryptol and SAW.

### Using Docker on Linux and OSX

```shell
.../cryptol-course> docker run --rm -it --read-only --mount type=bind,src=$(pwd),dst=/mnt/cryptol-course --env CRYPTOLPATH=/mnt/cryptol-course cryptolcourse/cryptol
    ...
Loading module Cryptol
Cryptol> :module labs::Demos::OneTimePad
Loading module labs::Demos::OneTimePad
labs::Demos::OneTimePad>
```

### Using Docker on Windows 10

```shell
...\cryptol-course> docker run --rm -it --read-only --mount type=bind,src=%CD%,dst=/mnt/cryptol-course --env CRYPTOLPATH=/mnt/cryptol-course cryptolcourse/cryptol
    ...
Loading module Cryptol
Cryptol> :module labs::Demos::OneTimePad
Loading module labs::Demos::OneTimePad
labs::Demos::OneTimePad>
```

## Running from a Direct Install

First ensure that the cryptol intepreter is started in the
`cryptol-course` directory, or set the environment variable
CRYPTOLPATH to point to that directory. Then, use the `:module`
command from within Cryptol to load the lab.

```shell
.../cryptol-course$ cryptol
    ...
Loading module Cryptol
Cryptol> :module labs::Demos::OneTimePad
Loading module labs::Demos::OneTimePad
labs::Demos::OneTimePad>
```
-----

# Editors & IDEs

Support exists for Cryptol (such as syntax highlighting and
interpreter bindings) in a number of popular software development
tools.

## VS Code

The [Cryptol
Highlighting](https://github.com/GaloisInc/cryptol-vscode.git) plugin
provides syntax highlighting and an interface to a local Cryptol
installation (e.g. evaluate the current expression or get its type).

The local `.vscode` configuration in the `cryptol-course` repo
supports running a `cryptolcourse/cryptol` Docker image via `Terminal
> Run Task... > cryptol-docker` in the VS Code menu bar.

## Emacs

A Cryptol major mode for Emacs can be found here:
https://github.com/thoughtpolice/cryptol-mode

## Vim

A Vim plugin for Cryptol can be found here:
https://github.com/victoredwardocallaghan/cryptol.vim
