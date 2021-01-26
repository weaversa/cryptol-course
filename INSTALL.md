# Acquiring the Cryptol course material

All of the Cryptol course material (presentations, labs, supporting
data files) is available on the `docs` branch of
[the `weaversa/cryptol-course` repo on GitHub](https://github.com/weaversa/cryptol-course).
You can acquire this course material by doing any of the following:
   * Download [this ZIP file](https://github.com/weaversa/cryptol-course/archive/docs.zip)
     into a writeable "workspace" folder of your choice.
   * **OR** if `git` is installed on your system, clone this repo and
     check out its `docs` branch *(no password or keys required)*:
     ```
     > cd {a writeable folder for your workspace}
     > git clone git@github.com:weaversa/cryptol-course.git
     > git fetch --all
     > git checkout docs
     ```
   * **OR** if `git` is not installed, download it via `curl` or a
     similar command line data transfer utility:
     ```
     > cd {a writeable folder for your workspace}
     > curl -L -o cryptol-course.zip https://github.com/weaversa/cryptol-course/archive/docs.zip
     > unzip cryptol-course.zip
     ```

The presentation material is formatted in Markdown (`.md`) files that
can be viewed directly in most browsers or IDEs by installing a
Markdown plugin *(an exercise left to the reader)*, or by browsing
the accompanying [GitHub Pages](https://weaversa.github.io/cryptol-course).

-----

# Installing Cryptol and SAW

This course currently focuses on learning to program in Cryptol. As
such, you will need to be able to run the Cryptol
interpreter. Supported platforms include **up-to-date** CentOS,
Ubuntu, MacOS, and Windows 10.

Some challenge labs make use of SAW (Software Analysis Workbench), a
companion verification tool associated with Cryptol. However, SAW is not
a requirement for success here.

## Recommended Installation Instructions (Visual Studio Code + Docker)

1. Download [the Cryptol course
   repository](https://github.com/weaversa/cryptol-course/archive/docs.zip)
   and unzip it somewhere on your computer.
2. Install Docker:
   [https://docs.docker.com/get-docker](https://docs.docker.com/get-docker)
   * Linux users: enable user-mode access to docker by executing `sudo
     groupadd docker && sudo usermod -aG docker $USER`, then logout
     and log back in.
3. Install Visual Studio Code:
   [https://code.visualstudio.com](https://code.visualstudio.com)
   * **If installing on Windows, be sure to download the "User Installer"**.
   _VS Code is used here for viewing and editing Cryptol modules and
   interacting with the Cryptol interpreter._
4. Open Visual Studio Code (command name: `code`) and 
    1. Install the "vscode-pdf" extension: click **View ->
       Extensions**, search for "tomoki1207", select and install.
       _This extension is for viewing pdf files in VS Code._
    2. Install the "Markdown Preview Enhanced" extension: click **View
       -> Extensions**, search for
       "shd101wyy.markdown-preview-enhanced", select and install.
       _This extension offers an enhanced "preview" view of any Markdown
       file being viewed.  It will create a second "Open Preview to the
       Side" icon above the Markdown document._<br/>
       Then, in the Extensions list, select the gear icon (hovertext 
       "Manage") for this extension, then Extension Settings.  Uncheck the
       Break On Single New Line option, then close the tab.
       _This will remove some unnecessary line breaks from the enhanced
       Preview view._
    3. Install the "Remote - Containers" extension: click **View ->
       Extensions**, search for "ms-vscode-remote.remote-containers",
       select and install.
    4. Open the cryptol-course directory: click **File -> Open
       Folder...** (or just **Open...** on MacOS) and select the top
       directory of the unzipped course repository, then click **OK**.
       * Be sure to select the top-level cryptol-course directory, not a
         subdirectory.
    5. Visual Studio Code will detect the remote container and prompt
       you in the lower right corner of the window -- choose **Reopen in
       Container**.
       * If you miss the prompt, click the `><` box (hovertext: "open a
         remote window") in the lower left hand corner of the window and
         choose "Remote-Containers: Reopen in Container".
       * It will take a while for Docker to build the container the
         first time. Once it's done, you can run Cryptol by clicking
         **Terminal -> Run Task... -> Cryptol**. (Or else type CTRL-backtick
         for a terminal window, then enter the command `cryptol`.)

If a terminal window appears with the Cryptol logo, you're done. Feel
free to load the next lab into the interpreter by typing:

```Xcryptol-session
Loading module Cryptol
Cryptol> :m labs::Overview::Overview
Loading module Cryptol
Loading module labs::Overview::Overview
```

You may now start using Visual Studio Code to work through the
course. We hope you enjoy learning Cryptol!

Link to the next lab: [Cryptol and SAW
Overview](./labs/Overview/Overview.md).

## Installation Instructions for Expert Users

More involved instructions follow for those powerusers that are
familiar with installing and using terminal-based tools and that want
a more customized environment in which to program Cryptol.

### Option 1: Docker

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

```
$ docker pull cryptolcourse/cryptol
...
$ docker pull cryptolcourse/saw
...
```

You should now test your installation by running Cryptol. Instructions
can be found by scrolling down to the "Running Cryptol" section.

### Option 2: Homebrew

[Homebrew](https://brew.sh) is a package manager for MacOS, Linux, and
Windows Subsystem for Linux. Instructions for installing Homebrew can
be found on [Homebrew's website](https://brew.sh), and consist of
pasting a simple command into a shell prompt.

Once Homebrew is installed, Cryptol (along with its `z3` dependency)
can be installed via:

```
brew update && brew install cryptol
```

Unfortunately, SAW is not available via Homebrew.

You should now test your installation by running Cryptol. Instructions
can be found by scrolling down to the "Running Cryptol" section.

### Option 3: Downloading pre-built Cryptol and SAW binaries

#### Downloading Cryptol and SAW

Galois provides releases of Cryptol at
https://cryptol.net/downloads.html and releases of SAW at
https://saw.galois.com/downloads.html. For Linux variants, Cryptol
comes bundled with SAW, so you will only need to install SAW to get
both tools.

The `bin` directory (containing `cryptol` and/or `saw`) of the archive
you downloaded should be placed in your system path.

For CentOS or Ubuntu, the whole process would look like:

```
$ curl -fsSL https://github.com/GaloisInc/saw-script/releases/download/v0.6/saw-0.6-Linux-x86_64.tar.gz | tar -xz
$ export PATH=$(pwd)/saw-0.6-Linux-x86_64/bin:${PATH}
```

*If you are running Windows 10, or you _only_ want Cryptol, you can
find an installer at https://cryptol.net/downloads.html. Note that
these instructions do not currently provide any details on how to
install Cryptol on Windows 10, though the installer is self
explanatory.*

*Prebuilt SAW binaries for CentOS, Ubuntu, and MacOS can also be
found here: https://saw.galois.com/builds/

#### Downloading Z3

Both Cryptol and SAW require the [Z3 Prover](https://github.com/z3prover/z3),
a third-party theorem prover based on [Boolean satisfiability (SAT)](https://en.wikipedia.org/wiki/Boolean_satisfiability_problem)
developed by Microsoft. Z3 is not bundled with Cryptol or SAW, so it
must be installed separately.
*(Note that the `z3` package available via default `apt` repos is
out of date and incompatible with this course.)*

Z3 can be installed as a [pre-built binary release](https://github.com/Z3Prover/z3/releases)
or compiled from [source](https://github.com/z3prover/z3) using
Visual Studio, GNU Make, or CMake. After installation, the directory
containing the (downloaded or built) `z3` binary should be added to
your system's `PATH` environment variable.

For Mac OS, the whole process would look something like
(depending on which OS build and version you download):

```
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
Docker container. We'll use the 
[One-Time Pad](./labs/Demos/Cryptol/OneTimePad.md) as an
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

### Using Docker on Linux and MacOS

```
.../cryptol-course> docker run --rm -it --read-only --mount type=bind,src=$(pwd),dst=/mnt/cryptol-course --env CRYPTOLPATH=/mnt/cryptol-course cryptolcourse/cryptol
    ...
Loading module Cryptol
```

```Xcryptol-session
Cryptol> :module labs::Demos::Cryptol::OneTimePad
Loading module Cryptol
Loading module labs::Demos::Cryptol::OneTimePad
labs::Demos::Cryptol::OneTimePad>
```

### Using Docker on Windows 10

```
...\cryptol-course> docker run --rm -it --read-only --mount type=bind,src=%CD%,dst=/mnt/cryptol-course --env CRYPTOLPATH=/mnt/cryptol-course cryptolcourse/cryptol
    ...
Loading module Cryptol
```

```Xcryptol-session
Cryptol> :module labs::Demos::Cryptol::OneTimePad
Loading module Cryptol
Loading module labs::Demos::Cryptol::OneTimePad
labs::Demos::Cryptol::OneTimePad>
```

## Running from a Direct Install

First ensure that the cryptol intepreter is started in the
`cryptol-course` directory, or set the environment variable
CRYPTOLPATH to point to that directory. Then, use the `:module`
command from within Cryptol to load the lab.

```
.../cryptol-course$ cryptol
    ...
Loading module Cryptol
```

```Xcryptol-session
Cryptol> :module labs::Demos::Cryptol::OneTimePad
Loading module Cryptol
Loading module labs::Demos::Cryptol::OneTimePad
labs::Demos::Cryptol::OneTimePad>
```
-----

# Editors & IDEs

Support exists for Cryptol (such as syntax highlighting and
interpreter bindings) in a number of popular software development
tools.

## Visual Studio Code

The [Cryptol
Highlighting](https://github.com/GaloisInc/cryptol-vscode.git) plugin
provides syntax highlighting and an interface to a local Cryptol
installation (e.g. evaluate the current expression or get its type).

The local `.vscode` configuration in the `cryptol-course` repo
supports running a `cryptolcourse/cryptol` Docker image via **Terminal
-> Run Task... > Cryptol** in the VS Code menu bar.

## Emacs

A Cryptol major mode for Emacs can be found here:
https://github.com/thoughtpolice/cryptol-mode

## Vim

A Vim plugin for Cryptol can be found here:
https://github.com/victoredwardocallaghan/cryptol.vim

# Solicitation

How was your experience with this lab? Suggestions are welcome in the
form of a ticket on the course GitHub page:
https://github.com/weaversa/cryptol-course/issues

# From here, you can go somewhere!

||||
|-:|:-:|-|
|| [ ^ Cryptol Course ](README.md) ||
|| **Installation** | [ Overview > ](labs/Overview/Overview.md) |