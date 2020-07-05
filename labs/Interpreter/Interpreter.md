# Using the Cryptol interpreter

The first thing you should do is start up the Cryptol
interpreter. Instructions on installing and running the interpreter
can be found in this repo's [INSTALL.md](../../INSTALL.md) file. Once
you've got the interpreter started, follow along with this lab by
entering the commands found here into the interpreter.


## Interpreter State

The interpreter has a number of different configuration settings. To
view them type `:set`.

```shell
Cryptol> :set
ascii = off
base = 16
core-lint = off
debug = off
infLength = 5
mono-binds = on
prover = z3
prover-stats = on
prover-validate = off
satNum = 1
show-examples = on
smtfile = -
tc-debug = 0
tc-solver = z3 -smt2 -in
tests = 100
warnDefaulting = on
warnShadowing = on
```

The most common setting to change is `base`. The default is `base =
16` which means the interpreter will display bitvectors by printing
their hexadecimal representations. For example,

```shell
Cryptol> 0xa
0xa
Cryptol> 10 : [4]
0xa
```

To make the interpreter display bitvectors in decimal (base 10) type
`:set base=10`.

```shell
Cryptol> :set base=10
Cryptol> 0xa
10
Cryptol> 10 : [4]
10
```

Feel free to change this setting to what is most comfortable for you.

You'll notice there are many different things to configure. Most of
these are set to an appropriate default, but there are a few we will
touch on later.


## Interpreter Help

Cryptol's interpreter has a built-in help command. To invoke it, type
`:h` followed by the command you'd like to know more about. For
example, if we'd like to know more about the `:set base` command, we
can type:

```shell
Cryptol> :h :set base

    base = 10

Default value: 16

The base to display words at (2, 8, 10, or 16).
```

Cryptol has a built-in command to reverse a list, called
`reverse`. Let's look at the help for this command.

```shell
Cryptol> :h reverse

    reverse : {n, a} (fin n) => [n]a -> [n]a

Reverses the elements in a sequence.
```


## Colon commands

You'll notice that some commands start with a colon (`:`) and others
do not. The colon commands are commands that are outside of the
Cryptol language, and only exist in the interpreter. You can see a full listing of these commands by typing `:h`.

```shell
Cryptol> :h
  :t, :type            Check the type of an expression.
  :b, :browse          Display environment for all loaded modules, or for a specific module.
  :?, :help            Display a brief description of a function, type, or command.
  :s, :set             Set an environmental option (:set on its own displays current values).
  :check               Use random testing to check that the argument always returns true.
                       (If no argument, check all properties.)
  :exhaust             Use exhaustive testing to prove that the argument always returns
                       true. (If no argument, check all properties.)
  :prove               Use an external solver to prove that the argument always returns
                       true. (If no argument, check all properties.)
  :sat                 Use a solver to find a satisfying assignment for which the argument
                       returns true. (If no argument, find an assignment for all properties.)
  :debug_specialize    Do type specialization on a closed expression.
  :eval                Evaluate an expression with the reference evaluator.
  :ast                 Print out the pre-typechecked AST of a given term.
  :extract-coq         Print out the post-typechecked AST of all currently defined terms,
                       in a Coq-parseable format.
  :q, :quit            Exit the REPL.
  :l, :load            Load a module by filename.
  :r, :reload          Reload the currently loaded module.
  :e, :edit            Edit FILE or the currently loaded module.
  :!                   Execute a command in the shell.
  :cd                  Set the current working directory.
  :m, :module          Load a module by its name.
  :w, :writeByteArray  Write data of type 'fin n => [n][8]' to a file.
  :readByteArray       Read data from a file as type 'fin n => [n][8]', binding
                       the value to variable 'it'.
  :dumptests           Dump a tab-separated collection of tests for the given
                       expression into a file. The first column in each line is
                       the expected output, and the remainder are the inputs. The
                       number of tests is determined by the "tests" option.
```

The most used commands have to do with files (loading `:l`, editing
`:e`, and reloading `:r` a file), quitting the interpreter `:q`, and
asking for the type of an expression `:t`. Many of the commands have
single character shortcuts for ease of use.


## Browsing the Environment

The `:browse` command will list all of the type synonyms, constraint
synonyms, primitive types, and symbols currently loaded into the
interpreter. Upon startup, the interpreter preloads a prelude
containing the Cryptol language. This prelude can be modified, but
this will likely make any Cryptol specifications you write
incompatible with the rest of the world, and hence modifying the
prelude is discouraged.

About `:browse` --- If you enter `:b` into the interpreter you will
first see:

```shell
Cryptol> :b
Type Synonyms
=============

  Public
  ------

    type Bool = Bit
    type Char = [8]
    type lg2 n = width (max 1 n - 1)
    type String n = [n][8]
    type Word n = [n]
    ...
```

Type synonyms are helper functions used to express the type of some
data. For example, The integer `10` can be expressed as a 32-bit word
via,

```shell
Cryptol> 10 : Word 32
0x0000000a
```

The next things you'll see with `:b` are some constraint
synonyms. These are helper functions used to express type
constraints. The ones preloaded into the interpreter unify different
types of comparison operators to `>=`. This just saves users from
having to express type constraints using only `>=`.

The next things you'll see with `:b` are some primitive types. These
include comparison and arithmetic operators, type classes, basic
types, and some type constraints that help with expressing some
cryptographic type constraints elegantly. These are used when defining
the type of a function or variable. For example, the size of a
bitvector can be represented in terms of the width of an integer:

```shell
Cryptol> :t 10 : [width 32]
(10 : [width 32]) : [6]
```

In the last section you'll see with `:b` are Cryptol's symbols. This
is where you'll find all of the value operators (as opposed to type
operators). These are used when defining the value of a function or
variable. For example, `reverse` can be used to reverse the order of a
sequence of integers:

```shell
Cryptol> reverse [1, 2, 3] : [3][2]
[0x3, 0x2, 0x1]
```

Each of the items in the environment can be queried using `:h`.


## Tab Completion and Scrolling

The interpreter supports [TAB
completion](https://en.wikipedia.org/wiki/Command-line_completion),
that is, pressing TAB will display all of available symbols. And, if
you start typing a symbol and then press TAB, the interpreter will
attempt to complete the symbol you've started typing.

The interpreter also records a history of commands issued. All
commands are saved in a file named `.cryptol_commandHistory` which (by
default) resides in the top-level of your user directory. Pressing the
up and down arrows will scroll through this history, enabling fast
recall of previous commands.


## Chaining Commands --- `it` and `let`

The Cryptol interpreter supports a couple of ways to chain commands
together. The first way happens automatically. The `it` symbol is a
name automatically bound to the result of the last command the
interpreter evaluated. For example, if we can `reverse` a list, the
result `[3, 2, 1]` is automatically bound to the `it` symbol. We can
then `reverse it` and see that we get `[1, 2, 3]` back.

```shell
Cryptol> :s base=10
Cryptol> reverse [1, 2, 3] : [3][2]
[3, 2, 1]
Cryptol> reverse it
[1, 2, 3]
```

Though, now the value of `it` has also become `[1, 2, 3]`. To bind a
value to a name (save it for later) we can use the `let` command. For
example, here we bind the result of `reverse [1, 2, 3] : [3][2]` to
`r`, then `reverse r` and show that the result is as expected and that
`r` still retains `[3, 2, 1]`.

```shell
Cryptol> let r = reverse [1, 2, 3] : [3][2]
Cryptol> r
[3, 2, 1]
Cryptol> reverse r
[1, 2, 3]
Cryptol> r
[3, 2, 1]
```

`let` is very helpful for debugging and program understanding,
however, it can cause confusion (as demonstrated in the simple example
below). Thus, industrial use of `let` is discouraged.

After running the example below, what is the value of `y`? Since, in
the interpreter, definitions can be overwritten, it's better to place
these kinds of definitions in a file and use the `:reload` (or `:r`)
command when editing to maintain a consistent state.

```shell
Cryptol> let x = 0
Cryptol> let y = x + 1
Cryptol> let x = 1
```


## Loading and Reloading Files

Speaking of loading files, there is a simple Cryptol file in the same
directory as this lab (`labs/Interpreter/test.cry`). If you run
Cryptol from the root of this repository, you can load this file using
`:l` like so:

```shell
[cryptol-course]$ cryptol
┏━╸┏━┓╻ ╻┏━┓╺┳╸┏━┓╻
┃  ┣┳┛┗┳┛┣━┛ ┃ ┃ ┃┃
┗━╸╹┗╸ ╹ ╹   ╹ ┗━┛┗━╸
version 2.8.0

Loading module Cryptol
Cryptol> :l labs/Interpreter/test.cry
Loading module Cryptol
Loading module labs::Interpreter::test
labs::Interpreter::test>
```

Here we see that this file contains a module named
`labs::Interpreter::test`, which really just describes it's filename
and directory path from the root of the repository.

We can browse the currently loaded module to see what new symbols it
provides. Remember that the interpreter supports TAB completion so you
only need to type `:b l` then press the TAB key and the interpreter
will fill in the rest.

```shell
labs::Interpreter::test> :b labs::Interpreter::test
Type Synonyms
=============

  Public
  ------

    type uint32_t = [32]

Symbols
=======

  Public
  ------

    f : uint32_t -> uint32_t
    x : uint32_t
    y : uint32_t
```

Here we see that this module provides one type synonym and three
symbols.


## Loading Modules

To load a module by its name (rather than by filename), we use
`:m`. For this command to work, either

  1) the Cryptol interpreter must have been started in the root
     directory of the module, or

  2) the root directory of the module must be in the CRYPTOLPATH
     environment variable.

The latter is preferred. To set the CRYPTOLPATH variable such that we
can access the labs and specs here, do this:

```shell
$ export CRYPTOLPATH=<path-to-cryptol-course>
cryptol-course]$ cryptol
┏━╸┏━┓╻ ╻┏━┓╺┳╸┏━┓╻
┃  ┣┳┛┗┳┛┣━┛ ┃ ┃ ┃┃
┗━╸╹┗╸ ╹ ╹   ╹ ┗━┛┗━╸
version 2.8.0

Loading module Cryptol
Cryptol> :m labs::Interpreter::test
Loading module labs::Interpreter::test
labs::Interpreter::test>
```


## Editing

The Cryptol interpreter supports editing the currently loaded file or
module via `:e`. However, if no file or module is loaded `:e` will
allow you to edit the Cryptol prelude, which is very dangerous. When
you type `:e`, Cryptol will load the current file or module in a text
editor, and reload the file when the editor is closed. The default
editor is [vim](https://www.vim.org/), but can be changed via setting
an environment variable called `EDITOR`. For example, if in a Linux
like environment, the following command will change the default to
[Emacs](https://www.gnu.org/software/emacs/).

```shell
$ export EDITOR="emacs -nw"
```


## Batch Commands

Interpreter commands can be issued directly from the command line, or
from a batch file. For example, here we issue some commands from the
command line using the interpreter's `-c` flag:

```shell
$ cryptol -c ":m labs::Interpreter::test" -c ":s base=10" -c "x + 2"
Loading module Cryptol
Loading module labs::Interpreter::test
3
```

And here we issue the same commands by running the `test.sry` batch
file using the interpreter's `-b` flag:

```shell
$ cat labs/Interpreter/test.sry
:m labs::Interpreter::test
:s base=10
x + 2
$ cryptol -b labs/Interpreter/test.sry
Loading module Cryptol
Loading module labs::Interpreter::test
3
```

## Usage options

The last few items covered here (and more) can be found querying Cryptol's usage options via:

```shell
$ cryptol --help
Usage: cryptol [OPTIONS]
  -b FILE     --batch=FILE             run the script provided and exit
  -e          --stop-on-error          stop script execution as soon as an error occurs.
  -c COMMAND  --command=COMMAND        run the given command and then exit; if multiple --command arguments are given, run them in the order they appear on the command line (overrides --batch)
              --color=MODE             control the color output for the terminal, which may be 'auto', 'none' or 'always' (default: 'auto')
  -v          --version                display version number
  -h          --help                   display this message
              --ignore-cryptolrc       disable reading of .cryptolrc files
              --cryptolrc-script=FILE  read additional .cryptolrc files
              --cryptolpath-only       only look for .cry files in CRYPTOLPATH; don't use built-in locations

Influential environment variables:
    CRYPTOLPATH
        A `:`-separated list of directories to be searched for Cryptol
        modules in addition to the default locations
    SBV_{ABC,BOOLECTOR,CVC4,MATHSAT,YICES,Z3}_OPTIONS
        A string of command-line arguments to be passed to the
        corresponding solver invoked for `:sat` and `:prove`
```

## End

That's all for the interpreter for now. This course is only designed
to be an introduction, so not everything is covered in detail. We
encourage you to explore more on your own, and if you have questions,
a great place to ask is the [cryptol-users mailing
list](https://groups.google.com/a/galois.com/forum/#!forum/cryptol-users).
