# Using the Cryptol interpreter

## Running the interpreter

To use Cryptol, from the linux command line type `cryptol` to get
this:

```
$ cryptol
┏━╸┏━┓╻ ╻┏━┓╺┳╸┏━┓╻  
┃  ┣┳┛┗┳┛┣━┛ ┃ ┃ ┃┃  
┗━╸╹┗╸ ╹ ╹   ╹ ┗━┛┗━╸
version 2.8.1 (e914cef)
https://cryptol.net  :? for help

Loading module Cryptol
Cryptol>
```

## Interpreter State

The interpreter has a number of different configuration settings. To view them type `:set`.

```
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

```
Cryptol> 0xa
0xa
Cryptol> 10 : [4]
0xa
```

To make the interpreter display bitvectors in decimal (base 10) type
`:set base=10`.

```
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

```
Cryptol> :h :set base

    base = 10

Default value: 16

The base to display words at (2, 8, 10, or 16).
```

Cryptol has a built-in command to reverse a list, called
`reverse`. Let's look at the help for this command.

```
Cryptol> :h reverse

    reverse : {n, a} (fin n) => [n]a -> [n]a

Reverses the elements in a sequence.
```

## Colon commands

You'll notice that some commands start with a colon (`:`) and others
do not. The colon commands are commands that are outside of the
Cryptol language, and only exist in the interpreter. You can see a full listing of these commands by typing `:h`.

```
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
`:e`, and reloading `:r` a file), quitting the interpreter `:q`, and asking for the type of an expression `:t`.

