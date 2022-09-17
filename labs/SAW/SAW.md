# Introduction

This is a tutorial aimed at training developers how to leverage the
[Software Analysis Workbench (SAW)](https://saw.galois.com) and
Cryptol to develop and verify cryptographic implementations in a
[Continuous Reasoning](https://dl.acm.org/doi/abs/10.1145/3209108.3209109)
paradigm. Continuous Reasoning is, roughly,

> formal reasoning about a (changing) codebase ... done in a fashion
> which mirrors the iterative, continuous model of software
> development that is increasingly practiced in industry.

SAW and Cryptol can be used by a Continuous Integration (CI) system to
enforce invariants (safety, security, and functional) that software
must have at certain stages in a software development pipeline. Some
industrial examples include [AWS's s2n](https://link.springer.com/chapter/10.1007/978-3-319-96142-2_26)
and [Supranational's BLST](https://github.com/GaloisInc/BLST-Verification).

## Prerequisites

Before working through this lab, you'll need
  * A recent version of Python, Cryptol, SAW, saw-remote-api, SAW's Python client package, and the clang C compiler to be installed and
  * an editor for completing the exercises in this file.

You'll also need experience with the following languages
  * Python
  * Cryptol
  * SAW
  * C

## Skills You'll Learn

By the end of this lab you will be comfortable using SAW's remote API
to formally verify cryptographic implementations. Since the bulk of
work with SAW lies in crafting linkages between the source language
and spec, this lab is filled with examples of different types of data
structures and how to represent them in SAW. Hence, this lab also
exists as a kind of reference that you may return to when using these
tools. As well, there are some examples
[here](https://github.com/GaloisInc/saw-script/tree/master/saw-remote-api/python/tests)
and a full-featured tutorial on SAW
[here](https://saw.galois.com/intro/).

# Why Use SAW?

In the previous labs, we learned how to look at algorithm specifications
from white papers and represent them in Cryptol. From there, we could
leverage tools in Cryptol to prove certain behaviors and ultimately write
up a coded representation of the algorithm.

While having a working specification of an algorithm in Cryptol is really 
awesome, it would be really really awesome to accurately represent that algorithm
in other programming language like C/C++ or Java. Translating our algorithms
into other languages gives us the chance to leverage strengths from those
languages such as improved runtimes and being more widely known to software
development teams.

Of course, we want to make sure that the code we write in those other languages
completely represents all of the cryptographic properties pertaining to our
target algorithm. We also want to uphold best coding practices for that language
and avoid vulnerabilities such as buffer or integer overflows. After all, what 
good is cryptography if an attacker can take advantage of an implementation flaw?

This is where the Software Analysis Workbench (SAW) comes in. SAW is a tool that
formally verifies properties of code using SAT and SMT solvers. SAW leverages
symbolic execution to translate code into formal models. Alright, that's a lot of
fancy words, but essentially SAW can verify that all inputs to a function/method
yield us our expected output. This makes SAW a much more powerful tool of testing
code compared to unit tests that can easily miss dangerous edge cases for a piece
of code.

In order to get SAW to work its magic, we need to tell it exactly what we are
looking to verify. For every function/method, we need to tell SAW its:
- Input(s)
- Output(s)
- Preconditions
- Postconditions


# Setting Everything Up

To run any of the examples in this lab, you need to first start the
Software Analysis Workbench (SAW) remote API (`saw-remote-api`).  If
you are using the development container that comes with this course
(`ghcr.io/weaversa/cryptol-course`), you can enter the following
command in your terminal:

```sh
$ start-saw-remote-api
```

Otherwise, this tool can be installed by following the instructions
for SAW found in the [Installation lab](../../INSTALL.md). Once
installed, to run `saw-remote-api`, enter the following commands into
your terminal:

```sh
$ export SAW_SERVER_URL=http://0.0.0.0:36691
$ saw-remote-api http --host 0.0.0.0 --port 36691 / &
```

Congrats! You now have a server version of SAW running in the
background ready to accept commands on port `36691`.

# SAW and Python

If you followed the instructions above, you now have SAW running in
the background, waiting for a connection on port `36691`. Galois
provides a [Python client package](https://pypi.org/project/saw-client/)
that allows you to write formal/logical contracts to enforce
invariants on some given software.

# Python Contracts Introduction

Here's the general layout for SAW in Python:

```python
# Import SAW's Python modules

class contractName(Contract):
  def specification(self):
    # Initialization and Preconditions
    # Execute Function
    # Initialization for output variables (optional)
    # Postconditions and Return

class testName(unittest.TestCase):
  def specificTestName(self):
    # Verify contracts

if __name__ == "__main__":
    unittest.main()
```

## Imports

To start making Python contracts first import the necessary files:

```python
import unittest
from saw_client             import *
from saw_client.crucible    import * 
from saw_client.llvm        import * 
from saw_client.proofscript import *
from saw_client.llvm_type   import * 
```

We put `*` for simplicity, but if one only wanted certain functions,
they could refer to this table for some common imports:

|Package | Values |
|---------------------|--------|
|`saw_client.crucible`| `cry`, `cry_f`|
|`saw_client.llvm`    | `Contract`, `CryptolTerm`, `SetupVal`, `FreshVar`, `i8`, `i32`, `i64`, `void`, `null`, `array_ty`, `field`, `struct`, `alias_ty` |
|`saw_client.llvm_type` | `LLVMType`, `LLVMArrayType` |

## Left Circular Shift Example

To see contracts in action we need an example. Here's some C code for
left circular shift we want to verify:

```C
uint32_t rotl(uint32_t bits, uint32_t shift) {
  return (bits << shift) | (bits >> (sizeof(bits) * 8 - shift));
}
```

In this example, SAW won't actually verify C source, but rather C
compiled down to LLVM intermediate representation (IR), or
bitcode. This can be accomplished via the `clang` compiler. In this
instance, we can create the bitcode by entering the following commands
in a terminal.

```sh
$ mkdir labs/SAW/rotl/artifacts
$ clang -emit-llvm labs/SAW/rotl/src/rotl1.c -c -o labs/SAW/rotl/artifacts/rotl.bc
```

We can inspect the bitcode, using SAW, by loading the module and
printing some meta-data. Note that when we run the following
example, we assume to be located in the `labs/SAW/rotl/` directory.

```Xsaw-session
$ saw
 ┏━━━┓━━━┓━┓━┓━┓
 ┃ ━━┓ ╻ ┃ ┃ ┃ ┃
 ┣━━ ┃ ╻ ┃┓ ╻ ┏┛
 ┗━━━┛━┛━┛┗━┛━┛ version 0.9.0.99 (<non-dev-build>)

sawscript> r <- llvm_load_module "artifacts/rotl.bc"
sawscript> print r
[00:30:35.338] Module: artifacts/rotl.bc
Types:

Globals:

External references:

Definitions:
  i32 @rotl(i32 %0, i32 %1)

```

The corresponding Cryptol specification for left circular shift is:

```cryptol
rotl : [32] -> [32] -> [32]
rotl xs shift = xs <<< shift
```

For the SAW Python API we make a `Contract` object with the required
`specification` function:

```python
class rotl_Contract(Contract):
  def specification(self):
    bits  = self.fresh_var(i32, "bits") 
    shift = self.fresh_var(i32, "shift")
    
    self.execute_func(bits, shift)

    self.returns_f("rotl {bits} {shift}")
```

Let's break down `specification` piece by piece.

### Fresh Symbolic Variables

The command `self.fresh_var(type, name)` creates a new symbolic
variable of type `type` and name `name`, where `name` is a
string. Names for fresh symbolic variables are not optional inputs,
though they mostly serve displaying names in counter-examples and
error messages. The string can be anything, but it makes sense to give
it the name of the variable being defined.

### Execute Functions

The command `self.execute_func(input1, input2, ...)` will symbolically
execute the function we're writing a contract for. There should be
exactly as many comma separated inputs as there are in the C
function. One can only place preconditions before this command and
postconditions after this command.

### Return Statements

The command `self.returns_f(string)` is a postcondition that asserts
the function returns a given Cryptol term (parsed from a Python
string). To use Python variables in scope within the string use
`{variable_name}`. For example,

|`self.`|`returns_f(`|`"rotl {bits} {shift}"`|)|
|-------|-----------|---------------------|----|
|In this contract| assert the current function returns the Cryptol term | left circular shift `bits` by `shift` |.|

Sometimes we don't want to return a Cryptol term. In those cases we
can just use `returns(someSetupValue)`. The specification function of
a Contract must **always** have a `self.returns(someSetupValue)` or
`self.returns_f(string)` statement. If the function returns `void`, one
can use `self.returns(void)`.

### Terms from Cryptol

The command `cry(string)` converts a Python string into a
`CryptolTerm` that can be used in SAW. The `cry_f(string)` command is
similar to `cry`, but the `_f` indicates one can pass Python local
variables into the strings. To do this, surround the variable with
braces as we did in `returns_f("{bits} >>> {shift}")`. In fact,
`returns_f` is just syntactic sugar for `returns(cry_f(string))`. In
general, `cry_f` and friends are mostly a wrapper around [formatted string literals](https://docs.python.org/3/tutorial/inputoutput.html#formatted-string-literals)
called "f-strings".

The `CryptolTerm` class is a subclass of `SetupVal`. This allows using
`CryptolTerm` as a `SetupVal`.

Braces are sometimes used in Cryptol to assign type parameters or
declare records.  The symbols `{{` and `}}` are used to denote literal
braces for these cases when parsing Python strings.  For example,
let's think about how to parse the following line:

{% raw %}
```python
  self.returns_f("{{a = take`{{5}} {var}, b = take`{{{N}}} {var} }} == foo `eel")
```
{% endraw %}

If `var` is a local Python variable equal to `23` and `N` is a
local Python variable equal to `2`, then the string parses in Cryptol as

```cryptol
{a = take`{5} 23, b = take`{2} 23} == foo `eel
```

where `foo` is some Cryptol function returning a record and `eel` is
some Cryptol type in the currently loaded specification.

## Unit Testing

```python
class rotlTest(unittest.TestCase):
  def test_rotl(self):
    connect(reset_server=True)
    if __name__ == "__main__": view(LogResults(verbose_failure=True))
    
    bcpath = "/some/path/to/your/file.bc"
    mod    = llvm_load_module(bcpath)
    
    crypath = "/some/path/to/your/file.cry"
    cryptol_load_file(crypath)
    
    rotl_result = llvm_verify(mod, 'rotl', rotl_Contract())
    self.assertIs(rotl_result.is_success(), True)
```

For a contract, the specification function should be called
`specification`. For tests, it doesn't matter what you name your
tests. Here we named it `test_rotl`. These tests will be run when you
run the Python program.

Let's break down the first few lines of this function:

- The command `connect(reset_server=True)` connects to the server so
  we can use the SAW Python API.
- The line `if __name__ == "__main__":
  view(LogResults(verbose_failure=True))` allows us to view the output
  with verbose error messages. If you don't want verbose error
  messages, then just use `if __name__ == "__main__":
  view(LogResults())`.
- The line `bcpath = "/some/path/to/your/file.bc"` declares the
  bitcode file to analyze. If there are multiple bitcode files,
  make a variable for each file.
- The line `mod = llvm_load_module(bcpath)` creates the object to
  pass to verification that represents the bitcode.
- The line `crypath = "/some/path/to/your/file.cry"` specifies the
  path to a Cryptol specification.
- The line `cryptol_load_file(crypath)` loads a Cryptol specification.

Now that the environment is set up, let's actually verify our
contract! This is done at the line

|`rotl_result =` | `llvm_verify(` | `mod,` | `'rotl',` | `rotl_Contract()`| `)`|
|----------|-----------|----|------|---------------|----|
|Assign this variable| to the result of trying to verify| the bitcode| function with this name| using this contract|.|

Now that we have the result, we want to assert this result succeeded
using `self.assertIs(rotl_result.is_success(), True)`.


## Debugging C with SAW

```python
from pathlib import Path
import unittest
from saw_client             import *
from saw_client.crucible    import * 
from saw_client.llvm        import * 
from saw_client.proofscript import *
from saw_client.llvm_type   import * 

class rotl_Contract(Contract):
  def specification(self):
    xs    = self.fresh_var(i32, "xs") 
    shift = self.fresh_var(i32, "shift")
    
    self.execute_func(xs, shift)

    self.returns_f("rotl {xs} {shift}")

class rotlTest(unittest.TestCase):
  def test_rotl(self):
    connect(reset_server=True)
    if __name__ == "__main__": view(LogResults(verbose_failure=True))
    
    basedir = Path(__file__).absolute().parents[1]
    bcpath  = basedir/"artifacts/rotl.bc"
    crypath = basedir/"specs/rotl.cry"

    cryptol_load_file(str(crypath))
    mod = llvm_load_module(str(bcpath))
    
    rotl_result = llvm_verify(mod, 'rotl', rotl_Contract())
    self.assertIs(rotl_result.is_success(), True)

if __name__ == "__main__":
    unittest.main()
```

Remember to always include 

```python
if __name__ == "__main__":
    unittest.main()
```

or else the python script won't do anything!

Note that we defined our bitcode and cryptol file paths (`bcpath` and 
`crypath`, respectively) relative to the rotl lab directory using 
`Path` from Python's [pathlib](https://docs.python.org/3/library/pathlib.html)
library.

```python
    basedir = Path(__file__).absolute().parents[1]
    bcpath  = basedir/"artifacts/rotl.bc"
    crypath = basedir/"specs/rotl.cry"

    cryptol_load_file(str(crypath))
    mod = llvm_load_module(str(bcpath))
```

Doing so gives us freedom to call the proof script from any initial
working directory. It also provides portability to multiple platforms
since we don't need to worry whether or not the operating system uses
`/` or `\` in paths. However, we must convert the path names to a
string so to satisfy SAW's API for file loads.

We can now run the proof script.

```sh
$ python3 proof/rotl.py
[03:08:29.986] Verifying rotl ...
[03:08:29.987] Simulating rotl ...
[03:08:29.988] Checking proof obligations rotl ...
[03:08:30.007] Subgoal failed: rotl safety assertion:
internal: error: in rotl
Undefined behavior encountered
Details:
  Poison value created
        The second operand of `shl` was equal to or greater than the number of bits in the first operand
        
[03:08:30.007] SolverStats {solverStatsSolvers = fromList ["W4 ->z3"], solverStatsGoalSize = 382}
[03:08:30.007] ----------Counterexample----------
[03:08:30.007]   shift0: 2147483648
[03:08:30.007] ----------------------------------

F
======================================================================
FAIL: test_rotl (__main__.rotlTest)
----------------------------------------------------------------------
Traceback (most recent call last):
  File "proof/rotl.py", line 31, in test_rotl
      self.assertIs(rotl_result.is_success(), True)
      AssertionError: False is not True
      
      ----------------------------------------------------------------------
      Ran 1 test in 0.750s
      
      FAILED (failures=1)
      🛑  The goal failed to verify.
```

SAW alerted us about potentially undefined behavior; the [C99 standard](https://www.open-std.org/jtc1/sc22/wg14/www/docs/n1256.pdf)
specifies the following about bit shifts:

> If the value of the right operand is negative or is greater than or
> equal to the width of the promoted left operand, the behavior is
> undefined.

As expected, this alerts us to a bug:

```
        The second operand of `shl` was equal to or greater than the number of bits in the first operand
        
[03:08:30.007] SolverStats {solverStatsSolvers = fromList ["W4 ->z3"], solverStatsGoalSize = 382}
[03:08:30.007] ----------Counterexample----------
[03:08:30.007]   shift0: 2147483648
[03:08:30.007] ----------------------------------
```

SAW also provides a handy counterexample, namely, when `shift =
2147483648` (clearly larger than 31), we encounter undefined behavior.

One remedy to this is the following:

```C
uint32_t rotl(uint32_t bits, uint32_t shift) {
  shift %= sizeof(bits) * 8;
  return (bits << shift) | (bits >> (sizeof(bits) * 8 - shift));
}
```

Recompiling and running SAW gives:

```sh
$ clang -c -emit-llvm -o artifacts/rotl.bc src/rotl2.c && python3 proof/rotl.py
[03:11:54.334] Verifying rotl ...
[03:11:54.334] Simulating rotl ...
[03:11:54.335] Checking proof obligations rotl ...
[03:11:54.351] Subgoal failed: rotl safety assertion:
internal: error: in rotl
Undefined behavior encountered
Details:
  Poison value created
        The second operand of `shl` was equal to or greater than the number of bits in the first operand
        
[03:11:54.351] SolverStats {solverStatsSolvers = fromList ["W4 ->z3"], solverStatsGoalSize = 388}
[03:11:54.351] ----------Counterexample----------
[03:11:54.351]   shift0: 0
[03:11:54.351] ----------------------------------

F
======================================================================
FAIL: test_rotl (__main__.rotlTest)
----------------------------------------------------------------------
Traceback (most recent call last):
  File "proof/rotl.py", line 31, in test_rotl
      self.assertIs(rotl_result.is_success(), True)
      AssertionError: False is not True
      
      ----------------------------------------------------------------------
      Ran 1 test in 0.723s
      
      FAILED (failures=1)
      🛑  The goal failed to verify.~
```

Aha! The counter example shows that we forgot about the case when
`shift = 0`! This causes `(sizeof(bits) * 8 - 0)` to be `32`,
which is equal to the word-size of `bits`, and hence causes `<<` to
exhibit undefined behavior.

Let's try again with

```C
uint32_t rotl(uint32_t bits, uint32_t shift) {
  shift %= sizeof(bits)*8;
  if(shift == 0) return bits;
  return (bits << shift) | (bits >> (sizeof(bits) * 8 - shift));
}
```

```sh
$ clang -c -emit-llvm -o artifacts/rotl.bc src/rotl.c && python3 proof/rotl.py
[03:14:09.561] Verifying rotl ...
[03:14:09.562] Simulating rotl ...
[03:14:09.563] Checking proof obligations rotl ...
[03:14:09.614] Proof succeeded! rotl
✅  Verified: lemma_rotl_Contract (defined at cryptol-course/labs/SAW/rotl/proof/rotl.py:30)
.
----------------------------------------------------------------------
Ran 1 test in 0.780s

OK
✅  The goal was verified!
```

Finally, SAW is happy. More importantly, the C is correct and free of
undefined behavior!

Note that we provide a Makefile for our SAW labs. If you
navigate to the target lab, you can run `make prove` to generate
the bitcode for the module and run the proof!

```sh
$ cd labs/SAW/rotl
$ make prove
mkdir -p artifacts
clang -c -g -emit-llvm -o artifacts/rotl.bc src/rotl.c
python3 proof/rotl.py
```

# Pointers and Arrays

We'll begin by writing a function that given two arrays of a common
fixed size, say five, adds the second to the first. One way to
accomplish this is to pass in the two arrays, mutate the first and
return nothing:

```C
void addRow5Mutate(uint32_t a[5], uint32_t b[5]) {
  for(int i  = 0 ; i < 5; i++) {
    a[i] += b[i];
  }
  return;
}
```

The corresponding Cryptol specification is:

```cryptol
addRow5 : [5][32] -> [5][32] -> [5][32]
addRow5 a b = a + b
```

## Initializing Arrays and Pointers

To initialize the arrays and pointers we'll use the `alloc` command
and `array_ty` type constructor:

```python
class addRow5Mutate_Contract(Contract):
  def specification(self):
    a   = self.fresh_var(array_ty(5, i32), "a")
    a_p = self.alloc(array_ty(5, i32), points_to=a) 
    b   = self.fresh_var(array_ty(5, i32), "b")
    b_p = self.alloc(array_ty(5, i32), points_to=b, read_only=True)
    
    self.execute_func(a_p, b_p)
    
    self.points_to(a_p, cry_f("addRow {a} {b}"))
    
    self.returns(void)
```

- The `array_ty(type, length)` command creates a type representing an
  array with entries of type `type` and length `length`.
- The `alloc` command will initialize a symbolic pointer. Let's break
  down the initialization of `b_p`:

| `b_p` | `self.alloc(` | `array_ty(5, i32)`|`,`|`points_to=b`|`,`|`read_only=True`|`)`|
|-------|---------------|-------------------|---|-------------|---|----------------|---|
| Assign this variable to | a symbolic pointer in the current contract | that has the following type | and | points to this object | with | read only permissions| . |

Since arrays are passed as pointers in C, when we call `execute_func`
we supply `a_p` and `b_p` rather than `a` and `b`.

To verify correctness, we assert after execution `a_p`
points to what the Cryptol specification claims it should:

| `self.points_to(` | `a_p,` | `cry_f( "addRow {a} {b}"` | `))` |
|-------------------|--------|---------------------------|------|
| Assert in the current contract that the pointer | with this name | points to this Cryptol term | . | 

Finally, `specification` must contain `self.returns` or `self.returns_f`, so we use `self.returns(void)`.

### Python Helper Functions

To limit code reuse we can define helper functions in Python. Code *reuse* 
is good. Code *repetition* is bad! For example, the following construct 
is often used:

```python
def ptr_to_fresh(c : Contract, ty : LLVMType, name : Optional[str] = None, read_only : Optional[bool] = False) -> Tuple[FreshVar, SetupVal]:
    var = c.fresh_var(ty, name)
    ptr = c.alloc(ty, points_to = var, read_only=read_only)
    return (var, ptr)
```

Given a contract and a type this function outputs a tuple `(var, ptr)`
where `var` is a fresh symbolic variable of the given type and `ptr`
is a pointer pointing to this variable. We give optional arguments to
name the fresh symbolic variable and to assert read-only pointer
constraints.

To see this in action, let's rewrite our previous contract:

```python
class addRow5Mutate_Contract(Contract):
  def specification(self):
    (a, a_p) = ptr_to_fresh(self, array_ty(5, i32), name="a")
    (b, b_p) = ptr_to_fresh(self, array_ty(5, i32), name="b", read_only=True)
    
    self.execute_func(a_p, b_p)
    
    self.points_to(a_p, cry_f("addRow {a} {b}"))
    
    self.returns(void)
```

## Auxiliary Variables

In this section we discuss an alternative way add two rows by using an auxillary variable. For example, consider the function

```C
uint32_t* addRow5NewVar(uint32_t a[5], uint32_t b[5]) {
  uint32_t* c = (uint32_t*) malloc(5*sizeof(uint32_t));
  for(int i  = 0 ; i < 5; i++) {
    c[i] = a[i] + b[i];
  }
  return c;
}
```

Recall that a SAW contract is strictly divided into three parts: 
1. Preconditions
2. Execution
3. Postconditions

SAW complains if you place a precondition after `execute_func` and
similarly for postcondition. If a function returns a value that was
not passed through `execute_func`, then you will have to initialize
new fresh symbolic variables. For example, consider the proposed
contract for `addRow5NewVar`:

```python
class addRow5NewVar_Contract(Contract):
  def specification(self):
    (a, a_p) = ptr_to_fresh(self, array_ty(5, i32), name="a")
    (b, b_p) = ptr_to_fresh(self, array_ty(5, i32), name="b", read_only=True)
    (c, c_p) = ptr_to_fresh(self, array_ty(5, i32), name="c")
    
    self.execute_func(a_p, b_p)
    
    self.points_to(c_p, cry_f("addRow {a} {b}"))
    
    self.returns(c_p)
```

Running a unit test yields the following error message:

```sh
[16:42:51.066] Subgoal failed: addRow5NewVar safety assertion:
internal: error: in _SAW_verify_prestate SAWServer
The following pointers had to alias, but they didn't:
  (12, 0x0:[64])
  (7, 0x0:[64])


[16:42:51.067] SolverStats {solverStatsSolvers = fromList ["W4 ->z3"], solverStatsGoalSize = 29}
[16:42:51.067] ----------Counterexample----------
[16:42:51.067] <<All settings of the symbolic variables constitute a counterexample>>
[16:42:51.067] ----------------------------------
```

Think about the precondition block, the part before `execute_func`, as
setting up the symbolic variables **before** they enter the
function. With this in mind, it doesn't make sense to declare `c_p` in
this block because `c_p` is defined **within** the C function. The fix
to the previous contract is moving the declaration of `c_p` to the
postcondition block:

```python
class addRow5NewVar_Contract(Contract):
  def specification(self):
    (a, a_p) = ptr_to_fresh(self, array_ty(5, i32), name="a")
    (b, b_p) = ptr_to_fresh(self, array_ty(5, i32), name="b", read_only=True)
    
    self.execute_func(a_p, b_p)
    
    (c, c_p) = ptr_to_fresh(self, array_ty(5, i32), name="c")
    self.points_to(c_p, cry_f("addRow {a} {b}"))
    
    self.returns(c_p)
```

Python supports wildcards, denoted by `_`, like Cryptol. Wildcards are placeholders for values we don't use. For example, we could rewrite the `addRow5NewVar_Contract` as follows: 

```python
class addRow5NewVar_Contract(Contract):
  def specification(self):
    (a, a_p) = ptr_to_fresh(self, array_ty(5, i32), name="a")
    (b, b_p) = ptr_to_fresh(self, array_ty(5, i32), name="b", read_only=True)
    
    self.execute_func(a_p, b_p)
    
    (_, c_p) = ptr_to_fresh(self, array_ty(5, i32), name="c")
    self.points_to(c_p, cry_f("addRow {a} {b}"))
    
    self.returns(c_p)
```

### Postconditions and `points_to`

One could replace the `points_to` line with a `postcondition_f` line to get an equivalent contract:

```python
class addRow5NewVar_Contract(Contract):
  def specification(self):
    (a, a_p) = ptr_to_fresh(self, array_ty(5, i32), name="a", read_only=True)
    (b, b_p) = ptr_to_fresh(self, array_ty(5, i32), name="b", read_only=True)
    
    self.execute_func(a_p, b_p)
    
    (c, c_p) = ptr_to_fresh(self, array_ty(5, i32), name="c")
    self.postcondition_f("{c} == addRow {a} {b}")
    
    self.returns(c_p)
```

While Cryptol has arrays, it doesn't have a notion of pointer, so we can't pass a symbolic pointer into `cry_f`. For example, if we wanted to assert `a_p` points to `b_p` then we can use `self.points_to(a_p, b_p)` but **NOT** `self.postcondition_f("{a_p} == {b_p}")`.

## Parameterized Contracts

Now let's consider a third possible way to write the same function. Here we are given two arrays of arbitrary size and a length.

```C
uint32_t* addRowAlias(uint32_t* a, uint32_t* b, uint8_t length) {
  for(int i  = 0 ; i < length; i++) {
    a[i] += b[i];
  }
  return a;
}
```

A corresponding Cryptol specification might is

```cryptol
addRow : {length} (fin length) => [length][32] -> [length][32] -> [length][32]
addRow a b = a + b
```

SAW currently does not have inductive reasoning capabilities and so
can only reason about concrete types. However, SAWCore terms can be
[formalized in Coq](https://github.com/GaloisInc/saw-script/tree/master/saw-core-coq),
a much more powerful theorem prover that has inductive reasoning (and
many more) capabilities. This is obviously well beyond the scope of
the course, but worth mentioning.

We could make a new contract for each value of `length` used in the C
code. Instead we make a single contract with a `length` parameter:

{% raw %}
```python
class addRowAlias_Contract(Contract):
  def __init__(self, length : int):
    super().__init__()
    self.length = length

  def specification(self):
    (a, a_p) = ptr_to_fresh(self, array_ty(self.length, i32), name="a")
    (b, b_p) = ptr_to_fresh(self, array_ty(self.length, i32), name="b", read_only=True)
    length   = fresh_var(i8, "length")
    
    self.execute_func(a_p, b_p, length)
    
    self.points_to(a_p, cry_f("addRow`{{{self.length}}} {a} {b}"))
    
    self.returns(a_p)
```
{% endraw %}

However, the above is not quite right just yet. As well we also need
to make a test for each individual length we desire to verify (say,
lengths 5 and 10):

```python
class ArrayTests(unittest.TestCase):
  def test_rowAdds(self):
    connect(reset_server=True)
    if __name__ == "__main__": view(LogResults(verbose_failure=True))
    
    bcpath  = "/some/path/to/your/file.bc"
    crypath = "/some/path/to/your/file.cry"
    
    cryptol_load_file(crypath)
    mod = llvm_load_module(bcpath)
    
    addRowAlias05_result = llvm_verify(mod, 'addRowAlias', addRowAlias_Contract(5))
    self.assertIs(addRowAlias05_result.is_success(), True)

    addRowAlias10_result = llvm_verify(mod, 'addRowAlias', addRowAlias_Contract(10))
    self.assertIs(addRowAlias10_result.is_success(), True)
```

## Full Code Example and Debugging SAW

{% raw %}
```python
from pathlib import Path
import unittest
from saw_client             import *
from saw_client.crucible    import * 
from saw_client.llvm        import * 
from saw_client.proofscript import *
from saw_client.llvm_type   import * 

def ptr_to_fresh(c : Contract, ty : LLVMType, name : Optional[str] = None, read_only : Optional[bool] = False) -> Tuple[FreshVar, SetupVal]:
    var = c.fresh_var(ty, name)
    ptr = c.alloc(ty, points_to = var, read_only=read_only)
    return (var, ptr)
    
class addRow5Mutate_Contract(Contract):
  def specification(self):
    (a, a_p) = ptr_to_fresh(self, array_ty(5, i32), name="a")
    (b, b_p) = ptr_to_fresh(self, array_ty(5, i32), name="b", read_only=True)
    
    self.execute_func(a_p, b_p)
    
    self.points_to(a_p, cry_f("addRow {a} {b}"))
    
    self.returns(void)

class addRow5NewVar_Contract(Contract):
  def specification(self):
    (a, a_p) = ptr_to_fresh(self, array_ty(5, i32), name="a")
    (b, b_p) = ptr_to_fresh(self, array_ty(5, i32), name="b", read_only=True)
    
    self.execute_func(a_p, b_p)
    
    (c, c_p) = ptr_to_fresh(self, array_ty(5, i32), name="c")
    self.points_to(c_p, cry_f("addRow {a} {b}"))
    
    self.returns(c_p)

class addRowAlias_Contract(Contract):
  def __init__(self, length : int):
    super().__init__()
    self.length = length

  def specification(self):
    (a, a_p) = ptr_to_fresh(self, array_ty(self.length, i32), name="a")
    (b, b_p) = ptr_to_fresh(self, array_ty(self.length, i32), name="b", read_only=True)
    length   = fresh_var(i8, "length")
    
    self.execute_func(a_p, b_p, length)
    
    self.points_to(a_p, cry_f("addRow`{{{self.length}}} {a} {b}"))

    self.returns(a_p)

class ArrayTests(unittest.TestCase):
  def test_rowAdds(self):
    connect(reset_server=True)
    if __name__ == "__main__": view(LogResults(verbose_failure=True))
    
    basedir = Path(__file__).absolute().parents[1]
    bcpath  = basedir/"artifacts/addRow.bc"
    crypath = basedir/"specs/addRow.cry"
    
    cryptol_load_file(str(crypath))
    mod = llvm_load_module(str(bcpath))
    
    addRow5Mutate_result = llvm_verify(mod, 'addRowMutate', addRowMutate_Contract())
    self.assertIs(addRow5Mutate_result.is_success(), True)
    
    addRow5NewVar_result = llvm_verify(mod, 'addRowNewVar', addRowNewVar_Contract())
    self.assertIs(addRow5NewVar_result.is_success(), True)
    
    addRowAlias05_result = llvm_verify(mod, 'addRowAlias', addRowAlias_Contract(5))
    self.assertIs(addRowAlias05_result.is_success(), True)
    
    addAddAlias10_result = llvm_verify(mod, 'addRowAlias', addRowAlias_Contract(10))
    self.assertIs(addRowAlias10_result.is_success(), True)
```
{% endraw %}

What do you think will happen if we run this code?

```sh
$ make prove -C labs/SAW/addRow
make: Entering directory '/workspace/cryptol-course/labs/SAW/addRow'
mkdir -p artifacts
clang -c -g -emit-llvm -o artifacts/addRow.bc src/addRow.c
python3 proof/addRow.py
[15:40:51.330] Verifying addRow5Mutate ...
[15:40:51.330] Simulating addRow5Mutate ...
[15:40:51.335] Checking proof obligations addRow5Mutate ...
[15:40:51.362] Proof succeeded! addRow5Mutate
✅  Verified: lemma_addRow5Mutate_Contract (defined at /home/cryptol/cryptol-course/labs/SAW/addRow/proof/addRow.py:64)
[15:40:51.430] Verifying addRow5NewVar ...
[15:40:51.430] Simulating addRow5NewVar ...
[15:40:51.435] Checking proof obligations addRow5NewVar ...
[15:40:51.462] Proof succeeded! addRow5NewVar
✅  Verified: lemma_addRow5NewVar_Contract (defined at /home/cryptol/cryptol-course/labs/SAW/addRow/proof/addRow.py:67)
```

SAW verifies the first two contracts but fails to verify the third
contract. It alerts us there is a memory error:

```sh
[15:40:51.527] Verifying addRowAlias ...
[15:40:51.528] Simulating addRowAlias ...
[15:40:51.532] Symbolic simulation completed with side conditions.
[15:40:51.535] Checking proof obligations addRowAlias ...
[15:40:51.575] Subgoal failed: addRowAlias safety assertion:
internal: error: in addRowAlias
Error during memory load
```

and even produces the following counterexample:

```sh
[15:40:51.575] SolverStats {solverStatsSolvers = fromList ["W4 ->z3"], solverStatsGoalSize = 331}
[15:40:51.575] ----------Counterexample----------
[15:40:51.575]   length0: 6
[15:40:51.575]   : False
[15:40:51.575] ----------------------------------
⚠️  Failed to verify: lemma_addRowAlias_Contract (defined at /home/cryptol/cryptol-course/labs/SAW/addRow/proof/addRow.py:38):
error: Proof failed.
      stdout:
              [15:40:51.527] Verifying addRowAlias ...
              [15:40:51.528] Simulating addRowAlias ...
              [15:40:51.532] Symbolic simulation completed with side conditions.
              [15:40:51.535] Checking proof obligations addRowAlias ...
              [15:40:51.575] Subgoal failed: addRowAlias safety assertion:
              internal: error: in addRowAlias
              Error during memory load

              [15:40:51.575] SolverStats {solverStatsSolvers = fromList ["W4 ->z3"], solverStatsGoalSize = 331}
              [15:40:51.575] ----------Counterexample----------
              [15:40:51.575]   length0: 6
              [15:40:51.575]   : False
              [15:40:51.575] ----------------------------------
F
======================================================================
FAIL: test_rowAdds (__main__.ArrayTests)
----------------------------------------------------------------------
Traceback (most recent call last):
File "proof/addRow.py", line 71, in test_rowAdds
  self.assertIs(addRowAlias05_result.is_success(), True)
AssertionError: False is not True

----------------------------------------------------------------------
Ran 1 test in 1.735s

FAILED (failures=1)
🛑  1 out of 3 goals failed to verify.
```
  
SAW is telling us we forgot to add a precondition to assert our
symbolic `length` agrees with our Python parameter `self.length`. This
is an easy fix:

{% raw %}
```python
class addRowAlias_Contract(Contract):
  def __init__(self, length : int):
    super().__init__()
    self.length = length

  def specification(self):
    (a, a_p) = ptr_to_fresh(self, array_ty(self.length, i32), name="a")
    (b, b_p) = ptr_to_fresh(self, array_ty(self.length, i32), name="b", read_only=True)
    length   = self.fresh_var(i8, "length")

    self.precondition_f("{length} == {self.length}")

    self.execute_func(a_p, b_p, length)
    
    self.points_to(a_p, cry_f("addRow`{{{self.length}}} {a} {b}"))

    self.returns(a_p)
```
{% endraw %}

And now SAW happily verifies the third and fourth contracts!
  
```sh
$ python3 addRow.py
✅  Verified: lemma_addRow5Mutate_Contract (defined at /home/cryptol/cryptol-course/labs/SAW/proof/addRow.py:64)
✅  Verified: lemma_addRow5NewVar_Contract (defined at /home/cryptol/cryptol-course/labs/SAW/proof/addRow.py:67)
✅  Verified: lemma_addRowAlias_Contract (defined at /home/cryptol/cryptol-course/labs/SAW/proof/addRow.py:38)
✅  Verified: lemma_addRowAlias_Contract (defined at /home/cryptol/cryptol-course/labs/SAW/proof/addRow.py:38)
.
----------------------------------------------------------------------
Ran 1 test in 1.115s

OK
✅  All 4 goals verified!
```

Note that we can be even more efficient in our proof. Instead of
creating a symbolic `length` variable, we can just use the Python
parameter `self.length`. Doing so will simplify the solver's proof
workload as it really only needs to consider one concrete value
rather than one value from a range of random symbolic ones.

```python
class addRowAlias_Contract(Contract):
  def __init__(self, length : int):
    super().__init__()
    self.length = length

  def specification(self):
    (a, a_p) = ptr_to_fresh(self, array_ty(self.length, i32), name="a")
    (b, b_p) = ptr_to_fresh(self, array_ty(self.length, i32), name="b", read_only=True)
    
    self.execute_func(a_p, b_p, cry(self.length))
    
    self.points_to(a_p, cry_f("addRow`{{{self.length}}} {a} {b}"))
    
    self.returns(a_p)
```

## Explicit Arrays

Another way to initialize arrays is through the `array` command. For
example, suppose we have the following C function (taken from
[here](https://github.com/GaloisInc/saw-script/blob/337ca6c9edff3bcbcd6924289471abd5ee714c82/saw-remote-api/python/tests/saw/test-files/llvm_array_swap.c)):

```C
#include <stdint.h>

void array_swap(uint32_t a[2]) {
    uint32_t tmp = a[0];
    a[0] = a[1];
    a[1] = tmp;
}
```

Here's a contract that doesn't even require Cryptol (taken from
[here](https://github.com/GaloisInc/saw-script/blob/337ca6c9edff3bcbcd6924289471abd5ee714c82/saw-remote-api/python/tests/saw/test_llvm_array_swap.py)):

```python
class ArraySwapContract(Contract):
    def specification(self):
        a0 = self.fresh_var(i32, "a0")
        a1 = self.fresh_var(i32, "a1")
        a  = self.alloc(array_ty(2, i32), points_to=array(a0, a1))

        self.execute_func(a)

        self.points_to(a[0], a1)
        self.points_to(a[1], a0)
        self.returns(void)
```

Explicit arrays can be useful when you want to assert a condition on a
particular element of the array. Of course, a drawback is you have to
initialize every member of the array. This can be problematic
for large arrays. Ideally, one would just have to declare the array
implicitly as before and then pass this array to a Cryptol
specification for postconditions as was done in the previous examples.

## Null Pointers

Consider the following C code to check if a pointer is null:

```C
int f(int *x) {
    return (x == (int*)0);
}
```

The following contract has possibly unexpected behavior (in C booleans are encoded as `0` for false and `1` for true).

```python
from pathlib import Path
import unittest
from saw_client          import *
from saw_client.crucible import *
from saw_client.llvm     import *


class FContract(Contract):
    def specification(self):
        p = self.alloc(i32)

        self.execute_func(p)

        self.returns(cry("1 : [32]"))

class LLVMAssertNullTest(unittest.TestCase):
    def test_llvm_assert_null(self):
        connect(reset_server=True)
        if __name__ == "__main__": view(LogResults(verbose_failure=True))

        basedir = Path(__file__).absolute().parents[1]
        bcpath  = basedir/"artifacts/null.bc"
        mod     = llvm_load_module(str(bcpath))

        result = llvm_verify(mod, 'f', FContract())
        self.assertIs(result.is_success(), True)


if __name__ == "__main__":
    unittest.main()
```

It turns out the contract above will fail!
  
```sh
$ make prove -C labs/SAW/null
make: Entering directory '/workspace/cryptol-course/labs/SAW/null'
mkdir -p artifacts
clang -c -g -emit-llvm -o artifacts/null.bc src/null.c
python3 proof/null.py
[17:21:44.701] Verifying isNull ...
[17:21:44.701] Simulating isNull ...
[17:21:44.703] Checking proof obligations isNull ...
[17:21:44.724] Subgoal failed: isNull safety assertion:
internal: error: in _SAW_verify_prestate SAWServer
Literal equality postcondition
[more error message]
FAILED (failures=1)
🛑  The goal failed to verify.
```

The counterexample produced may seem mystical.
```sh
[17:21:44.725] ----------Counterexample----------
[17:21:44.725] <<All settings of the symbolic variables constitute a counterexample>>
[17:21:44.725] ----------------------------------
```

However, if **every** setting is a counterexample, then this is telling us the pointer must have been the null pointer! An initialized symbolic pointer that hasn't been assigned a symbolic variable to point to is **NOT** equivalent to a null pointer in SAW. We can use `null()` in situations where we want a null pointer. For example, if we change the contract above to

```python
class FContract(Contract):
  def specification(self):
  
      self.execute_func(null())

      self.returns(cry("1 : [32]"))
```

then SAW is happy.

```sh
$ python3 null.py
[17:33:50.802] Verifying isNull ...
[17:33:50.802] Simulating isNull ...
[17:33:50.804] Checking proof obligations isNull ...
[17:33:50.804] Proof succeeded! isNull
✅  Verified: lemma_isNull_Contract (defined at proof/null.py:22)
.
----------------------------------------------------------------------
Ran 1 test in 1.301s

OK
✅  The goal was verified!
```

This example is from the [GaloisInc/saw-script repo](https://github.com/GaloisInc/saw-script/blob/master/saw-remote-api/python/tests/saw/test_llvm_assert_null.py).


# Structs 

In this section we will learn how to verify code involving structs by analyzing 
a game. The code for the game can be found 
[here](https://github.com/weaversa/cryptol-course/tree/master/labs/SAW/Game/src).

To complete this lab, navigate to the 
[Game directory](https://github.com/weaversa/cryptol-course/tree/master/labs/SAW/Game). 
In there, you'll notice the following:
- `Makefile`: Provides the necessary steps to generate our bitcode and run our 
SAW Python scripts.
- `src/`: Contains the source code we'll be analyzing.
- `proof/`: Contains our Python scripts to run our SAW contracts. Your job will 
be to complete the `TODO` sections marked throughout 
[Game.py](https://github.com/weaversa/cryptol-course/blob/master/labs/SAW/Game/proof/Game.py). 
If you get stuck, you can refer to 
[Game_answers.py](https://github.com/weaversa/cryptol-course/blob/master/labs/SAW/Game/proof/Game_answers.py) 
or look at the discussions mentioned later in this markdown file!
- `specs/`: Contains our Cryptol specs that our SAW contracts can call. Feel 
free to add your own Cryptol functions in 
[Game.cry](https://github.com/weaversa/cryptol-course/blob/master/labs/SAW/Game/specs/Game.cry) 
to help you complete this lab!
- `DLC/`: Contains an extended version of this lab (think Downloadable Content) 
with even more Game functions for you to play with! While there aren't any lab 
worksheets included in there, you can reference the contents to learn how to 
tackle additional functions. For more information regarding to what each 
function intends to teach, refer to [GameDLC.md](./Game/DLC/GameDLC.md).

With that knowledge, make sure you have `start-saw-remote-api` running, open up 
Game.py, fill out your answers, and test your work by running `make prove`. 
Game on!


## Struct Initialization

The game defines the following `sprite_t` and `character_t` types.

```C
#define MAX_NAME_LENGTH 12
#define GAITS 2
#define DIRECTIONS 4
#define ANIMATION_STEPS 3

typedef struct {
  uint8_t frames[GAITS][DIRECTIONS][ANIMATION_STEPS];
  uint32_t xPos;
  uint32_t yPos;
} sprite_t;

typedef struct {
  uint8_t name[MAX_NAME_LENGTH];
  uint32_t level;
  uint32_t hp;
  uint32_t atk;
  uint32_t def;
  uint32_t spd;
  sprite_t* sprite;
} character_t;

typedef character_t player_t;
```

For now, let's focus on the `player_t` type. Note that `player_t` is just an 
alias for `character_t`. Let's consider the function, `initDefaultPlayer`, 
that initializes a player with some default values.

```C
player_t* initDefaultPlayer()
{
  player_t* player = (player_t*) malloc(sizeof(player_t));

  uint8_t i = 0;
  uint32_t hp_default  = 10;
  uint32_t atk_default = 5;
  uint32_t def_default = 4;
  uint32_t spd_default = 3;

  for (i = 0; i < MAX_NAME_LENGTH; i++)
  {
    player->name[i] = 'A';
  }
  player->level = 1;
  player->hp  = hp_default;
  player->atk = atk_default;
  player->def = def_default;
  player->spd = spd_default;

  player->sprite = NULL;

  return player;
}
```

Observing `initDefaultPlayer`, we can see that the function first allocates 
memory for a `player_t` struct called `player`. The function then assigns a 
value to each of `player`'s fields. When it comes to the `sprite` field, the 
function sets that value to `NULL`. The `Game` library assumes that a 
character's sprite (and by extension, player sprites too) can only be set 
once. Given that the only information known to the game is to initialize a 
default player, it does not yet know what sprite information it should use. 
The game will rely on a separate function call to initialize the `sprite` field
when more information is known later.

We can craft a contract to verify this initialization function like so:

```python
MAX_NAME_LENGTH = 12

class initDefaultPlayer_Contract(Contract):
  def specification (self):

    self.execute_func()

    player = self.alloc(alias_ty("struct.character_t"))

    self.points_to(player['name'], cry_f("repeat 0x41 : [{MAX_NAME_LENGTH}][8]"))
    self.points_to(player['level'], cry_f("1 : [32]"))
    self.points_to(player['hp'], cry_f("10 : [32]"))
    self.points_to(player['atk'], cry_f("5 : [32]"))
    self.points_to(player['def'], cry_f("4 : [32]"))
    self.points_to(player['spd'], cry_f("3 : [32]"))
    self.points_to(player['sprite'], null())

    self.returns(player)
```

Notice that our contract immediately goes into the *execute state*. There isn't a
need to define any variables in the *initial state* since there aren't any inputs
passed to `initDefaultPlayer`. Although the function does allocate memory for 
`player`, that is done within the function, and passed as a return value. As
such, we represent that allocation in the contract's *final state*.

For every C symbol defined using `#define` in the source code, we make a 
corresponding Python global variable. At this point, we only care about
`MAX_NAME_LENGTH` since that is the only constant `initDefaultPlayer`
references.

The command `alias_ty("struct.<typedef name>")` creates a type corresponding 
to the structure. In our contract, 

```python
player = self.alloc(alias_ty("struct.character_t"))
```
creates a symbolic pointer variable, `player`, pointing to a structure of type 
`character_t`. Since `player_t` is an alias for `character_t`, we need to use
the base struct name, `character_t`. If we instead defined our `player` 
symbolic pointer as

```python
player = self.alloc(alias_ty("struct.player_t"))
```

SAW would throw an error saying that `player_t` is an unknown identifier:

```sh
error: unsupported type: %struct.player_t
Details:
Unknown type alias Ident "struct.player_t"
```

It is not SAW's fault that `player_t` is an unknown type alias. The issue here 
is that knowledge of `player_t` isn't maintained when the C source code is 
compiled to LLVM bitcode. To illustrate this point, let's take a look at what
is contained in `Game.bc`:

```sh
$ cd labs/SAW/Game
$ make
mkdir -p artifacts
clang -c -g -emit-llvm -o artifacts/Game.bc src/Game.c
$ saw
sawscript> r <- llvm_load_module "artifacts/Game.bc"
sawscript> print r
[00:24:06.901] Module: artifacts/Game.bc
Types:
  %struct.character_t = type { [12 x i8], i32, i32, i32, i32, i32,
                               %struct.sprite_t* }
  %struct.sprite_t = type { [2 x [4 x [3 x i8]]], i32, i32 }

Globals:

External references:
  declare default void @llvm.dbg.declare(metadata, metadata,
                                         metadata)
  declare default i8* @malloc(i64)

Definitions:
  %struct.character_t* @initDefaultPlayer()
  i32 @initDefaultSprite(%struct.character_t* %0)
  void @resolveAttack(%struct.character_t* %0, i32 %1)
  i32 @checkStats(%struct.character_t* %0)
```

Even though `Game.h` contains:

```C
typedef character_t player_t;
```

the generated bitcode does not keep this information. In fact, the function 
prototype for `initDefaultPlayer()` in `Game.h` is: 

```C
player_t* initDefaultPlayer();
```

However, as we saw in above in the above LLVM bitcode, the definition for 
`initDefaultPlayer()` has the return type `%struct.character_t*`.

Remember that SAW evaluates **bitcode** during Formal Verification. If the
information isn't contained in the bitcode, it isn't known to SAW.

Let's continue and breakdown one of the contract's `points_to` command:


| `self.points_to(` | `player` | `['name']` | `,` | `cry_f("repeat 0x41 : [{MAX_NAME_LENGTH}][8]")` | `)` |
|-------------------|-----------|----------|------|-------------------------------------------------|-----|
| Assert in the current contract that the following pointer | with this name | points to a struct with this named field | and the value of that field is | this expression | . |

Above, we use strings to reference fields of structures. However, we can only do
this when strings are present in the bitcode. In other words, when debug 
symbols are included in the generated bitcode. The `-g` clang flag tells the 
compiler to include the field names of the structs in the bitcode. For the 
full compilation details, check out the Makefile template, 
[prove.mk](https://github.com/weaversa/cryptol-course/blob/master/labs/SAW/prove.mk), 
which is used for all of our SAW labs. 

If we didn't have the debug symbols in the bitcode, SAW would produce an error:

```sh
$ cd labs/SAW/Game
$ mkdir artifacts
$ clang -c -emit-llvm -o artifacts/Game.bc src/Game.c
$ python3 proof/Game.py
⚠️  Failed to verify: lemma_initDefaultPlayer_Contract (defined at proof/Game.py:513):
error: Unable to resolve struct field name: '"name"'
Could not resolve setup value debug information into a struct type.
Perhaps you need to compile with debug symbols enabled.
```

If the bitcode lacked debug symbols, then could instead reference the struct 
fields by using their corresponding indices:

```python
class initDefaultPlayer_Contract(Contract):
  def specification (self):
    self.execute_func(player)

    self.points_to(player[0], cry_f("repeat 0x41 : [{MAX_NAME_LENGTH}][8]"))
    self.points_to(player[1], cry_f("1 : [32]"))
    self.points_to(player[2], cry_f("10 : [32]"))
    self.points_to(player[3], cry_f("5 : [32]"))
    self.points_to(player[4], cry_f("4 : [32]"))
    self.points_to(player[5], cry_f("3 : [32]"))
    self.points_to(player[6], null())
    
    self.returns(player)
```


## Explicit Structs

We can also explicitly define a struct in SAW. Let's go back and consider the 
`sprite_t` struct:

```C
#define GAITS 2
#define DIRECTIONS 4
#define ANIMATION_STEPS 3

typedef struct {
  uint8_t frames[GAITS][DIRECTIONS][ANIMATION_STEPS];
  uint32_t xPos;
  uint32_t yPos;
} sprite_t;
```

The idea behind the `sprite_t` struct is to hold all of the sprites associated 
with a character that we will present in our game. `frames` is a 3D `uint8_t` 
array where each element represents an asset identifier from our art collection.
Why a 3D array? Well, we want to provide animations for our characters that way 
it looks like they are moving on the screen (and it's a great excuse to discuss 
multi-dimensional arrays in SAW). 
- The first dimension refers to the number of 
gaits we want to represent, that being walking and running. 
- The second dimension 
refers to the number of directions a character can face. Imagine that we are 
working with a 2D top down game, so we have 4 directions: up, down, left, and 
right. 
- The third dimension refers to the number of frames per movement.

Let's think about how we walk forward (feel free to try this at home). If you 
are walking forward, you first stand up, move one foot in front of the other, 
place that foot down, lift up your back foot, and move that back foot ahead of 
your front foot. Rinse and repeat, and you'll be walking 'cross the floor.

Now that's a lot of steps! We can simplify this process by only considering 3 
frames: standing up, left foot in front, and right foot in front. We can then 
reuse the standing up frame as a transition between the left foot forward and 
the right foot forward frames.

Alright, that's enough of a crash course in animation. Let's get back to our 
struct. We have two more fields: `xPos` and `yPos`. These are simply positional 
references for where a character is located relative to the screen.

With that understanding, let's consider a function that uses the `sprite_t` struct:

```C
uint32_t initDefaultSprite(character_t* character)
{
  if (character == NULL)
  {
    return FAILURE;
  }
  else if (character->sprite != NULL)
  {
    return FAILURE;
  }

  sprite_t* sprite = (sprite_t*) malloc(sizeof(sprite_t));

  for (uint8_t i = 0; i < GAITS; i++)
  {
    for (uint8_t j = 0; j < DIRECTIONS; j++)
    {
      for (uint8_t k = 0; k < ANIMATION_STEPS; k++)
      {
        sprite->frames[i][j][k] = 0x00;
      }
    }
  }

  sprite->xPos = 1;
  sprite->yPos = 2;

  character->sprite = sprite;

  return SUCCESS;
}
```

This function uses two more constants, `SUCCESS` and `FAILURE`, which are 
defined as:

```C
#define SUCCESS 170
#define FAILURE 85
```

Those are some strange looking values, huh? Normally we would want to represent
successes and failures as booleans `TRUE` and `FALSE`, so `1` and `0` 
respectively right? Well, we could but the 
[Hamming Distance](https://en.wikipedia.org/wiki/Hamming_distance) 
between those values is just 1. This means a single bit flip could turn a 
`SUCCESS` into `FAILURE` and vice versa. Software tends to rely on the validity
of status codes for both functionality and security reasons. If we want to
increase our confidence that these status values are what they should be, we 
should increase the Hamming Distance to better differentiate our statuses.

| Decimal Value | Hex Representation | Binary Representation |
|:-------------:|:------------------:|:---------------------:|
| 170           | 0xAA               | 10101010              |
| 85            | 0x55               | 01010101              |

We can see that 170 and 85 have a Hamming Distance of 8. Take that bit flips!
As a side note, keep in mind that the constants `SUCCESS` and `FAILURE` are
actually 32-bit integers in C. While we could further increase the Hamming
Distance between the two numbers, we wanted to show a simplified proof of 
concept here. Alright, let's go back to understanding `initDefaultSprite`.

The function first performs input checking on `character` to ensure that it is 
not a `NULL` pointer. After all, it would be a shame to perform a `NULL`
dereference. The function then checks `character`'s `sprite` field to uphold 
the `Game` library's assumption that character sprites can only be set once. 
If any of these checks fail, `initDefaultSprite` immediately terminates and 
returns `FAILURE`. Otherwise, the function continues to do its job in 
allocating memory for a `sprite_t` struct and initializing the `sprite` 
fields for `character`.

Now, let's go ahead and make a contract to represent this function:

```python
SUCCESS         = 170
FAILURE         = 85
GAITS           = 2
DIRECTIONS      = 4
ANIMATION_STEPS = 3

class initDefaultSprite_Contract(Contract):
  def specification (self):
    name        = self.fresh_var(array_ty(MAX_NAME_LENGTH, i8), "character.name")
    level       = self.fresh_var(i32, "character.level")
    hp          = self.fresh_var(i32, "character.hp")
    atk         = self.fresh_var(i32, "character.atk")
    defense     = self.fresh_var(i32, "character.def")
    spd         = self.fresh_var(i32, "character.spd")
    character_p = self.alloc( alias_ty("struct.character_t")
                            , points_to = struct( name
                                                , level
                                                , hp
                                                , atk
                                                , defense
                                                , spd
                                                , null() ))

    self.execute_func(character_p)

    sprite_p = self.alloc( alias_ty("struct.sprite_t")
                         , points_to = struct( cry_f("zero : [{GAITS}][{DIRECTIONS}][{ANIMATION_STEPS}][8]")
                                             , cry_f("1 : [32]")
                                             , cry_f("2 : [32]") ))

    self.points_to(character_p, struct( name
                                      , level
                                      , hp
                                      , atk
                                      , defense
                                      , spd
                                      , sprite_p ))

    self.returns_f("`({SUCCESS}) : [32]")
```

There's quite a bit going on with the three uses of the `struct` keyword.
Let's look at them one at a time. Like `array`, the `struct` keyword 
declares a symbolic struct given a variable for each struct field.

The First `struct` Instance:

```python
    character_p = self.alloc( alias_ty("struct.character_t")
                            , points_to = struct( name
                                                , level
                                                , hp
                                                , atk
                                                , defense
                                                , spd
                                                , null() ))
```

For `character_p`, we assert the precondition that our pointer points to this 
symbolic struct containing our fresh variables `name`, `level`, `hp`, `atk`, 
`defense`, `spd`, and `null()`. Notice that we don't need to allocate memory
yet in the contract's *initial state* for a `sprite_t` struct since the function
assumes the passed `character` does not have an allocated sprite. As a result,
we match the function's assumption that the `sprite` field is `NULL`.

The Second `struct` Instance:

```python
    sprite_p = self.alloc( alias_ty("struct.sprite_t")
                         , points_to = struct( cry_f("zero : [{GAITS}][{DIRECTIONS}][{ANIMATION_STEPS}][8]")
                                             , cry_f("1 : [32]")
                                             , cry_f("2 : [32]") ))
```

After the call to symbolically execute the function, we allocate memory for
a symbolic `sprite_t` struct. In the same line, we also set a postcondition
using `struct` to assert how the `sprite` fields should look according to
`initDefaultSprite`.

The Third `struct` Instance:

```python
    self.points_to(character_p, struct( name
                                      , level
                                      , hp
                                      , atk
                                      , defense
                                      , spd
                                      , sprite_p ))
```
Now that `sprite_p` has been allocated and its field values were asserted, we
connect `sprite_p` to `character_p`. The other fields for `character` do not
change in the function, so we can resuse the symbolic variables that were 
defined in the contract's *initial state*.

As we can see, the `struct` keyword is quite versatile as we can pass symbolic
variables, symbolic pointers, and even define specific Cryptol values to it.

### Structs as Cryptol Tuples

Explicit structs is not the only way to represent values for preconditions and
postconditions. We can also use tuples since Cryptol interprets symbolic 
structs as tuples.

For example, we could rewrite the second `struct` instance explored above as:

```python
sprite_p = self.alloc( alias_ty("struct.sprite_t")
                     , points_to = cry_f("""( zero : [{GAITS}][{DIRECTIONS}][{ANIMATION_STEPS}][8]
                                            , 1 : [32]
                                            , 2 : [32] )"""))
```

Notice that we use 3 double quotes `"""` in our `cry_f` call. This technique is
useful when we want to separate our expected Cryptol-defined behaviors over 
multiple lines to improve code readability. Python considers the 3 double 
quotes as a multiline string. Multiline strings may also be used as block 
comments in Python.

However, the setup we see above results in wide open spaces, which will be 
noticable when debugging strings passed to the family of `cry` functions. These 
spaces can be mitigated using `dedent` from the `textwrap` package that comes 
with Python. For example:

```python
from textwrap import dedent
# ...

    self.returns(cry(dedent("""
      (y, z)
        where
          y = "foo"
          z = "bar"
    """).strip()))
```

This renders (without leading/trailing whitespace) as:

```python
(y, z)
  where
    y = "foo"
    z = "bar"
```


### Structs as Cryptol Records

While Cryptol's record types could also represent structs, SAW does
not currently support translating Cryptol's record types into
crucible-llvm's type system. If we tried to represent the struct as a
Cryptol record:

{% raw %}
```python
sprite_p = self.alloc( alias_ty("struct.sprite_t")
                     , points_to = cry_f("""{{ frames = zero : [{GAITS}][{DIRECTIONS}][{ANIMATION_STEPS}][8]
                                             , xPos   = 1 : [32]
                                             , yPos   = 2 : [32]
                                            }}
                                         """))
```
{% endraw %}

SAW would return this error:

```sh
⚠️  Failed to verify: lemma_initDefaultSprite_Contract (defined at proof/Game.py:525):
error: SAW doesn't yet support translating Cryptol's record type(s) into crucible-llvm's type system.
```

If a Cryptol specification uses a record type to represent structs, then we can
define a Python helper function for wrapping. 


## Exercise: Resolving an Attack!

Feeling pretty confident with our little `player_t` and `character_t` structs? How about we go for a full on attack then? Well, in our game of course ;)

Consider the following function:

```C
void resolveAttack(character_t* target, uint32_t atk)
{
  if ( target->def >= atk)
  {
    // The target's defense mitigates the attack
    target->hp = target->hp;
  }
  else if ( target->hp <= (atk - target->def) )
  {
    // The attack will knock out the target
    target->hp = 0;
  }
  else
  {
    // Calculate damage as normal
    target->hp = target->hp - (atk - target->def);
  }
}
```

Our function, `resolveAttack`, takes two inputs:
- `character_t* target`: Our defending character
- `uint32_t atk`: The attack stat of our attacker

Think about how to make a SAW contract for this function. Go ahead and try it if you'd like! We'll wait for you :)

Got an idea? Great! Let's go over the steps we need to go through...

First, let's set up our basic format for the contract:

```python
class resolveAttack_Contract(Contract):
  def specification (self):
    # Declare variables
    (target, target_p) = ptr_to_fresh(self, alias_ty("struct.character_t"), name="target")
    atk = self.fresh_var(i32, "atk")

    # Assert preconditions

    # Symbolically execute the function
    self.execute_func(target_p, atk)

    # Assert postconditions

    self.returns(void)
```

Alright, we got our basic setup! But wait a second, there are 3 possible assignments for the target's hp... Each of these behaviors are determined by the stat ranges for the defender and attacker. How are we going to represent these different behaviors?

First, let's consider the 3 possible cases for `resolveAttack`:
- Case 1: Attack mitigated
  - Precondition: `target->def >= atk`
  - Postcondition: `target->hp = target->hp`
- Case 2: Immediate KO
  - Precondition: `target->hp <= (atk - target-> def)`
  - Postcondition: `target->hp = 0`
- Case 3: Regular attack calculation
  - Precondition: Anything other than Cases 1 and 2
  - Postcondition: `target->hp = target->hp - (atk - target->def)`

With those 3 cases laid out, we now have a battle plan to represent the preconditions and postconditions needed for our proof. Now one option to account for all of these cases is to create 3 different contracts where each one specifies the appropriate precondition and postcondition pair for our case. Do we really want to copy and paste the bulk of our proof across 3 contracts though? Probably not. How about we bring our knowledge of parameterized contracts to battle! Tactics!

```python
class resolveAttack_Contract(Contract):
  def __init__(self, case : int):
    super().__init__()
    self.case = case
    # There are 3 possible cases for resolveAttack
    #   Case 1: Attack mitigated
    #   Case 2: Immediate KO
    #   Case 3: Regular attack

  def specification (self):
    # Declare variables
    (target, target_p) = ptr_to_fresh(self, alias_ty("struct.character_t"), name="target")
    atk = self.fresh_var(i32, "atk")

    # Assert preconditions
    # Determine the preconditions based on the case parameter
    if (self.case == 1):
      # target->def >= atk
      self.precondition_f("{target}.4 >= {atk}")
    elif (self.case == 2):
      # target->hp <= (atk - target->def)
      self.precondition_f("({target}.2 + {target}.4) <= {atk}")
    else:
      # Assume any other case follows the formal attack calculation
      self.precondition_f("{target}.4 < {atk}")
      self.precondition_f("({target}.2 + {target}.4) > {atk}")
    
    # Symbolically execute the function
    self.execute_func(target_p, atk)

    # Assert postconditions
    # Determine the postcondition based on the case parameter
    if (self.case == 1):
      self.points_to(target_p['hp'], cry_f("{target}.2 : [32]"))
    elif (self.case == 2):
      self.points_to(target_p['hp'], cry_f("0 : [32]"))
    else:
      self.points_to(target_p['hp'], cry_f("resolveAttack ({target}.2) ({target}.4) {atk}"))

    self.returns(void)
```

Our contract is looking pretty good now! Notice that we use `{target}.2` and `{target}.4`for our postconditions. This notation lets us access fields within our target type. Specifically, `{target}.2` refers to the `hp` field, and `{target}.4` refers to the `def` field.

It should be noted that using these cases in a parameterized contract is not the only way to set up our contract. We could have designed some fancy Cryptol that considers all of the different stat ranges for our preconditions and postconditions. However, we wanted to show this method of using cases since we can leverage Python's if statements in our proofs.

Let's set up our unit test:

```python
class GameTests(unittest.TestCase):
  def test_Game(self):
    connect(reset_server=True)
    if __name__ == "__main__": view(LogResults(verbose_failure=True))

    basedir = Path(__file__).absolute().parents[1]
    bcpath  = basedir/"artifacts/Game.bc"
    crypath = basedir/"specs/Game.cry"

    cryptol_load_file(str(crypath))
    module = llvm_load_module(str(bcpath))

    resolveAttack_case1_result = llvm_verify(module, 'resolveAttack', resolveAttack_Contract(1))
    resolveAttack_case2_result = llvm_verify(module, 'resolveAttack', resolveAttack_Contract(2))
    resolveAttack_case3_result = llvm_verify(module, 'resolveAttack', resolveAttack_Contract(3))

    self.assertIs(resolveAttack_case1_result.is_success(), True)
    self.assertIs(resolveAttack_case2_result.is_success(), True)
    self.assertIs(resolveAttack_case3_result.is_success(), True)


if __name__ == "__main__":
  unittest.main()
```

Excellent, now for the moment of truth!

```sh
$ clang -c -emit-llvm -o artifacts/Game.bc src/Game.c
$ python3 proof/Game.py
[01:59:53.576] Verifying resolveAttack ...
[01:59:53.577] Simulating resolveAttack ...
[01:59:53.580] Checking proof obligations resolveAttack ...
[01:59:53.594] Proof succeeded! resolveAttack
✅  Verified: lemma_resolveAttack_Contract (defined at proof/Game.py:179)
[01:59:53.658] Verifying resolveAttack ...
[01:59:53.659] Simulating resolveAttack ...
[01:59:53.662] Checking proof obligations resolveAttack ...
[01:59:53.689] Subgoal failed: resolveAttack safety assertion:
internal: error: in llvm_points_to SAWServer
Literal equality postcondition


[01:59:53.689] SolverStats {solverStatsSolvers = fromList ["W4 ->z3"], solverStatsGoalSize = 438}
[01:59:53.689] ----------Counterexample----------
[01:59:53.689]   target0: ([0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0], 0, 4294967293, 0, 3, 0)
[01:59:53.689]   atk0: 4294967295
[01:59:53.689] ----------------------------------
⚠️  Failed to verify: lemma_resolveAttack_Contract0 (defined at proof/Game.py:179):
error: Proof failed.
        stdout:
                [01:59:53.658] Verifying resolveAttack ...
                [01:59:53.659] Simulating resolveAttack ...
                [01:59:53.662] Checking proof obligations resolveAttack ...
                [01:59:53.689] Subgoal failed: resolveAttack safety assertion:
                internal: error: in llvm_points_to SAWServer
                Literal equality postcondition


                [01:59:53.689] SolverStats {solverStatsSolvers = fromList ["W4 ->z3"], solverStatsGoalSize = 438}
                [01:59:53.689] ----------Counterexample----------
                [01:59:53.689]   target0: ([0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0], 0, 4294967293, 0, 3, 0)
                [01:59:53.689]   atk0: 4294967295
                [01:59:53.689] ----------------------------------

[01:59:53.789] Verifying resolveAttack ...
[01:59:53.789] Simulating resolveAttack ...
[01:59:53.792] Checking proof obligations resolveAttack ...
[01:59:53.905] Proof succeeded! resolveAttack
✅  Verified: lemma_resolveAttack_Contract1 (defined at proof/Game.py:179)
F
======================================================================
FAIL: test_Game (__main__.GameTests)
----------------------------------------------------------------------
Traceback (most recent call last):
  File "proof/Game.py", line 549, in test_Game
    self.assertIs(resolveAttack_case2_result.is_success(), True)
AssertionError: False is not True

----------------------------------------------------------------------
Ran 1 test in 1.132s

FAILED (failures=1)
🛑  1 out of 3 goals failed to verify.
make: *** [Makefile:14: all] Error 1
```

A counterexample?! Let's take a closer look at what SAW provides us and reassess our strategy.

```sh
[01:59:53.689] ----------Counterexample----------
[01:59:53.689]   target0: ([0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0], 0, 4294967293, 0, 3, 0)
[01:59:53.689]   atk0: 4294967295
[01:59:53.689] ----------------------------------
```

Notice that our unit case failed for case 2, but passed for cases 1 and 3. How about we look at our precondition for case 2 again:

```python
    elif (self.case == 2):
      # target->hp <= (atk - target->def)
      self.precondition_f("({target}.2 + {target}.4) <= {atk}")
```

and if we plug in the counterexample values SAW provides us...

```sh
{target}.2 + {target}.4 <= {atk}
4294967293 +     3      <= 4294967295
             4294967296 <= 4294967295
```

Well that doesn't make sense, now does it? Well actually, it does make some sense when we recognize one BIG detail. And yes, I'm referring to the big values SAW gave us. Doesn't the atk stat look familiar? Like the upper bound for unsigned 32-bit integers?

Taking that into consideration, then our efforts to test out the counterexample was incorrect. We forgot to account for integer overflow!

```sh
4294967293 + 3 <= 4294967295
4294967295 + 1 <= 4294967295
             0 <= 4294967295
```

*Audible gasp* Good thing we have SAW on our side! Now, let's determine what we can do to fix this issue. First, let's compare our precondition again to the source code's if condition:

```python
self.precondition_f("({target}.2 + {target}.4) <= {atk}")
```

```C
else if ( target->hp <= (atk - target->def) )
```

So maybe we tried being *too* fancy showing off our associative property knowledge. While moving the defense term to the left-hand side of the expression does not violate any mathematical rules on paper, it does violate our expectations in practice when accounting for limited bit widths.

Fine, let's toss out our fancy math skills and write our contact's precondition for case 2 exactly as we see it in the source code:

```python
    elif (self.case == 2):
      # target->hp <= (atk - target->def)
      self.precondition_f("{target}.2 <= ({atk} - {target}.4)")
```

```sh
$ clang -c -emit-llvm -o artifacts/Game.bc src/Game.c
$ python3 proof/Game.py
[02:34:39.387] Verifying resolveAttack ...
[02:34:39.388] Simulating resolveAttack ...
[02:34:39.391] Checking proof obligations resolveAttack ...
[02:34:39.409] Proof succeeded! resolveAttack
✅  Verified: lemma_resolveAttack_Contract (defined at proof/Game.py:179)
[02:34:39.481] Verifying resolveAttack ...
[02:34:39.481] Simulating resolveAttack ...
[02:34:39.483] Checking proof obligations resolveAttack ...
[02:34:39.508] Subgoal failed: resolveAttack safety assertion:
internal: error: in llvm_points_to SAWServer
Literal equality postcondition


[02:34:39.508] SolverStats {solverStatsSolvers = fromList ["W4 ->z3"], solverStatsGoalSize = 438}
[02:34:39.509] ----------Counterexample----------
[02:34:39.509]   target0: ([0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0], 0, 134217728, 0, 4184634256, 0)
[02:34:39.509]   atk0: 23884688
[02:34:39.509] ----------------------------------
⚠️  Failed to verify: lemma_resolveAttack_Contract0 (defined at proof/Game.py:179):
error: Proof failed.
        stdout:
                [02:34:39.481] Verifying resolveAttack ...
                [02:34:39.481] Simulating resolveAttack ...
                [02:34:39.483] Checking proof obligations resolveAttack ...
                [02:34:39.508] Subgoal failed: resolveAttack safety assertion:
                internal: error: in llvm_points_to SAWServer
                Literal equality postcondition


                [02:34:39.508] SolverStats {solverStatsSolvers = fromList ["W4 ->z3"], solverStatsGoalSize = 438}
                [02:34:39.509] ----------Counterexample----------
                [02:34:39.509]   target0: ([0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0], 0, 134217728, 0, 4184634256, 0)
                [02:34:39.509]   atk0: 23884688
                [02:34:39.509] ----------------------------------

[02:34:39.613] Verifying resolveAttack ...
[02:34:39.613] Simulating resolveAttack ...
[02:34:39.616] Checking proof obligations resolveAttack ...
[02:34:39.735] Proof succeeded! resolveAttack
✅  Verified: lemma_resolveAttack_Contract1 (defined at proof/Game.py:179)
F
======================================================================
FAIL: test_Game (__main__.GameTests)
----------------------------------------------------------------------
Traceback (most recent call last):
  File "proof/Game.py", line 550, in test_Game
    self.assertIs(resolveAttack_case2_result.is_success(), True)
AssertionError: False is not True

----------------------------------------------------------------------
Ran 1 test in 1.200s

FAILED (failures=1)
🛑  1 out of 3 goals failed to verify.
make: *** [Makefile:14: all] Error 1
```

Nope, that didn't work either. But hey, SAW gave us a different counterexample. Let's look at that one:

```sh
[02:34:39.509] ----------Counterexample----------
[02:34:39.509]   target0: ([0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0], 0, 134217728, 0, 4184634256, 0)
[02:34:39.509]   atk0: 23884688
[02:34:39.509] ----------------------------------
```

Plugging those values into our updated case 2 precondition:

```sh
{target}.2 <= ({atk} - {target}.4)
 134217728 <= 23884688 - 4184634256
 134217728 <= 134217728
```

Nice, we got an integer underflow counterexample. Based on these input values, the source code would actually meet the first if condition:

```C
target->def >= atk
```

This means our precondition for case 2 is lacking something. Let's adjust it in this way:

```python
    elif (self.case == 2):
      # target->hp <= (atk - target->def)
      self.precondition_f("{target}.4 < {atk}")
      self.precondition_f("{target}.2 <= ({atk} - {target}.4)")
```

```sh
$ clang -c -emit-llvm -o artifacts/Game.bc src/Game.c
$ python3 proof/Game.py
[02:55:30.437] Verifying resolveAttack ...
[02:55:30.437] Simulating resolveAttack ...
[02:55:30.440] Checking proof obligations resolveAttack ...
[02:55:30.455] Proof succeeded! resolveAttack
✅  Verified: lemma_resolveAttack_Contract (defined at proof/Game.py:179)
[02:55:30.551] Verifying resolveAttack ...
[02:55:30.551] Simulating resolveAttack ...
[02:55:30.554] Checking proof obligations resolveAttack ...
[02:55:30.570] Proof succeeded! resolveAttack
✅  Verified: lemma_resolveAttack_Contract0 (defined at proof/Game.py:179)
[02:55:30.672] Verifying resolveAttack ...
[02:55:30.673] Simulating resolveAttack ...
[02:55:30.675] Checking proof obligations resolveAttack ...
[02:55:30.789] Proof succeeded! resolveAttack
✅  Verified: lemma_resolveAttack_Contract1 (defined at proof/Game.py:179)
.
----------------------------------------------------------------------
Ran 1 test in 1.140s

OK
✅  All 3 goals verified!
```

Whoo hoo! We finally won the battle! From our exercise, we found that SAW provides us with some pretty useful counterexamples to consider for edge cases that may be lacking in traditional software unit testing.

Now let's imagine that very large character stats is a concern in our game. In order to balance characters in our game and avoid problems with very large values, let's say we decided to add a function (`checkStats`) that checks character stats. Let's also assume that this function is always called before functions that use those stats like what we saw in `resolveAttack`.

```C
uint32_t checkStats(character_t* character)
{
  // Assume failure by default
  uint32_t result = FAILURE;

  // Check the stats
  if (character->hp  <= MAX_STAT &&
      character->atk <= MAX_STAT &&
      character->def <= MAX_STAT &&
      character->spd <= MAX_STAT )
  {
    result = SUCCESS;
  }

  return result;
}
```

Is this a good idea security-wise? Eh, maybe not. The assumption that `checkStats` is ALWAYS called before functions that use character stats may be missed. So what can we do? Using `resolveAttack` as an example, should we rely on its caller to also call `checkStats` and perform error handling ahead of time? An argument could be made for performance benefits, but at the cost of security. Should we call `checkStats` within `resolveAttack`? That strategy would provide more security, but the repeated `checkStats` may be redundant and could hurt performance depending on the use case. As we can see, the answer for `checkStats`'s best placement follows the classic "it depends".

For the sake of argument, let's go with the assumption that `checkStats` is always called BEFORE the call to `resolveAttack`. While the stat checks aren't performed in `resolveAttack`, we could include them as preconditions in our contract. Let's add the stat checks to our original `resolveAttack_Contract`, problems with case 2's preconditons and all!

```python
class resolveAttack_Contract(Contract):
  def __init__(self, case : int):
    super().__init__()
    self.case = case

  def specification (self):
    # Declare variables
    (target, target_p) = ptr_to_fresh(self, alias_ty("struct.character_t"), name="target")
    atk = self.fresh_var(i32, "atk")

    # Assert the precondition that the stats are below the max stat cap
    # Suggests checkStats was called before resolveAttack
    self.precondition_f("{atk} <= `{MAX_STAT}")
    self.precondition_f("{target}.2 <= `{MAX_STAT}")
    self.precondition_f("{target}.4 <= `{MAX_STAT}")

    # Determine the preconditions based on the case parameter
    if (self.case == 1):
      # target->def >= atk
      self.precondition_f("{target}.4 >= {atk}")
    elif (self.case == 2):
      # target->hp <= (atk - target->def)
      self.precondition_f("({target}.2 + {target}.4) <= {atk}")
    else:
      # Assume any other case follows the formal attack calculation
      self.precondition_f("{target}.4 < {atk}")
      self.precondition_f("({target}.2 + {target}.4) > {atk}")
    
    # Symbolically execute the function
    self.execute_func(target_p, atk)

    # Determine the postcondition based on the case parameter
    if (self.case == 1):
      self.points_to(target_p['hp'], cry_f("{target}.2 : [32]"))
    elif (self.case == 2):
      self.points_to(target_p['hp'], cry_f("0 : [32]"))
    else:
      self.points_to(target_p['hp'], cry_f("resolveAttack ({target}.2) ({target}.4) {atk}"))

    self.returns(void)
```

Would this contract pass verification? Absolutely. Given that the `MAX_STAT` preconditions limits our input values, we would never see SAW's counterexample of an integer overflow/underflow from case 2.

# Using Gained Knowledge

## Assumptions and Lemmas

Only functions with implementations in the bitcode can be verified. 
If a function is imported from an external library, that function's
implementation is defined elsewhere. As such, the implementation will
not be present in the generated bitcode. Instead of verifying these 
library functions, we can use contracts to assume their behavior.

For example, the height of
[height-balanced binary
trees](https://en.wikipedia.org/wiki/Self-balancing_binary_search_tree)
on n nodes is given by the ceiling of `log(n)`. In C, we might try to 
implement this using 

```C
#include <math.h>

uint64_t ceilLog2(uint64_t i) {
  return ceil(log2(i));
}

```

We may not care to verify this function if it's from a well-known library
that has been widely tested and inspected by the public. Alternatively,
we may not care to verify the function if that library has already been 
formally verified. Even if we have verified this function ourselves, we 
may want to speed up the verification process by blackboxing its behavior.
This is particularly important to consider if a function we formally 
verified took a long time to be verified.

To accomplish these goals, we make a contract outlining the behavior of 
our `ceilLog2` function:

```python
class ceilLog2Contract(Contract):
    def specification(self):
        i = self.fresh_var(i64, "i")
        
        self.execute_func(i)

        self.returns_f("lg2 {i}")
```

As a quick mental exercise, does our `ceilLog2` function defined earlier
always yield the behavior outlined in `ceilLog2Contract`? In other words,
is `ceil(log2(i))` in C always equivalent to Cryptol's `lg2 i`?

Objection, Your Honor! Leading the witness!

It turns out that `ceil(log2(i))` in C is NOT equal to Cryptol's `lg2 i`
for values of `i` equal to `1 << j + 1 where j > 49`. Don't believe us?
We would like to call Cryptol and C to the stand:

```Xcryptol-session
Cryptol> let j49 = 1 << 49 + 1 : [64]
Cryptol> j49
1125899906842624
Cryptol> lg2 j49
50

Cryptol> let j50 = 1 << 50 + 1 : [64]
Cryptol> j50
2251799813685248
Cryptol> lg2 j50
51

Cryptol> let j50m1 = j50 - 1
Cryptol> j50m1
2251799813685247
Cryptol> lg2 j50m1
51
```

```C
#include <stdio.h>
#include <math.h>
#include <stdint.h>

int main()
{
  uint64_t j49 = 1125899906842624;    // 1 << j + 1 where j == 49
  uint64_t j50 = 2251799813685248;    // 1 << j + 1 where j == 50
  uint64_t j50m1 = 2251799813685247;  // j50 - 1

  printf("ceil(log2(%ld)) = %llu\n", j49, (unsigned long long)ceil(log2(j49)));
  printf("ceil(log2(%ld)) = %llu\n", j50, (unsigned long long)ceil(log2(j50)));
  printf("ceil(log2(%ld)) = %llu\n", j50m1, (unsigned long long)ceil(log2(j50m1)));

  return 0;
}
```

```sh
$ gcc -o log2Test log2Test.c
$ ./log2Test
ceil(log2(1125899906842624)) = 50
ceil(log2(2251799813685248)) = 52
ceil(log2(2251799813685247)) = 52
```

The lesson here, beware of `float`s claiming to mimic `int`s. Now instead of using
`ceilLog2` as our assumption example, we could consider other functions like 
[CLZ](https://en.wikipedia.org/wiki/Find_first_set#CLZ) instead.

```c
int __builtin_clzll (unsigned long long)
```

As a bonus exercise, try to verify `__builtin_clzll`. Check out
[ceilLog2.py](./ceilLog2/proof/ceilLog2.py) to compare your results.

However, let's go back to our current `ceilLog2` function that uses
`ceil(log2(i))`. We'll add a precondition to our SAW contract to account
for the disparity we mentioned earlier.

```python
class ceilLog2Contract(Contract):
  def specification(self):
    i = self.fresh_var(i64, "i")

    self.precondition_f("lg2 {i} <= 49")
        
    self.execute_func(i)

    self.returns_f("lg2 {i}")
```

In the unit test, we would assume the `ceilLog2Contract`:

```python
log2i_assume = llvm_assume(mod, 'ceilLog2', ceilLog2Contract())
```

If a C function ever used `ceilLog2`, then we could pass in the
assumption as an optional argument to verification:

```python
getHeight_result = llvm_verify(mod, 'getHeight', getHeightContract(), lemmas=[ceilLog2_assume])
```

The optional argument is a list because we can supply multiple
assumptions or previous verifications. For example, we might have

```python
addNode_result         = llvm_verify(mod, 'addNode', addNodeContract(), lemmas=[getHeight_result])
removeNode_result      = llvm_verify(mod, 'removeNode', removeNodeContract(), lemmas=[getHeight_result])
addOneRemoveTwo_result = llvm_verify(mod, 'addOneRemoveTwo', addOneRemoveTwo(), lemmas=[addNode_result, removeNode_result])
```

One can think of lemmas as rewrite rules under the hood. Whenever SAW
encounters ceilLog2 function in the C it will interpret its behavior as
what is specified in the `ceilLog2Contract()`.


## Uninterpreting Functions

Another way to simplify proofs is to uninterpret a function. This is
useful when you don't care about the specifics of the values produced
by a total function, but rather, the types of the value produced. For
example, hashing algorithms are often total functions. We often don't
care about the particular bits of a hash function, but rather, that
the hash function returned some 32-bit integer.

This is also useful _in conjunction with lemmas_, reducing proof
complexity by decomposing a complex _specification_ into manageable
logical steps (constrained by the lemmas), much as verifying
function implementations and using results as lemmas does for the
corresponding _implementation_.

See [SMT: Equality Logic With Uninterpreted Functions](https://www21.in.tum.de/teaching/sar/SS20/6.pdf), which describes
how uninterpreted functions and constraints are applied to
Satisfiability Modulo Theories.

# Solicitation

How was your experience with this lab? Suggestions are welcome in the
form of a ticket on the course GitHub page:
https://github.com/weaversa/cryptol-course/issues

# From here, you can go somewhere!

||||
|-:|:-:|-|
|| [- Cryptographic Properties](../CryptoProofs/CryptoProofs.md) ||
