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

# Setting Everything Up

To run any of the examples in this lab, you need to first start the
Software Analysis Workbench (SAW) remote API (`saw-remote-api`).  If
you are using the development container that comes with this course
(`ghcr.io/weaversa/cryptol-course`), you can enter the following
command in your terminal:

```
$ start-saw-remote-api
```

Otherwise, this tool can be installed by following the instructions
for SAW found in the [Installation lab](../../INSTALL.md). Once
installed, to run `saw-remote-api`, enter the following commands into
your terminal:

```
$ export SAW_SERVER_URL=http://0.0.0.0:36691
$ saw-remote-api http --host 0.0.0.0 --port 36691 &
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
instance, we can create the bitcode by entering the following command
in a terminal.

```sh
$ clang -emit-llvm labs/SAW/src/rotl.c -c -o labs/SAW/src/rotl.bc
```

We can inspect the bitcode using SAW by loading the module and
printing some meta-data.

```Xsaw-session
$ saw
 ┏━━━┓━━━┓━┓━┓━┓
 ┃ ━━┓ ╻ ┃ ┃ ┃ ┃
 ┣━━ ┃ ╻ ┃┓ ╻ ┏┛
 ┗━━━┛━┛━┛┗━┛━┛ version 0.9.0.99 (<non-dev-build>)

sawscript> r <- llvm_load_module "labs/SAW/src/rotl.bc"
sawscript> print r
Module: rotl.bc
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

```python
  self.returns_f("{{a = take`{{5}} {var}, b = take`{{{N}}} {var} }} == foo `eel")
```

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
    
    bcname  = "/some/path/to/your/file.bc"
    mod = llvm_load_module(bcname)
    
    cryname = "/some/path/to/your/file.cry"
    cryptol_load_file(cryname)
    
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
- The line `bcname = "/some/path/to/your/file.bc"` declares the
  bitcode file to analyze. If there are multiple bitcode files,
  make a variable for each file.
- The line `mod = llvm_load_module(bcname)` creates the object to
  pass to verification that represents the bitcode.
- The line `cryname = "/some/path/to/your/file.cry"` specifies the
  path to a Cryptol specification.
- The line `cryptol_load_file(cryname)` loads a Cryptol specification.

Now that the environment is set up, let's actually verify our
contract! This is done at the line

|`rotl_result =` | `llvm_verify(` | `mod,` | `'rotl',` | `rotl_Contract()`| `)`|
|----------|-----------|----|------|---------------|----|
|Assign this variable| to the result of trying to verify| the bitcode| function with this name| using this contract|.|

Now that we have the result, we want to assert this result succeeded
using `self.assertIs(rotl_result.is_success(), True)`.

## Debugging C with SAW

<details>
  <summary>Click here for the full code</summary>

```python
import os
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
    
    pwd = os.getcwd()
    bcname  = pwd + "/../src/rotl.bc"
    cryname = pwd + "/spec/rotl.cry"
    
    cryptol_load_file(cryname)
    mod = llvm_load_module(bcname)
    
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
  
</details>

We can now run the proof script.

```sh
$ cd labs/SAW/proof
$ python3 rotl.py
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
  File "cryptol-course/labs/SAW/proof/rotl.py", line 31, in test_rotl
      self.assertIs(rotl_result.is_success(), True)
      AssertionError: False is not True
      
      ----------------------------------------------------------------------
      Ran 1 test in 0.750s
      
      FAILED (failures=1)
      🛑  The goal failed to verify.
```

SAW alerted us about potentially undefined behavior mentioned in the
[C specification](https://www.open-std.org/jtc1/sc22/wg14/www/docs/n1256.pdf)
has the following to say about bit shifts:

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
$ clang ../src/rotl2.c -o ../src/rotl.bc -c -emit-llvm && python3 rotl.py
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
  File "cryptol-course/labs/SAW/proof/rotl.py", line 31, in test_rotl
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
$ clang ../src/rotl3.c -o ../src/rotl.bc -c -emit-llvm && python3 rotl.py
[03:14:09.561] Verifying rotl ...
[03:14:09.562] Simulating rotl ...
[03:14:09.563] Checking proof obligations rotl ...
[03:14:09.614] Proof succeeded! rotl
✅  Verified: lemma_rotl_Contract (defined at cryptol-course/labs/SAW/proof/rotl.py:30)
.
----------------------------------------------------------------------
Ran 1 test in 0.780s

OK
✅  The goal was verified!
```

Finally, SAW is happy. More importantly, the C is correct and free of
undefined behavior.

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
def addRow5Mutate_Contract(Contract):
  def specification(self):
    a   = self.fresh_var(array_ty(5, i32), "a")
    a_p = self.alloc(array_ty(5, i32), points_to=a) 
    b   = self.fresh_var(array_ty(5, i32), "b")
    b_p = self.alloc(array_ty(5, i32), points_to=b, read_only=True)
    
    self.execute_func(a_p, b_p)
    
    self.points_to(a_p, cry_f("rowAdd {a} {b}"))
    
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

| `self.points_to(` | `a_p,` | `cry_f( "rowAdd {a} {b}"` | `))` |
|-------------------|--------|---------------------------|------|
| Assert in the current contract that the pointer | with this name | points to this Cryptol term | . | 

Finally, `specification` must contain `self.returns` or `self.returns_f`, so we use `self.returns(void)`.

### Python Helper Functions

To limit code reuse we can define helper functions in Python. For
example, the following construct is often used:

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
def addRow5Mutate_Contract(Contract):
  def specification(self):
    (a, a_p) = ptr_to_fresh(self, array_ty(5, i32), name="a")
    (b, b_p) = ptr_to_fresh(self, array_ty(5, i32), name="b", read_only=True)
    
    self.execute_func(a_p, b_p)
    
    self.points_to(a_p, cry_f("rowAdd {a} {b}"))
    
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
def addRow5NewVar_Contract(Contract):
  def specification(self):
    (a, a_p) = ptr_to_fresh(self, array_ty(5, i32), name="a")
    (b, b_p) = ptr_to_fresh(self, array_ty(5, i32), name="b", read_only=True)
    (c, c_p) = ptr_to_fresh(self, array_ty(5, i32), name="c")
    
    self.execute_func(a_p, b_p)
    
    self.points_to(c_p, cry_f("rowAdd {a} {b}"))
    
    self.returns(c_p)
```

Running a unit test yields the following error message:

```
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
def addRow5NewVar_Contract(Contract):
  def specification(self):
    (a, a_p) = ptr_to_fresh(self, array_ty(5, i32), name="a")
    (b, b_p) = ptr_to_fresh(self, array_ty(5, i32), name="b", read_only=True)
    
    self.execute_func(a_p, b_p)
    
    (c, c_p) = ptr_to_fresh(self, array_ty(5, i32), name="c")
    self.points_to(c_p, cry_f("rowAdd {a} {b}"))
    
    self.returns(c_p)
```

Python supports wildcards, denoted by `_`, like Cryptol. Wildcards are placeholders for values we don't use. For example, we could rewrite the `addRow5NewVar_Contract` as follows: 

```python
def addRow5NewVar_Contract(Contract):
  def specification(self):
    (a, a_p) = ptr_to_fresh(self, array_ty(5, i32), name="a")
    (b, b_p) = ptr_to_fresh(self, array_ty(5, i32), name="b", read_only=True)
    
    self.execute_func(a_p, b_p)
    
    (_, c_p) = ptr_to_fresh(self, array_ty(5, i32), name="c")
    self.points_to(c_p, cry_f("rowAdd {a} {b}"))
    
    self.returns(c_p)
```

### Postconditions and `points_to`

One could replace the `points_to` line with a `postcondition_f` line to get an equivalent contract:

```python
def addRow5NewVar_Contract(Contract):
  def specification(self):
    (a, a_p) = ptr_to_fresh(self, array_ty(5, i32), name="a", read_only=True)
    (b, b_p) = ptr_to_fresh(self, array_ty(5, i32), name="b", read_only=True)
    
    self.execute_func(a_p, b_p)
    
    (c, c_p) = ptr_to_fresh(self, array_ty(5, i32), name="c")
    self.postcondition_f("{c} == rowAdd {a} {b}")
    
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

We could make a new contract for each value of `length` used in the C code. Instead we make a single contract with a `length`
parameter:

```python
def addRowAlias_Contract(Contract):
  def __init__(self, length : int):
    super().__init__()
    self.length = length

  def specification(self):
    (a, a_p) = ptr_to_fresh(self, array_ty(self.length, i32), name="a")
    (b, b_p) = ptr_to_fresh(self, array_ty(self.length, i32), name="b", read_only=True)
    length   = fresh_var(i8, "length")
    
    self.execute_func(a_p, b_p, length)
    
    self.points_to(a_p, cry_f("rowAdd`{{{self.length}}} {a} {b}"))
    
    self.returns(a_p)
```

However, we still need to make a test for each length encountered:

```python
class ArrayTests(unittest.TestCase):
  def test_rowAdds(self):
    connect(reset_server=True)
    if __name__ == "__main__": view(LogResults(verbose_failure=True))
    
    bcname  = "/some/path/to/your/file.bc"
    cryname = "/some/path/to/your/file.cry"
    
    cryptol_load_file(cryname)
    mod = llvm_load_module(bcname)
    
    arrayAddNewVar05_result = llvm_verify(mod, 'addRowAlias', addRowAlias_Contract(5))
    arrayAddNewVar10_result = llvm_verify(mod, 'addRowAlias', addRowAlias_Contract(10))
    self.assertIs(addRowAlias05_result.is_success(), True)
    self.assertIs(addRowAlias10_result.is_success(), True)
```

## Full Code Example and Debugging SAW

```python
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
    
def addRow5Mutate_Contract(Contract):
  def specification(self):
    (a, a_p) = ptr_to_fresh(self, array_ty(5, i32), name="a")
    (b, b_p) = ptr_to_fresh(self, array_ty(5, i32), name="b", read_only=True)
    
    self.execute_func(a_p, b_p)
    
    self.points_to(a_p, cry_f("rowAdd {a} {b}"))
    
    self.returns(void)

def addRow5NewVar_Contract(Contract):
  def specification(self):
    (a, a_p) = ptr_to_fresh(self, array_ty(5, i32), name="a")
    (b, b_p) = ptr_to_fresh(self, array_ty(5, i32), name="b", read_only=True)
    
    self.execute_func(a_p, b_p)
    
    (c, c_p) = ptr_to_fresh(self, array_ty(5, i32), name="c")
    self.points_to(c_p, cry_f("rowAdd {a} {b}"))
    
    self.returns(c_p)

def addRowAlias_Contract(Contract):
  def __init__(self, length : int):
    super().__init__()
    self.length = length

  def specification(self):
    (a, a_p) = ptr_to_fresh(self, array_ty(self.length, i32), name="a")
    (b, b_p) = ptr_to_fresh(self, array_ty(self.length, i32), name="b", read_only=True)
    length   = fresh_var(i8, "length")
    
    self.execute_func(a_p, b_p, length)
    
    self.points_to(a_p, cry_f("rowAdd`{{{length}}} {a} {b}"))
    
    self.returns(a_p)

class ArrayTests(unittest.TestCase):
  def test_rowAdds(self):
    connect(reset_server=True)
    if __name__ == "__main__": view(LogResults(verbose_failure=True))
    
    bcname = "../src/addRow.bc"
    cryname = "spec/addRow.cry"
    
    cryptol_load_file(cryname)
    mod = llvm_load_module(bcname)
    
    addRow5Mutate_result = llvm_verify(mod, 'addRowMutate', addRowMutate_Contract())
    self.assertIs(addRow5Mutate_result.is_success(), True)
    
    addRow5NewVar_result = llvm_verify(mod, 'addRowNewVar', addRowNewVar_Contract())
    self.assertIs(addRow5NewVar_result.is_success(), True)
    
    addRowAlias05_result = llvm_verify(mod, 'addRowAlias', addRowAlias_Contract(5))
    addAddAlias10_result = llvm_verify(mod, 'addRowAlias', addRowAlias_Contract(10))
    self.assertIs(addRowAlias05_result.is_success(), True)
    self.assertIs(addRowAlias10_result.is_success(), True)
```

What do you think will happen if we run this code?

<details>
  <summary>Click here to find out!</summary>
  Running the code, SAW verifies the first two contracts
  
  ```sh
  $ clang ../src/addRow.c -o ../src/addRow.bc -c -emit-llvm && python3 addRow.py
  [15:40:51.330] Verifying addRow5Mutate ...
  [15:40:51.330] Simulating addRow5Mutate ...
  [15:40:51.335] Checking proof obligations addRow5Mutate ...
  [15:40:51.362] Proof succeeded! addRow5Mutate
  ✅  Verified: lemma_addRow5Mutate_Contract (defined at /home/cryptol/cryptol-course/labs/SAW/proof/addRow.py:64)
  [15:40:51.430] Verifying addRow5NewVar ...
  [15:40:51.430] Simulating addRow5NewVar ...
  [15:40:51.435] Checking proof obligations addRow5NewVar ...
  [15:40:51.462] Proof succeeded! addRow5NewVar
  ✅  Verified: lemma_addRow5NewVar_Contract (defined at /home/cryptol/cryptol-course/labs/SAW/proof/addRow.py:67)
  ```
  
  ...but fails to verify the third contract. It alerts us there is a memory error
  
  ```
  [15:40:51.527] Verifying addRowAlias ...
  [15:40:51.528] Simulating addRowAlias ...
  [15:40:51.532] Symbolic simulation completed with side conditions.
  [15:40:51.535] Checking proof obligations addRowAlias ...
  [15:40:51.575] Subgoal failed: addRowAlias safety assertion:
  internal: error: in addRowAlias
  Error during memory load
  ```
  
  and even produces the following counterexample:

  ```
  [15:40:51.575] SolverStats {solverStatsSolvers = fromList ["W4 ->z3"], solverStatsGoalSize = 331}
  [15:40:51.575] ----------Counterexample----------
  [15:40:51.575]   length0: 6
  [15:40:51.575]   : False
  [15:40:51.575] ----------------------------------
  ⚠️  Failed to verify: lemma_addRowAlias_Contract (defined at /home/cryptol/cryptol-course/labs/SAW/proof/addRow.py:37):
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
  File "/home/cryptol/cryptol-course/labs/SAW/proof/addRow.py", line 71, in test_rowAdds
    self.assertIs(addRowAlias05_result.is_success(), True)
  AssertionError: False is not True

  ----------------------------------------------------------------------
  Ran 1 test in 1.735s

  FAILED (failures=1)
  🛑  1 out of 3 goals failed to verify.
  ```
  
  SAW is telling us we forgot to add a precondition to assert our symbolic `length` agrees with our Python parameter `self.length`. This is an easy fix:

  ```
  def addRowAlias_Contract(Contract):
  def __init__(self, length : int):
    super().__init__()
    self.length = length

  def specification(self):
    (a, a_p) = ptr_to_fresh(self, array_ty(self.length, i32), name="a")
    (b, b_p) = ptr_to_fresh(self, array_ty(self.length, i32), name="b", read_only=True)
    length   = fresh_var(i8, "length")
    
    self.precondition_f("{self.length} == {length}")
    
    self.execute_func(a_p, b_p, length)
    
    self.points_to(a_p, cry_f("rowAdd`{{{length}}} {a} {b}"))
    
    self.returns(a_p)
  ```

And now SAW happily verifies the third contract!
  
  ```sh
  $ python3 addRow.py
  ✅  Verified: lemma_addRow5Mutate_Contract (defined at /home/cryptol/cryptol-course/labs/SAW/proof/addRow.py:64)
  ✅  Verified: lemma_addRow5NewVar_Contract (defined at /home/cryptol/cryptol-course/labs/SAW/proof/addRow.py:67)
  ✅  Verified: lemma_addRowAlias_Contract (defined at /home/cryptol/cryptol-course/labs/SAW/proof/addRow.py:37)
  .
  ----------------------------------------------------------------------
  Ran 1 test in 1.985s

  OK
  ✅  All 3 goals verified!
  ```

</details>

## Explicit Arrays

Another way to initialize arrays is through the `array` command. For
example, suppose I have the following C function (taken from
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

```Python
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
        bcname = '../src/null.bc'
        mod    = llvm_load_module(bcname)

        result = llvm_verify(mod, 'f', FContract())
        self.assertIs(result.is_success(), True)


if __name__ == "__main__":
    unittest.main()
```

<details>
  <summary>Click here to see if your guess is correct!</summary>
  
  It turns out the contract above will fail!
  
  ```sh
  $ clang ../src/null.c -o ../src/null.bc -c -emit-llvm && python3 null.py
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
  ```
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
  
  then SAW is a happy.
  
  ```sh
  $ python3 null.py
  [17:33:50.802] Verifying isNull ...
  [17:33:50.802] Simulating isNull ...
  [17:33:50.804] Checking proof obligations isNull ...
  [17:33:50.804] Proof succeeded! isNull
  ✅  Verified: lemma_isNull_Contract (defined at /home/cryptol/cryptol-course/labs/SAW/proof/null.py:22)
  .
  ----------------------------------------------------------------------
  Ran 1 test in 1.301s

  OK
  ✅  The goal was verified!
  ```
</details>

This example is from [here]()


# Structs 

In this section we will learn how to verify code involving structs by analyzing a game. The code for the game can be found [here](./Game/src/).

To complete this lab, navigate to the [Game directory](./Game). In there, you'll notice the following:
- `Makefile`: Provides the necessary steps to generate our bitcode and run our SAW Python scripts.
- `src/`: Contains the source code we'll be analyzing
- `proof/`: Contains our Python scripts to run our SAW contracts. Your job will be to complete the `TODO` sections marked throughout [Game.cry](./Game/proof/Game.py). If you get stuck, you can refer to [Game_answers.cry](./Game/proof/Game_answers.py) or look at the discussions mentioned later in this markdown file!
- `specs/`: Contains our Cryptol specs that our SAW contracts can call. Feel free to add your own Cryptol functions to help you complete this lab!
- `DLC/`: Contains an extended version of this lab (think Downloadable Content) with even more Game functions for you to play with! While there aren't any lab worksheets configured for you in there, you can reference the contents for how to tackle additional functions. For more information regarding to what each function intends to teach, refer to [GameOutline.md](./Game/DLC/GameOutline.md).

With that knowledge, make sure you have `start-saw-remote-api` running, open up Game.cry, fill out your answers, and test your work by running `make`. Game on!


## Struct Initialization

The game defines the following player type.

```C
typedef struct {
  uint8_t name[MAX_NAME_LENGTH];
  uint32_t level;
  uint32_t hp;
  uint32_t atk;
  uint32_t def;
  uint32_t spd;
} character_t;

typedef character_t player_t;
```

A player is initialized with some default values

```C
uint32_t initDefaultPlayer(player_t* player)
{
  uint8_t  i = 0;
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

  return SUCCESS;
}
```

where `SUCCESS` and `MAX_NAME_LENGTH` are C constants:

```C
#define MAX_NAME_LENGTH 12
#define SUCCESS 170
#define FAILURE 85
```

We use the following contract to verify this initialization function:

```python
MAX_NAME_LENGTH = 12
SUCCESS         = 170
FAILURE         = 85

class initDefaultPlayer_Contract(Contract):
  def specification (self):
    player = self.alloc(alias_ty("struct.player_t"))

    self.execute_func(player)

    self.points_to(player['name'], cry_f("repeat 0x41 : [{MAX_NAME_LENGTH}][8]"))
    self.points_to(player['level'], cry_f("1 : [32]"))
    self.points_to(player['hp'], cry_f("10 : [32]"))
    self.points_to(player['atk'], cry_f("5 : [32]"))
    self.points_to(player['def'], cry_f("4 : [32]"))
    self.points_to(player['spd'], cry_f("3 : [32]"))
    
    self.returns(cry_f("`({SUCCESS}) : [32]"))
```

For every C symbol defined using `#define` we make a corresponding Python global variable.

```
MAX_NAME_LENGTH = 12
SUCCESS         = 170
FAILURE         = 85
```

The command `alias_ty("struct.<typedef name>")` creates a type corresponding to the structure, e.g., 

```python3 
player = self.alloc(alias_ty("struct.player_t"))
```
creates a symbolic pointer variable `player` pointing to a structure of type `player_t`.

Let's breakdown a `points_to` command seen above:


| `self.points_to(` | `player` | `['name']` | `,` | `cry_f("repeat 0x41 : [{MAX_NAME_LENGTH}][8]")` | `)` |
|-------------------|-----------|----------|------|-------------------------------------------------|-----|
| Assert in the current contract that the following pointer | with this name | points to a struct with this named field | and the value of that field is | this expression | . |


Above we use strings to reference fields of structures. However, we can only do this when strings are present in the bitcode, e.g., when debug symbols are included in the generated bitcode. The `-g` clang flag tells the compiler to include the field names of the structs in the bitcode. For the full compilation details, check out the [Makefile](./Game/Makefile) associated with the `Game` directory. 

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

If we didn't want to include debug symbols in the bitcode, then we can reference the struct fields by using their corresponding indices:

```python
class initDefaultPlayer_Contract(Contract):
  def specification (self):
    player = self.alloc(alias_ty("struct.character_t"))

    self.execute_func(player)

    self.points_to(player[0], cry_f("repeat 0x41 : [{MAX_NAME_LENGTH}][8]"))
    self.points_to(player[1], cry_f("1 : [32]"))
    self.points_to(player[2], cry_f("10 : [32]"))
    self.points_to(player[3], cry_f("5 : [32]"))
    self.points_to(player[4], cry_f("4 : [32]"))
    self.points_to(player[5], cry_f("3 : [32]"))
    
    self.returns(cry_f("`({SUCCESS}) : [32]"))
```


### Structs as Cryptol Tuples and Records

We can replace all of the `points_to` postconditions in the previous contract since Cryptol interprets symbolic structs as tuples.

```python
self.points_to(player, cry_f("""( repeat 0x41 : [{MAX_NAME_LENGTH}][8],
                                  1  : [32],
                                  10 : [32],
                                  5  : [32],
                                  4  : [32],
                                  3  : [32] )"""))
```

We use 3 double quotes `"""` in our `cry_f` call. This technique is useful when we want to separate our expected Cryptol-defined behaviors over multiple lines to improve code readability. Python considers the 3 double quotes as a multiline string. Multiline strings may also be used as block comments in Python.

While Cryptol's record types could also represent structs, SAW does not currently support translating Cryptol's record types into crucible-llvm's type system. If we tried to represent the struct as a Cryptol record like so:

```python
self.points_to(player, cry_f("""{{ name = repeat 0x41 : [{MAX_NAME_LENGTH}][8],
                                   level = 1 : [32],
                                   hp = 10 : [32],
                                   atk = 5 : [32],
                                   def = 4 : [32],
                                   spd = 3 : [32] }}"""))
```

SAW would return this error:

```
clang -c -g -emit-llvm -o artifacts/Game.bc src/Game.c
python3 proof/Game.py
⚠️  Failed to verify: lemma_initDefaultPlayer_Contract (defined at proof/Game.py:537):
error: SAW doesn't yet support translating Cryptol's record type(s) into crucible-llvm's type system.
        stdout:

F
======================================================================
FAIL: test_Game (__main__.GameTests)
----------------------------------------------------------------------
Traceback (most recent call last):
  File "proof/Game.py", line 563, in test_Game
    self.assertIs(initDefaultPlayer_result.is_success(), True)
AssertionError: False is not True

----------------------------------------------------------------------
Ran 1 test in 0.752s

FAILED (failures=1)
🛑  The goal failed to verify.
make: *** [Makefile:14: all] Error 1
```

If a Cryptol specification uses a record type to represent structs, then we can define a Python helper function for wrapping. 


## Explicit Structs

We can also explicitly define a struct in SAW. Let's consider another struct:

```c
#define GAITS 2
#define DIRECTIONS 4
#define ANIMATION_STEPS 3

typedef struct {
  character_t* character;
  uint8_t frames[GAITS][DIRECTIONS][ANIMATION_STEPS];
  uint32_t xPos;
  uint32_t yPos;
} sprite_t;
```

The idea behind the `sprite_t` struct is to hold all of the sprites associated with a character that we will present in our game. `character` is a pointer to a `character_t` type that the sprite is tied to. `frames` is a 3D uint8_t array where each element represents an asset identifier from our art collection. Why a 3D array? Well, we want to provide animations for our characters that way it looks like they are moving on the screen (and it's a great excuse to discuss multi-dimensional arrays in SAW). The first dimension refers to the number of gaits we want to represent, that being walking and running. The second dimension refers to the number of directions a character can face. Imagine that we are working with a 2D top down game, so we have 4 directions: up, down, left, and right. The third dimension refers to the number of frames per movement.

Let's think about how we walk forward (feel free to try this at home). If you are walking forward, you first stand up, move one foot in front of the other, place that foot down, lift up your back foot, and move that back foot ahead of your front foot. Rinse and repeat, and you'll be walking 'cross the floor.

Now that's a lot of steps! We can simplify this process by only considering 3 frames: standing up, left foot in front, and right foot in front. We can then reuse the standing up frame as a transition between the left foot forward and the right foot forward frames.

Alright, that's enough of a crash course in animation. Let's get back to our struct. We have two more fields: xPos and yPos. These are simply positional references for where a character is located relative to the screen.

With that understanding, let's consider a function that uses the `sprite_t` struct:

```c
uint32_t initDefaultSprite(character_t* character, sprite_t* sprite)
{
  // Initialize the character to the passed pointer
  sprite->character = character;

  // Initialize the sprite frames to the default asset
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

  // Initialize sprite's default position
  sprite->xPos = 1;
  sprite->yPos = 2;

  return SUCCESS;
}
```

Now, let's go ahead and make a contract to represent this function:

```python
class initDefaultSprite_Contract(Contract):
  def specification (self):
    # Declare variables
    character       = self.alloc(alias_ty("struct.character_t"))
    tempCharacter_p = self.alloc(alias_ty("struct.character_t"))
    frames   = self.fresh_var(array_ty(GAITS, array_ty(DIRECTIONS, array_ty(ANIMATION_STEPS, i8))), "sprite.frames")
    xPos     = self.fresh_var(i32, "sprite.xPos")
    yPos     = self.fresh_var(i32, "sprite.yPos")
    sprite   = struct(tempCharacter_p, frames, xPos, yPos)
    sprite_p = self.alloc(alias_ty("struct.sprite_t"))
    self.points_to(sprite_p, sprite)

    # Symbolically execute the function
    self.execute_func(character_p, sprite_p)

    # Assert postconditions
    self.points_to(sprite_p, struct( character_p,
                                     cry_f("zero : [{GAITS}][{DIRECTIONS}][{ANIMATION_STEPS}][8]"),
                                     cry_f("1 : [32]"),
                                     cry_f("2 : [32]") ))
                                           
    self.returns_f("`({SUCCESS}) : [32]")
```

Like `array`, the `struct` keyword declares a symbolic struct given a variable for each struct field. We assert the precondition that our pointer points to this symbolic struct. Alternatively, we could replace

```python
    sprite   = struct(tempCharacter_p, frames, xPos, yPos)
    sprite_p = self.alloc(alias_ty("struct.sprite_t"))
    self.points_to(sprite_p, sprite)
```
 with 
```python
sprite_p = self.alloc(alias_ty("struct.sprite_t"), points_to = struct(tempCharacter_p, frames, xPos, yPos))
```
since we don't use `sprite` later in the code. If we wanted, we could assert other preconditions on `tempCharacter_p`, `frames`, `xPos`, and `yPos``. We don't in this example, but it's still a feature to consider!

In the postcondition, we assert `sprite_p` points to some concrete structure. The benefit of using explicit structs is that it allows us to represent pointer fields that may be present in a struct.

However, explicit structs isn't the only way to represent pointer fields. We could also use a tuple to assert our postcondition. However, we will need to change our definition for `character_p`.

```python
class initDefaultSprite_Contract(Contract):
  def specification (self):
    # Declare variables
    character_p = self.fresh_var(ptr_ty(alias_ty("struct.character_t")))
    tempCharacter_p = self.alloc(alias_ty("struct.character_t"))
    ty = array_ty(GAITS, array_ty(DIRECTIONS, array_ty(ANIMATION_STEPS, i8)))
    frames = self.fresh_var(ty, "sprite.frames")
    xPos = self.fresh_var(i32, "sprite.xPos")
    yPos = self.fresh_var(i32, "sprite.yPos")
    sprite_p = self.alloc(alias_ty("struct.sprite_t"), points_to = struct(tempCharacter_p, frames, xPos, yPos))

    # Symbolically execute the function
    self.execute_func(character_p, sprite_p)

    # Assert postconditions
    self.points_to(sprite_p, cry_f("""( {character_p},
                                     zero : [{GAITS}][{DIRECTIONS}][{ANIMATION_STEPS}][8],
                                     1 : [32],
                                     2 : [32]) """))
                  
    self.returns_f("`({SUCCESS}) : [32]")
```


## Exercise: Resolving an Attack!

Feeling pretty confident with our little `player_t` and `character_t` structs? How about we go for a full on attack then? Well, in our game of course ;)

Consider the following function:

```c
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

Got an idea, great! Let's go over the steps we need to go through...

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

    pwd = os.getcwd()
    bitcode_name = pwd + "/artifacts/Game.bc"
    cryptol_name = pwd + "/specs/Game.cry"

    cryptol_load_file(cryptol_name)
    module = llvm_load_module(bitcode_name)

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

```
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

```
{target}.2 + {target}.4 <= {atk}
4294967293 +     3      <= 4294967295
             4294967296 <= 4294967295
```

Well that doesn't make sense, now does it? Well actually, it does make some sense when we recognize one BIG detail. And yes, I'm referring to the big values SAW gave us. Doesn't the atk stat look familiar? Like the upper bound for unsigned 32-bit integers?

Taking that into consideration, then our efforts to test out the counterexample was incorrect. We forgot to account for integer overflow!

```
4294967293 + 3 <= 4294967295
4294967295 + 1 <= 4294967295
             0 <= 4294967295
```

*Audible gasp* Good thing we have SAW on our side! Now, let's determine what we can do to fix this issue. First, let's compare our precondition again to the source code's if condition:

```python
self.precondition_f("({target}.2 + {target}.4) <= {atk}")
```

```c
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

```
[02:34:39.509] ----------Counterexample----------
[02:34:39.509]   target0: ([0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0], 0, 134217728, 0, 4184634256, 0)
[02:34:39.509]   atk0: 23884688
[02:34:39.509] ----------------------------------
```

Plugging those values into our updated case 2 precondition:

```
{target}.2 <= ({atk} - {target}.4)
 134217728 <= 23884688 - 4184634256
 134217728 <= 134217728
```

Nice, we got an integer underflow counterexample. Based on these input values, the source code would actually meet the first if condition:

```c
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

```c
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

Only functions with implementations in the bitcode can be verified. If one imports a function from a library, then that implementation will not be present in the bitcode generated as above. Instead of verifying these library functions we can use contracts to assume their behavior.

For example, the height of
[height-balanced binary
trees](https://en.wikipedia.org/wiki/Self-balancing_binary_search_tree)
on n nodes is given by the ceiling of `log(n)`. In C we might try to implement this using 

```C
#include <math.h>

uint64_t ceilLog2(uint64_t i) {
  return ceil(log2(i));
}

```

We may not care to verify this function because it's a library
function, especially if that library has already been formally
verified. Also, even if we have verified this function we would like
to speed up the verification procedure by black boxing its behavior.

To accomplish these goals we make a contract outlining what the
behavior of this function:

```python
class ceilLog2Contract(Contract):
    def specification(self):
        i = self.fresh_var(i64, "i")
        
        self.execute_func(i)

        self.returns_f("lg2 {i}")
```

We should note that `ceil(log2(i))` in C is NOT equal to Cryptol's `lg2` for values `1 << j + 1 where j > 49`.

To illustrate this disparity, consider the following:

```
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

```c
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

```
$ gcc -o log2Test log2Test.c
$ ./log2Test
ceil(log2(1125899906842624)) = 50
ceil(log2(2251799813685248)) = 52
ceil(log2(2251799813685247)) = 52
```

The lesson here, beware of `float`s claiming to mimic `int`s. To account for this disparity, we could add a precondition to our SAW contract.

```python
class ceilLog2Contract(Contract):
  def specification(self):
    i = self.fresh_var(i64, "i")

    self.precondition_f("lg2 {i} <= 49")
        
    self.execute_func(i)

    self.returns_f("lg2 {i}")
```

In the unit test, we would assume the `ceilLog2Contract`:

```
log2i_assume = llvm_assume(mod, 'ceilLog2', ceilLog2Contract())

```

If a C function ever used `ceilLog2`, then we could pass in the
assumption as an optional argument to verification:

```
getHeight_result = llvm_verify(mod, 'getHeight', getHeightContract(), lemmas=[ceilLog2_assume])
```

The optional argument is a list because we can supply multiple
assumptions or previous verifications. For example, we might have

```
addNode_result         = llvm_verify(mod, 'addNode', addNodeContract(), lemmas=[getHeight_result])
removeNode_result      = llvm_verify(mod, 'removeNode', removeNodeContract(), lemmas=[getHeight_result])
addOneRemoveTwo_result = llvm_verify(mod, 'addOneRemoveTwo', addOneRemoveTwo(), lemmas=[addNode_result, removeNode_result])
```

One can think of lemmas as rewrite rules under the hood. Whenever SAW
encounters ceilLog2 function in the C it will interpret its behavior as
what is specified in the `ceilLog2Contract()`.

**Exercise**: Does the `C` function `ceilLog2` as defined above always yield the behavior outlined in `ceilLog2Contract`? That is, was our assumption a fair one to make? If not, change the `C` code (possibly using different library functions) to match `lg2` in Cryptol.

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
