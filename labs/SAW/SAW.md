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

# Python Contracts Introduction

Here's the general layout for SAW in Python:

```python
# Import SAW's Python modules

class contractName(Contract):
  def specification(self):
    # Initialization and Preconditions
    # Execute Function
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

## Right Circular Shift Example

To see contracts in action we need an example. Here's some C code for
right circular shift we want to verify:

```C
uint32_t RCS(uint32_t bits, uint32_t shift) {
  return (bits << (sizeof(bits) * 8 - shift)) | (bits >> shift);
}
```

SAW doesn't actually verify C source, but rather C compiled down to
LLVM intermediate representation (IR), or bitcode. This can be
accomplished via the `clang` compiler. In this instance, we can create
the bitcode by entering the following command in a terminal.

```sh
$ clang -emit-llvm labs/SAW/src/rcs.c -c -o labs/SAW/src/rcs.bc
```

We can inspect the bitcode using SAW by loading the module and
printing some meta-data.

```Xsaw-session
$ saw
 ┏━━━┓━━━┓━┓━┓━┓
 ┃ ━━┓ ╻ ┃ ┃ ┃ ┃
 ┣━━ ┃ ╻ ┃┓ ╻ ┏┛
 ┗━━━┛━┛━┛┗━┛━┛ version 0.9.0.99 (<non-dev-build>)

sawscript> r <- llvm_load_module "labs/SAW/src/rcs.bc"
sawscript> print r
Module: rcs.bc
Types:

Globals:

External references:

Definitions:
  i32 @RCS(i32 %0, i32 %1)

```

The corresponding Cryptol specification for right circular shift is:

```cryptol
RCS : [32] -> [32] -> [32]
RCS xs shift = xs >>> shift
```

For the SAW Python API we make a `Contract` object with the required
`specification` function:

```python
class RCS_Contract(Contract):
  def specification(self):
    bits  = self.fresh_var(i32, "bits") 
    shift = self.fresh_var(i32, "shift")
    
    self.execute_func(bits, shift)

    self.returns_f("RCS {bits} {shift}")
```

Let's break down `specification` piece by piece.

### Fresh Symbolic Variables

The command `self.fresh_var(type, name)` creates a new symbolic
variable of type `type` and name `name`, where `name` is a
string. Names for fresh symbolic variables are not optional inputs,
and they mostly serve for error messages. The string can be anything,
but it makes sense to give it the name of the variable being defined.

### Execute Functions

The command `self.execute_func(input1, input2, ...)` will symbolically
execute the function. There should be exactly as many comma separated
inputs as there are in the C function. One can only place
preconditions before this command and postconditions after this
command.

### Return Statements

The command `self.returns_f(string)` asserts the current function
returns the Cryptol term parsed from a Python string. To use Python
variables in scope within the string use `{variable_name}`. For
example,

|`self.`|`returns_f(`|`"RCS {bits} {shift}"`|)|
|-------|-----------|---------------------|----|
|In this contract| assert the current function returns the Cryptol term | right circular shift `bits` by `shift` |.|

Sometimes we don't want to return a Cryptol term. In this case we can
just use `returns(someSetupValue)`. The specification function of a
Contract must **always** have a `self.returns(someSetupValue)` or
`self.returns_f(string)` statement. If the function returns `void` one
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
let's think how to parse the following line:

```python
  self.returns_f("{{a = take`{{5}} {blah}, b = take`{{{N}}} {blah} }} == foo `")
```

If `blah` is a local Python variable equal to `23` and `N` is a
local Python variable equal to `2`, then the string parses in Cryptol as

```cryptol
{a = take`{5} 23, b = take`{2} 23} == foo `blah
```

where `foo` is some Cryptol function returning a record and `blah` is
some Cryptol type in the specification loaded.

## Unit Testing

```python
class RCSTest(unittest.TestCase):
  def test_RCS(self):
    connect(reset_server=True)
    if __name__ == "__main__": view(LogResults(verbose_failure=True))
    
    bcname  = "/some/path/to/your/file.bc"
    mod = llvm_load_module(bcname)
    
    cryname = "/some/path/to/your/file.cry"
    cryptol_load_file(cryname)
    
    RCS_result = llvm_verify(mod, 'RCS', RCS_Contract())
    self.assertIs(RCS_result.is_success(), True)
```

For a contract the specification function should be called
`specification`. For tests, it doesn't matter what you name your
tests. Here we named it `test_RCS`. These tests will be ran when you
try running the Python.

Let's break down the first few lines of this function:

- The command `connect(reset_server=True)` connects to the server so
  we can use the SAW Python API
- The line `if __name__ == "__main__":
  view(LogResults(verbose_failure=True))` allows us to view the output
  with verbose error messages. If you don't want verbose error
  messages, then just use `if __name__ == "__main__":
  view(LogResults())`
- The line `bcname = "/some/path/to/your/file.bc"` declares which
  bitcode file we're analyzing. If you have multiple bitcode files,
  then make a variable for each file.
- The line `mod = llvm_load_module(bcname)` creates the object we will
  pass to verification that represents the bitcode.
- The line `cryname = "/some/path/to/your/file.cry"` specifies the
  path to the Cryptol specification.
- The line `cryptol_load_file(cryname)` loads the Cryptol specification.

Now that we've set up our environment, let's actually verify our
contract! This is done at the line

|`RCS_result =` | `llvm_verify(` | `mod,` | `'RCS',` | `RCS_Contract()`| `)`|
|----------|-----------|----|------|---------------|----|
|Assign this variable| to the result of trying to verify| the bitcode| function with this name| using this contract|.|

Now that we have the result, we want to assert this result succeeded
using `self.assertIs(RCS_result.is_success(), True)`.

## Debugging C with SAW

The full code is:

```python
import os
import unittest
from saw_client             import *
from saw_client.crucible    import * 
from saw_client.llvm        import * 
from saw_client.proofscript import *
from saw_client.llvm_type   import * 

class RCS_Contract(Contract):
  def specification(self):
    xs    = self.fresh_var(i32, "xs") 
    shift = self.fresh_var(i32, "shift")
    
    self.execute_func(xs, shift)

    self.returns_f("RCS {xs} {shift}")

class RCSTest(unittest.TestCase):
  def test_RCS(self):
    connect(reset_server=True)
    if __name__ == "__main__": view(LogResults(verbose_failure=True))
    
    pwd = os.getcwd()
    bcname  = pwd + "/../src/rcs.bc"
    cryname = pwd + "/spec/rcs.cry"
    
    cryptol_load_file(cryname)
    mod = llvm_load_module(bcname)
    
    RCS_result = llvm_verify(mod, 'RCS', RCS_Contract())
    self.assertIs(RCS_result.is_success(), True)

if __name__ == "__main__":
    unittest.main()
```

We can now run the proof script.

```
$ cd labs/SAW/proof
$ python3 rcs.py
[03:08:29.986] Verifying RCS ...
[03:08:29.987] Simulating RCS ...
[03:08:29.988] Checking proof obligations RCS ...
[03:08:30.007] Subgoal failed: RCS safety assertion:
internal: error: in RCS
Undefined behavior encountered
Details:
  Poison value created
        The second operand of `shl` was equal to or greater than the number of bits in the first operand
        
[03:08:30.007] SolverStats {solverStatsSolvers = fromList ["W4 ->z3"], solverStatsGoalSize = 382}
[03:08:30.007] ----------Counterexample----------
[03:08:30.007]   shift0: 134217728
[03:08:30.007] ----------------------------------

F
======================================================================
FAIL: test_RCS (__main__.RCSTest)
----------------------------------------------------------------------
Traceback (most recent call last):
  File "cryptol-course/labs/SAW/proof/rcs.py", line 31, in test_RCS
      self.assertIs(RCS_result.is_success(), True)
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

As expected, this alerts us of a bug:

```
        The second operand of `shl` was equal to or greater than the number of bits in the first operand
        
[03:08:30.007] SolverStats {solverStatsSolvers = fromList ["W4 ->z3"], solverStatsGoalSize = 382}
[03:08:30.007] ----------Counterexample----------
[03:08:30.007]   shift0: 134217728
[03:08:30.007] ----------------------------------
```

SAW also provides a handy counterexample, namely, when `shift =
134217728` (clearly larger than 31), we encounter undefined behavior.

One remedy to this is the following:

```C
uint32_t RCS(uint32_t bits, uint32_t shift) {
  shift %= sizeof(bits) * 8;
  return (bits << (sizeof(bits) * 8 - shift)) | (bits >> shift);
}
```

Recompiling and running SAW gives:

```sh
$ clang ../src/rcs2.c -o ../src/rcs.bc -c -emit-llvm && python3 rcs.py
[03:11:54.334] Verifying RCS ...
[03:11:54.334] Simulating RCS ...
[03:11:54.335] Checking proof obligations RCS ...
[03:11:54.351] Subgoal failed: RCS safety assertion:
internal: error: in RCS
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
FAIL: test_RCS (__main__.RCSTest)
----------------------------------------------------------------------
Traceback (most recent call last):
  File "cryptol-course/labs/SAW/proof/rcs.py", line 31, in test_RCS
      self.assertIs(RCS_result.is_success(), True)
      AssertionError: False is not True
      
      ----------------------------------------------------------------------
      Ran 1 test in 0.723s
      
      FAILED (failures=1)
      🛑  The goal failed to verify.~
```

Aha! The counter example shows that we forgot about the case when
`shift` is zero! This causes `(sizeof(bits) * 8 - 0)` to be `32`,
which is equal to the word-size of `bits`, and hence causes `<<` to
exhibit undefined behavior.

Let's try again with

```C
uint32_t RCS(uint32_t bits, uint32_t shift) {
  shift %= sizeof(bits)*8;
  if(shift == 0) return bits;
  return (bits << (sizeof(bits) * 8 - shift)) | (bits >> shift);
}
```

Finally, SAW is happy. More importantly, the C is correct and free of
undefined behavior.

```sh
$ clang ../src/rcs3.c -o ../src/rcs.bc -c -emit-llvm && python3 rcs.py
[03:14:09.561] Verifying RCS ...
[03:14:09.562] Simulating RCS ...
[03:14:09.563] Checking proof obligations RCS ...
[03:14:09.614] Proof succeeded! RCS
✅  Verified: lemma_RCS_Contract (defined at cryptol-course/labs/SAW/proof/rcs.py:30)
.
----------------------------------------------------------------------
Ran 1 test in 0.780s

OK
✅  The goal was verified!
```

# Pointers and Arrays

We'll begin by writing a function that given two arrays of a common fixed size, say five, adds the
second to the first. One way to accomplish this is to pass in the two arrays, mutate the first
and return nothing:

```C
void addRow5Mutate(uint32_t a[5], uint32_t b[5]) {
  for(int i  = 0 ; i < 5; i++) {
    a[i] += b[i];
  }
  return;
}
```

An alternative way to do this would be to create a new array storing
the result and return it:

```C
uint32_t* addRow5NewVar(uint32_t a[5], uint32_t b[5]) {
  uint32_t* c = (uint32_t*) malloc(5*sizeof(uint32_t));
  for(int i  = 0 ; i < 5; i++) {
    c[i] = a[i] + b[i];
  }
  return c;
}
```

Finally, we could mutate the first input and return it:

```C
uint32_t* addRow5Alias(uint32_t a[5], uint32_t b[5]) {
  for(int i  = 0 ; i < 5; i++) {
    a[i] += b[i];
  }
  return a;
}
```

The corresponding Cryptol specification is:

```cryptol
addRow5 : [5][32] -> [5][32] -> [5][32]
addRow5 a b = a + b
```

### Initializing Arrays and Pointers

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

### Auxiliary Variables

A SAW contract is strictly divided into three parts: 
1. Preconditions
2. Execution
3. Postconditions

SAW will complain if you place a precondition after `execute_func` and
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

```sh
SAW error message
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

While Cryptol has arrays as primitives, it does not have any notion of pointer, so we can not pass a symbolic pointer into `cry_f` (or any function with `_f`). For example, if we wanted to assert `a_p` points to `b_p` then we can use `self.points_to(a_p, b_p)` but **NOT** `self.postcondition_f(a_p, "{b_p}")`.

### Parameterized Contracts

Suppose our C code was written as 

```C
uint32_t* addRowAlias(uint32_t* a, uint32_t* b, uint32_t length) {
  for(int i  = 0 ; i < length; i++) {
    a[i] += b[i];
  }
  return a;
}
```

where the length of the array is not specified. The corresponding
Cryptol code might be:

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

One option here is to make a new contract for each `length` that's
actually used. Instead we can make a single contract with a length
input:

```python
def addRowAlias_Contract(Contract):
  def __init__(self, length : int):
    super().__init__()
    self.length = length

  def specification(self):
    (a, a_p) = ptr_to_fresh(self, array_ty(self.length, i32), name="a")
    (b, b_p) = ptr_to_fresh(self, array_ty(self.length, i32), name="b", read_only=True)
    
    self.execute_func(a_p, b_p)
    
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

## Array Example Full Code and Debugging SAW

```python
import unittest
from saw_client             import *
from saw_client.crucible    import * 
from saw_client.llvm        import * 
from saw_client.proofscript import *
from saw_client.llvm_type   import * 

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
    
    self.execute_func(a_p, b_p)
    
    self.points_to(a_p, cry_f("rowAdd`{{{self.length}}} {a} {b}"))
    
    self.returns(a_p)

class ArrayTests(unittest.TestCase):
  def test_rowAdds(self):
    connect(reset_server=True)
    if __name__ == "__main__": view(LogResults(verbose_failure=True))
    
    bcname1 = "/some/path/to/your/file.bc"
    bcname2 = "/some/path/to/your/file.bc"
    cryname = "/some/path/to/your/file.cry"
    
    cryptol_load_file(cryname)
    mod1 = llvm_load_module(bcname)
    mod2 = llvm_load_module(bcname)
    
    addRow5Mutate_result = llvm_verify(mod1, 'addRowMutate', addRowMutate_Contract())
    self.assertIs(addRow5Mutate_result.is_success(), True)
    
    addRow5NewVar_result = llvm_verify(mod1, 'addRowNewVar', addRowNewVar_Contract())
    self.assertIs(addRow5NewVar_result.is_success(), True)
    
    addRowAlias05_result = llvm_verify(mod2, 'addRowAlias', addRowAlias_Contract(5))
    addAddAlias10_result = llvm_verify(mod2, 'addRowAlias', addRowAlias_Contract(10))
    self.assertIs(addRowAlias05_result.is_success(), True)
    self.assertIs(addRowAlias10_result.is_success(), True)
```

If we try to run this Python we get:


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

Consider the following C code that checks if a pointer is null:

```C
int f(int *x) {
    return (x == (int*)0);
}
```

What do you think is the behavior of running the following contract? Keep in mind that booleans in C are encoded as `0` for false and `1` for true.

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
  
  ```
  saw failure
  ```
  
  An initialized symbolic pointer that hasn't been assigned a symbolic variable to point to is **NOT** equivalent to a symbolic null pointer. We can use `null()` in situations where we want a null pointer. For example, if we change the contract above to 
  
  ```
  class FContract(Contract):
    def specification(self):
        self.execute_func(null())

        self.returns(cry("1 : [32]"))
  ```
  
  then SAW is a happy:
  
  ```
  saw error message
  ```
</details>

This example is from [here]()


# Structs 

//Dj help

Consider the following struct:

```C
typedef struct {
  uint32_t *vector;
  uint32_t size;
} 
```

### Struct Initialization and Explicit Structs

### Structs in Cryptol

# Using Gained Knowledge

## Assumptions and Lemmas

Only functions with implementations in the bitcode can be verified. If one imports a function from a library, then that implementation will not be present in the bitcode generated as above. Instead of verifying these library functions we can use contracts to assume their behavior.

For example, the height of
[height-balanced binary
trees](https://en.wikipedia.org/wiki/Self-balancing_binary_search_tree)
on n nodes is given by the ceiling of `log(n)`. In C we might use the
`math` library to write the helper function.

```C
#include <math.h>

uint64_t log2i(uint64_t i) {
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
class log2iContract(Contract):
    def specification(self):
        i = self.fresh_var(i64, "i")
        
        self.execute_func(i)

        self.returns_f("lg2 {i}")
```


In the unit test we would assume the `log2Contract`:

```
log2i_assume = llvm_assume(mod, 'log2i', log2iContract())

```

If a C function ever used `log2i`, then we could pass in the
assumption as an optional argument to verification:

```
getHeight_result = llvm_verify(mod, 'getHeight', getHeightContract(), lemmas=[log2i_assume])
```

The optional argument is a list because we can supply multiple
assumptions or previous verifications. For example, we might have

```
addNode_result         = llvm_verify(mod, 'addNode', addNodeContract(), lemmas=[getHeight_result])
removeNode_result      = llvm_verify(mod, 'removeNode', removeNodeContract(), lemmas=[getHeight_result])
addOneRemoveTwo_result = llvm_verify(mod, 'addOneRemoveTwo', addOneRemoveTwo(), lemmas=[addNode_result, removeNode_result])
```

One can think of lemmas as rewrite rules under the hood. Whenever SAW
encounters log2 function in the C it will interpret its behavior as
what is specified in the `log2Contract()`.

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

//add in use when function complicated
//add uninterpreting may cause failure (logic needed sometimes). This can be avoided sometimes by making sure for every function you are uninterpreting you are also passing in a corresponding lemma


# Advanced Topics and Exercises

//Dj add stuff

## Global Variables

//DJ add stuff

//maybe something with `extern`

### Nested DEFINE Statements

// SAWs limitations
// Worse case: edit the source code

## Aliasing

// Talk about in general what aliasing is
// DJ add stuff (specifically with aliases)
// Talk about SAW limitations and lemmas

### Wrapper Functions

// Worse case scenario: editing the source code

## Large Data

// Dj add the stuff about inputting in a big array

## Capstone

