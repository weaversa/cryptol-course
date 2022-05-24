# Setting Everything Up

- Get saw-python to run
- directory layout

# Python Contracts

Here's the general layout for SAW in Python:

```python
# Import junk

class contractName(Contract):
  def specification(self):
    # Initialization and Preconditions
    # Execute Function
    # Postconditions and Return

class testName(unittest.TestCase):
  def specificTestName(self):
    # Verify contracts
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

We put `*` for simplicity, but if one only wanted certain functions, they could refer to this table for some common imports:

|Package | Values |
|---------------------|--------|
|`saw_client.crucible`| `cry`, `cry_f`|
|`saw_client.llvm`    | `Contract`, `CryptolTerm`, `SetupVal`, `FreshVar`, `i8`, `i32`, `i64`, `void`, `null`, `array_ty`, `field`, `struct`, `alias_ty` |
|`saw_client.llvm_type` | `LLVMType`, `LLVMArrayType` |

## Right Circular Shift Example

To see contracts in action we need an example. Here's some C code for right circular shift we want to verify:

```C
uint32_t RCS(uint32_t bits, uint32_t shift) {
  return (bits << shift) | (bits >> (sizeof(bits)*8 - shift));
}
```

The first step is to make the corresponding Cryptol specification for right circular shift:

```cryptol
RCS : [32] -> [32] -> [32]
RCS xs shift = xs >>> shift
```

Now let's make our Python contract:

```python
class RCS_Contract(Contract):
  def specification(self):
    bits  = self.fresh_var(i32, "bits") 
    shift = self.fresh_var(i32, "shift")
    
    self.execute_func(bits, shift)

    self.returns_f("{bits} >>> {shift}")
```

Every `Contract` object keeps track of the state of symbolic variables internally, so throughout there will be many calls to `self`. 

### Fresh Symbolic Variables

The command `self.fresh_var(type, nameString)` creates a new symbolic variable of type `type` and name `nameString`. Names for fresh symbolic variables
are not optional inputs, and they mostly serve for error messages. The string can be anything, but it makes sense to give it the name of the variable being defined. 

### Execute Functions
The command `self.execute_func(inputs)` will symbolically execute the function. There should be exactly as many comma separated inputs as there are 
in the C function. As usual with SAW, one can only place preconditions before this command and postconditions after this command 
(there are no preconditions or postconditions in this contract; we'll discuss those later).

### Return Statements
The command `self.returns_f(string)` asserts the current function returns the Cryptol term parsed from a Python String. To use Python variables in scope within the string use 
`{variable_name}`. For example,

|`self.`|`returns_f`|`("{bits} >>> {shift}")`|
|-------|-----------|---------------------|
|In this contract| assert the current function returns the Cryptol term | circular shift the bit array `xs` by `shift`|

Sometimes we don't want to return a Cryptol Term. In this case we can just use `returns(someSetupValue)`. The specification function of a Contract must
**always** have a `self.returns(someSetupValue)` or `self.returns_f(string)` statement. If the function returns `void` one can use `self.returns(void)`.

### Terms from Cryptol
The `CryptolTerm` class is a subclass of `SetupVal`, so you can use them anywhere you'd use a `SetupVal` in SAW (in non-Python SAW they are disjoint types).

The command `cry(string)` converts a Python string into a Cryptol Term that can be used in SAW. The `_f` indicates one can pass in Python local variables into the strings by surrounding
the variable with braces as we did in `{bits} >>> {shift}`. In fact, `returns_f` is just syntactic sugar for `returns(cry_f(string))`.

Braces are sometimes used in Cryptol to assign type parameters or record types. To have the parser parse a literal brace in a Cryptol string repeat the `{` or `}` symbols.
For example, consider the following made up SAW line:

```python
  self.returns_f("{{a = take`{{5}} {blah}, b = take`{{{N}}} blah }} == foo `blah")
```

We parse the Cryptol string as a boolean assertion between records

```cryptol
{a = take`{5} 23, b = take`{2} 23} == foo `blah
```

where `blah` is a local Python variable equal to `23`, `N` is a local Python variable equal to `2`, `foo` some function returning a record, and `blah` some 
Cryptol type in the specification loaded.

### Preconditions and Postconditions

The [C specification](https://www.open-std.org/jtc1/sc22/wg14/www/docs/n1256.pdf) has the following to say about bit shifts:

> If the value of the right operand is negative or is
> greater than or equal to the width of the promoted left operand, the behavior is undefined.

Due to this, we should probably add a precondition to the previous code:

```python
class RCS_Contract(Contract):
  def specification(self):
    xs    = self.fresh_var(i32, "xs") 
    shift = self.fresh_var(i32, "shift")
    
    self.precondition_f("{shift} <= 32")
    
    self.execute_func(xs, shift)

    self.returns_f("{xs} >>> {shift}")
```

The command `precondition_f(string)` asserts the constraint parsed from the Cryptol string is satisfied before execution. The program will error if you place a 
precondition after `execute_func`. Similarly, the `postcondition_f(string)` command asserts the constrained is satisfied after execution. There is also a
positionally agnostic command `proclaim_f(string)`. This latter command can be useful in helper functions (more on this later).

### Unit Testing

<insert example of verification>

## Pointers

Let's change our right circular shift function to accept a pointer to an integer instead:

```C
void RCS(uint32_t *bits, uint32_t shift) {
  bits = (bits << shift) | (bits >> (sizeof(bits)*8 - shift));
  return;
}
```

Then our contract may change to

```python
class RCS_Contract(Contract):
  def specification(self):
    bits   = self.fresh_var(i32, "bits")
    bits_p = self.alloc(i32, name="bits_p", points_to=bits, read_only=False) 
    shift = self.fresh_var(i32, "shift")
    
    self.precondition_f("{shift} <= 32")
    
    self.execute_func(bits_p, shift)

    self.postcondition_f("bits_p == ")
    
    self.returns(void)
```

## Arrays

## Structs

## Verifying Options

### Assumptions

Sometimes we wish to assume the validity of functions. For example, we might want to black box a call to a library function. 

### Lemmas

### Uninterpreting Functions
