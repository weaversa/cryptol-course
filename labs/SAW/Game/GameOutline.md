# Initial Commit

Below is a list of functions included in Game/.

## levelUp(uint32_t level);
**Goal:** Determine how to set up and verify a very simple function in a SAW contract.

**Lessons Learned**
- How to declare a fresh variable in SAW
- How to call a Cryptol function
- How to pass SAW types to Cryptol function (curly braces)

**DJ's Notes:**
- This function was more of a proof of concept for me to get familiar with SAW's Python API.
- Lessons Learned here are already covered in previous SAW.md sections


## getDefaultLevel();
**Goal:** Determine how to handle global variables in a SAW contract.

**Lessons Learned:**
- How and when to use `global_initializer`
  - When the source code already initializes the value and SAW should just use that value
- How to declare a global variable in SAW (`global_var`)
- How to set a global variable to an initialized value in a SAW contract (`points_to`)
- When to pass no inputs to `execute_func`


## initDefaultPlayer(player_t* player);
**Goal:** Determine how to setup a struct variable in a SAW contract.

**Lessons Learned:**
- How and when to use `alias_ty`
- How and when to use `alloc`
  - Use `alloc` when you only consider the pointer
  - Use `ptr_to_fresh` when you care about the values being referenced (i.e. for pre/postconditions) and the pointer
- SAW only recognizes the base struct name (`player_t` vs `character_t`)
- Passing global variables/parameters/defines defined in SAW to contracts
  - Values copied from the source code's defines
  - Examples: `MAX_NAME_LENGTH`, `SUCCESS`
- How to assert postconditions using `points_to`
- Compiling clang with the `-g` flag provides debug symbols, so you can reference fields names rather than just indices
- SAW has issues using the `points_to` postcondition for describing behavior associated for a struct with many fields
  - See the **Errors to Explore** section below
  - This contract requires explicitly checking each and every struct field

**Errors to Explore:**
- "Invalid label in record update"
  - Generated when using the following postcondition (not using struct field names)
```python
self.points_to(player, cry_f("{{ repeat 0x41 : [{MAX_NAME_LENGTH}][8], 1 : [32], 10 : [32], 5 : [32], 4 : [32], 3 : [32] }}"))
```
- "SAW doesn't yet support translating Cryptol's record type(s) into crucible-llvm's type system"
  - Generated when using the following postcondition (with struct field names)
```python
self.points_to(player, cry_f("{{ name=(repeat 0x41 : [{MAX_NAME_LENGTH}][8]), level=1 : [32], hp=10 : [32], atk=5 : [32], def=4 : [32], spd=3 : [32] }}"))
```

## checkStats(character_t* character);
**Goal:** Determine how to use parameterized contracts to set preconditions and postconditions.

**Lessons Learned:**
- Understand that when a function's behavior depends on specific input value ranges, it may be necessary to define multiple cases for a given SAW contract
  - Determine how to break a function into its fundamental case behaviors (i.e. `SUCCESS` and `FAILURE`)
- How to parameterize a SAW contract
  - Understand which parameter type is best for the job (i.e. `int`, `bool`, `string`, etc)
- How to pass and use parameters within a SAW contract
  - Note that SAW's Python API takes advantage of if-statements, which reduces the number of SAW contracts/specs that we need to write
- Understand that `ptr_to_fresh` is useful for setting preconditions for a struct's fields
- How to use contract parameters to assert two possible postconditions

**DJ's Notes:**
- Note that the `checkStats` function would be used in the Game library to check character stats every instance before such stats would be referenced/used for gameplay.
- In terms of the example, `checkStats` provides gameplay balancing by limiting how well characters can perform.
- Given this policy, all other functions included in the library assume that `checkStats` is called before them.

## resolveAttack(character_t* target, uint32_t atk);


## resetInventoryItems(inventory_t* inventory);


## initScreen(screen_t* screen, uint8_t assetID);


## setScreenTile(screen_t* screen, uint32_t screenIdx, uint32_t tableIdx);


## quickBattle(player_t* player, character_t* opponent);


## counterBattle(player_t* player, character_t* opponent);
Note: No Contract for this one. Considering dropping due to how complex the Contract needs to be (multiple possible outcomes depending on the inputs - requires specific preconditions and postconditions)


SAW finds something not true, counterexample.
- Logical error in the functions
