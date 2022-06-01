# Initial Commit

Below is a list of functions included in Game/.

## levelUp(uint32_t level);
**Goal:** How to set up and verify a very simple function in a SAW contract.

**Lessons Learned**
- How to declare a fresh variable in SAW
- How to call a Cryptol function
- How to pass SAW types to Cryptol function (curly braces)

**DJ's Notes:**
- This function was more of a proof of concept for me to get familiar with SAW's Python API.
- Lessons Learned here are already covered in previous SAW.md sections


## getDefaultLevel();
**Goal:** How to handle global variables in a SAW contract.

**Lessons Learned:**
- How and when to use `global_initializer`
  - When the source code already initializes the value and SAW should just use that value
- How to declare a global variable in SAW (`global_var`)
- How to set a global variable to an initialized value in a SAW contract (`points_to`)
- When to pass no inputs to `execute_func`


## initDefaultPlayer(player_t* player);
**Goal:** How to setup a struct variable in a SAW contract.

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


## resolveAttack(character_t* target, uint32_t atk);


## resetInventoryItems(inventory_t* inventory);


## initScreen(screen_t* screen, uint8_t assetID);


## setScreenTile(screen_t* screen, uint32_t screenIdx, uint32_t tableIdx);


## quickBattle(player_t* player, character_t* opponent);


## counterBattle(player_t* player, character_t* opponent);
Note: No Contract for this one. Considering dropping due to how complex the Contract needs to be (multiple possible outcomes depending on the inputs - requires specific preconditions and postconditions)


SAW finds something not true, counterexample.
- Logical error in the functions
