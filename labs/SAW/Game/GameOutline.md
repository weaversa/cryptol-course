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
- How to represent multiple Unit Tests on the same contract with different input parameters

**DJ's Notes:**
- Note that the `checkStats` function would be used in the Game library to check character stats every instance before such stats would be referenced/used for gameplay.
- In terms of the example, `checkStats` provides gameplay balancing by limiting how well characters can perform.
- Given this policy, all other functions included in the library assume that `checkStats` is called before them.
  - This means we will use the preconditions from `checkStats_Contract` in their contracts
  - We will show an example in `resolveAttack` about what happens when the preconditions aren't used
- Discuss the coding & security tradeoffs between checks made in callers vs callees

## resolveAttack(character_t* target, uint32_t atk);
**Goal:** Expand upon the lessons learned with `checkStats` to get a contract ready for overrides/lemmas.

**Lessons Learned:**
- All of the points referenced in `checkStats` **Lessons Learned**
- Understand how to write cases for a function with more than 2 possible behaviors
- Understand when it is appropriate to include preconditions for functions that lack input checks
  - Goes back to the caller vs callee tradeoffs mentioned in `checkStats` **DJ's Notes**

**Errors to Explore:**
- Explain why the verification fails when the preconditions for `resolveAttack` are commented out.
  - Spoiler alert, integer overflow/underflow/wrap-around
- Explain why SAW produces a counterexample when...
  - the contract looks like:
```python
# Contract
class resolveAttack_Contract(Contract):
  def __init__(self, case : int):
    super().__init__()
    self.case = case
    # There are 3 possible cases for resolveAttack
    #   Case 2: Immediate KO
    #   Case 1: Attack mitigated
    #   Case 3: Regular attack

  def specification (self):
    # Declare variables
    (target, target_p) = ptr_to_fresh(self, alias_ty("struct.character_t"), name="target")
    atk = self.fresh_var(i32, "atk")

    # Assert the precondition that the stats are below the max stat cap
    self.precondition_f("{atk} <= `{MAX_STAT}")
    self.precondition_f("{target}.2 <= `{MAX_STAT}")
    self.precondition_f("{target}.4 <= `{MAX_STAT}")

    # Determine the preconditions based on the case parameter
    if (self.case == 1):
       # target->hp <= (atk - target->def)
      self.precondition_f("({target}.2 + {target}.4) <= {atk}")
    elif (self.case == 2):
      # target->def >= atk
      self.precondition_f("{target}.4 >= {atk}")
    else:
      # Assume any other case follows the formal attack calculation
      self.precondition_f("{target}.4 < {atk}")
      self.precondition_f("({target}.2 + {target}.4) > {atk}")
    
    # Symbolically execute the function
    self.execute_func(target_p, atk)

    # Determine the postcondition based on the case parameter
    if (self.case == 1):
      self.points_to(target_p['hp'], cry_f("0 : [32]"))
    elif (self.case == 2):
      self.points_to(target_p['hp'], cry_f("{target}.2 : [32]"))
    else:
      self.points_to(target_p['hp'], cry_f("resolveAttack ({target}.2) ({target}.4) {atk}"))

    self.returns(void)
```
  - and the source code looks like:
```c
// Source code
void resolveAttack(character_t* target, uint32_t atk)
{
  if ( target->hp <= (atk - target->def) )
  {
    // The attack will knock out the target
    target->hp = 0;
  }
  else if ( target->def >= atk)
  {
    // The target's defense mitigates the attack
    target->hp = target->hp;
  }
  else
  {
    // Calculate damage as normal
    target->hp = target->hp - (atk - target->def);
  }
}
```
  - Spoiler alert: Logical error in the source code where the first condition already covers the second condition


## selfDamage(player_t* player);
**Goal:** To provide a case study where SAW should have complained about its memory disjoint assertion being violated. Note that SAW's Python API silently resolves this issue. **Consider dropping from the SAW.md discussion**. Can still keep as a bonus example though!


## resetInventoryItems(inventory_t* inventory);
**Goal:** To show the problems SAW faces when verifying structs with pointer fields.

**Lessons Learned:**
- Consider rewriting the source code to avoid unallocated pointers as it makes SAW happy and reduces assumptions code makes (improves security)

**Errors to Explore:**
- Assuming the inventory_t struct is defined as:
```c
typedef struct {
  item_t* item;
  uint32_t numItems;
} inventory_t;
```
understand why the following contract setups fail:

Setup Attempt #1
```python
class resetInventoryItems_Contract(Contract):
  def __init__(self, numItems : int):
    super().__init__()
    self.numItems = numItems

  def specification (self):
    # Declare variables
    # Note: The setup here does not use item_p for the proof. However, item_p
    #       is included to show errors that can be encountered with the
    #       inventory_t struct.
    (item, item_p) = ptr_to_fresh(self, array_ty(self.numItems, alias_ty("struct.item_t")), name="item")
    inventory_p = self.alloc(alias_ty("struct.inventory_t"), points_to = struct(item, cry_f("{self.numItems} : [32]")))

    # Symbolically execute the function
    self.execute_func(inventory_p)

    # Assert the postconditions
    for i in range(self.numItems):
      self.points_to(inventory_p['item'][i][1], cry_f("0 : [32]"))

    self.returns(void)
```

```bash
...
      ⚠️  Failed to verify: lemma_resetInventoryItems_Contract (defined at proof/Game.py:190):
      error: types not memory-compatible:
      { %struct.item_t*, i32 }
      { [5 x { i32, i32 }], i32 }
              stdout:
```

Setup Attempt #2
```python
class resetInventoryItems_Contract(Contract):
  def __init__(self, numItems : int):
    super().__init__()
    self.numItems = numItems

  def specification (self):
    # Declare variables
    # Note: The setup here does not use item_p for the proof. However, item_p
    #       is included to show errors that can be encountered with the
    #       inventory_t struct.
    (item, item_p) = ptr_to_fresh(self, array_ty(self.numItems, alias_ty("struct.item_t")), name="item")
    inventory_p = self.alloc(alias_ty("struct.inventory_t"), points_to = struct(item_p, cry_f("{self.numItems} : [32]")))

    # Symbolically execute the function
    self.execute_func(inventory_p)

    # Assert the postconditions
    for i in range(self.numItems):
      self.points_to(inventory_p['item'][i][1], cry_f("0 : [32]"))

    self.returns(void)
```

```bash
      ⚠️  Failed to verify: lemma_resetInventoryItems_Contract (defined at proof/Game.py:190):
      error: typeOfSetupValue: llvm_elem requires pointer to struct or array, found %struct.item_t**
              stdout:
    Considering both of these verification setup attempts, we can see that
    defining inventory_t with an item_t pointer is tough for SAW to setup and.
    prove. Consequently, it is better to use fixed array lengths for structs!
```


## initScreen(screen_t* screen, uint8_t assetID);
**Goal:** To provide a case study where SAW should have complained about not knowing what nested defines resolve to. Note that SAW's Python API silently resolves this issue. **Consider dropping from the SAW.md discussion**. Can still keep as a bonus example though!


## setScreenTile(screen_t* screen, uint32_t screenIdx, uint32_t tableIdx);
**Goal:** Demonstrate how to initialize an extern global array.

**Lessons Learned:**
- How to handle extern global variables when they aren't linked into the generated bitcode
  - Copy extern definition to Cryptol spec file
  - Could be possible to ignore the initialization if the bitcode links the file that implements the extern global (testing required)
- Understand when `ptr_to_fresh` vs `self.alloc` is needed
- Repointing a pointer to a SAW variable (`screen_post`) in order to use that variable for postconditions

**Errors to Explore:**
- Why can't we just declare and pass the screen pointer, `screen_p`?
```python
class setScreenTile_Contract(Contract):
  def __init__(self, shouldPass : bool):
    super().__init__()
    self.shouldPass = shouldPass

  def specification (self):
    # Declare variables
    screen_p = self.alloc(alias_ty("struct.screen_t"))
    screenIdx = self.fresh_var(i32, "screenIdx")
    tableIdx  = self.fresh_var(i32, "tableIdx")

    # Initialize Game.h's assetTable according to Assets.c
    self.points_to(global_var("assetTable"), cry_f("assetTable"))

    # Assert preconditions depending on the Contract parameter
    if (self.shouldPass):
      self.precondition_f("{screenIdx} < {SCREEN_TILES}")
      self.precondition_f("{tableIdx}  < {ASSET_TABLE_SIZE}")
    else:
      # Note: Only one of the following preconditions is needed
      self.precondition_f("{screenIdx} >= {SCREEN_TILES}")
      self.precondition_f("{tableIdx}  >= {ASSET_TABLE_SIZE}")
    
    # Symbolically execute the function
    self.execute_func(screen_p, screenIdx, tableIdx)

    # Since we just want to check one index, let's have our screen pointer
    # point to a new screen_t variable.
    screen_post = self.fresh_var(alias_ty("struct.screen_t"), "screen_post")

    # Assert that the original screen pointer now points to the new screen_t
    # variable. This will allow us to reference screen_post in Cryptol for our
    # later postconditions.
    self.points_to(screen_p, screen_post)

    # Assert postconditions depending on the Contract parameter
    if (self.shouldPass):
      self.postcondition_f("({screen_post}@{screenIdx}) == assetTable@{tableIdx}")
      self.returns_f("`({SUCCESS}) : [32]")
    else:
      self.returns_f("`({FAILURE}) : [32]")
```


## quickBattle(player_t* player, character_t* opponent);



## counterBattle(player_t* player, character_t* opponent);
Note: No Contract for this one. Considering dropping due to how complex the Contract needs to be (multiple possible outcomes depending on the inputs - requires specific preconditions and postconditions)


# TODO
- Polish comments in all Game/ files
  - Remove unused code when appropriate
- Define the background of the library (structs, functions, etc)
- Cryptol record/tuples ('(') for initDefaultPlayer
- Add `selfDamage` prototype to `Game.h`
- Remove the first 3 preconditions in `selfDamage_Contract` given that they were used for debugging and are no longer needed
- Move `quickBattle` to come right after `selfDamage`, which is after `resolveAttack` given that it makes sense in the lesson plan
- Consider moving `resetInventoryItems`, `initScreen`, and `setScreenTile` earlier in the lesson plan
  - Recall that `setScreenTile` shows extern global variables --> move after `getDefaultLevel` to continue the global variable discussion
- Confirm the error cases yield the expected error results
  - Necessary because some of these errors were encountered and resolved already.
  - Want to properly recreate them for learning purposes.
- Determine if the resolveAttack logical error should be kept
- Determine if some functions should be dropped...
  - Entirely: `counterBattle`
    - It has so many SAW behaviors to consider (so fairly complicated), and I have not written a SAW contract for it
  - Just from SAW.md: `selfDamage`
    - Intended to be an example of field aliasing and show SAW complaining about "Memory not disjoint", but the Python API resolves it
  - Just from SAW.md: `initScreen`
    - Intended to be an example of where SAW cannot resolve nested defines, but the Python API resolves it
