# Game DLC

This directory contains additional Game functions and SAW contracts for you to reference. You will notice:
- `Makefile`: Provides the necessary steps to generate our bitcode and run our SAW Python scripts.
- `src/`: Contains the source code we'll be analyzing.
- `proof/`: Contains our Python scripts to run our SAW contracts.
- `specs/`: Contains our Cryptol specs that our SAW contracts can call.

# DLC Functions

Below is a list of functions included in src/.

## `uint32_t levelUp(uint32_t level)`

**Goal:** Set up a SAW contract to verify a simple function.

**Lessons Learned:**
- How to use `fresh_var` to represent `uint32_t` variables in a contract
- How to call a Cryptol function in a contract
- How to pass variables defined in the SAW contract to a Cryptol function (curly braces)


## `uint32_t initDefaultPlayer(player_t* player)`

**Goal:** Represent and initialize C structs in a SAW contract.

**Lessons Learned:**
- How to use `alias_ty` to represent structs in a contract
- Understand that SAW only recognizes the base struct name (`player_t` vs `character_t`)
- How and when to use `alloc`
  - Use `alloc` when you only consider the pointer
  - Use `ptr_to_fresh` when you care about the values being referenced (i.e. for pre/postconditions) and the pointer
- Passing global variables/parameters/defines defined in SAW to contracts
  - Values copied from the source code's defines
  - Examples: `MAX_NAME_LENGTH`, `SUCCESS`
- How to assert postconditions using `points_to`
- Compiling clang with the `-g` flag provides debug symbols, so you can reference fields names rather than just indices


// TODO: Update this section
## `uint32_t initDefaultSprite(character_t* character, sprite_t* sprite)`

**Goal:** Set up a struct


## checkStats(character_t* character)
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

**Additional Notes:**
- Note that the `checkStats` function would be used in the Game library to check character stats every instance before such stats would be referenced/used for gameplay.
- In terms of the example, `checkStats` provides gameplay balancing by limiting how well characters can perform.
- Given this policy, all other functions included in the library assume that `checkStats` is called before them.
  - This means we will use the preconditions from `checkStats_Contract` in their contracts
  - We will show an example in `resolveAttack` about what happens when the preconditions aren't used
- Discuss the coding & security tradeoffs between checks made in callers vs callees


## resolveAttack(character_t* target, uint32_t atk)
**Goal:** Expand upon the lessons learned with `checkStats` to get a contract ready for overrides/lemmas.

**Lessons Learned:**
- All of the points referenced in `checkStats` **Lessons Learned**
- Understand how to write cases for a function with more than 2 possible behaviors
- Understand when it is appropriate to include preconditions for functions that lack input checks
  - Goes back to the caller vs callee tradeoffs mentioned in `checkStats` **Additional Notes**


// TODO: Expand & discuss background with SAWscripts
## selfDamage(player_t* player)
**Goal:** To provide a case study where SAW should have complained about its memory disjoint assertion being violated. Note that SAW's Python API silently resolves this issue.


## quickBattle(player_t* player, character_t* opponent)
**Goal:** To show how to pass overrides (lemmas) to a Unit test.

**Lessons Learned:**
- Must pass preconditions that match the preconditions included in the passed overrides

**Additional Notes:**
- The function assumes that both the player and opponent have non-zero HP values, otherwise there wouldn't be a battle!
  - Explains why the `> 0` preconditions exist


## getDefaultLevel()
**Goal:** Determine how to handle global variables in a SAW contract.

**Lessons Learned:**
- How and when to use `global_initializer`
  - When the source code already initializes the value and SAW should just use that value
- How to declare a global variable in SAW (`global_var`)
- How to set a global variable to an initialized value in a SAW contract (`points_to`)
- When to pass no inputs to `execute_func`


## initScreen(screen_t* screen, uint8_t assetID)
**Goal:** To provide a case study where SAW should have complained about not knowing what nested defines resolve to. Note that SAW's Python API silently resolves this issue.


## setScreenTile(screen_t* screen, uint32_t screenIdx, uint32_t tableIdx)
**Goal:** Demonstrate how to initialize an extern global array.

**Lessons Learned:**
- How to handle extern global variables when they aren't linked into the generated bitcode
  - Copy extern definition to Cryptol spec file
  - Could be possible to ignore the initialization if the bitcode links the file that implements the extern global (testing required)
- Understand when `ptr_to_fresh` vs `self.alloc` is needed
- Repointing a pointer to a SAW variable (`screen_post`) in order to use that variable for postconditions


// TODO: Reevaluate position based on work done with initDefaultSprite()
## resetInventoryItems(inventory_t* inventory)
**Goal:** To show the problems SAW faces when verifying structs with pointer fields.

**Lessons Learned:**
- Consider rewriting the source code to avoid unallocated pointers as it makes SAW happy and reduces assumptions code makes (improves security)
