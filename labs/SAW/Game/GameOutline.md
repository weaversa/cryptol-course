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


## resetInventoryItems(inventory_t* inventory);


## initScreen(screen_t* screen, uint8_t assetID);


## setScreenTile(screen_t* screen, uint32_t screenIdx, uint32_t tableIdx);


## quickBattle(player_t* player, character_t* opponent);


## counterBattle(player_t* player, character_t* opponent);
Note: No Contract for this one. Considering dropping due to how complex the Contract needs to be (multiple possible outcomes depending on the inputs - requires specific preconditions and postconditions)


# TODO
- Add `selfDamage` prototype to `Game.h`
- Move `quickBattle` to come right after `selfDamage`, which is after `resolveAttack` given that it makes sense in the lesson plan
- Consider moving `resetInventoryItems`, `initScreen`, and `setScreenTile` earlier in the lesson plan
  - Recall that `setScreenTile` shows extern global variables
- Confirm the error cases yield the expected error results
  - Necessary because some of these errors were encountered and resolved already.
  - Want to properly recreate them for learning purposes.
- Determine if the resolveAttack logical error should be kept
- Determine if counterBattle should be kept
  - Current thoughts are no given how many behavior states must be considered
