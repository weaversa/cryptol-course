# N-Queens

## Installation and Loading

To use Cryptol, you must first [install](../../INSTALL.md) it. To 
load this document into Cryptol, change to your `cryptol-course` 
directory in a terminal (Linux) or command prompt (Windows), then 
run Cryptol via a locally installed binary or Docker image:

### Linux

```sh
.../cryptol-course> cryptol labs/Demos/NQueens.md
...

Loading module Cryptol
Loading module labs::Demos::NQueens
labs::Demos::NQueens> 
```

Alternatively, you can use the `:m` or `:l` command from within 
Cryptol to load this document. (To avoid errors, make sure Cryptol 
was started in the `cryptol-course` directory). 

```sh
.../cryptol-course> cryptol
...

Loading module Cryptol
Cryptol> :m labs::Demos::NQueens
Loading module labs::Demos::NQueens
labs::Demos::NQueens> 
```

### Windows

```sh
.../cryptol-course> cryptol labs/Demos/NQueens.md
...

Loading module Cryptol
Loading module labs::Demos::NQueens
labs::Demos::NQueens> 
```

Alternatively, you can use the `:m` or `:l` command from within 
Cryptol to load this document. (To avoid errors, make sure Cryptol 
was started in the `cryptol-course` directory). 

```sh
...\cryptol-course> cryptol
...

Loading module Cryptol
Cryptol> :m labs::Demos::NQueens
Loading module labs::Demos::NQueens
labs::Demos::NQueens> 
```

### Docker (Linux)
```sh
.../cryptol-course> docker run --read-only --mount type=bind,src=$(pwd),dst=/mnt/cryptol-course --env CRYPTOLPATH=/mnt/cryptol-course -it cryptolcourse/cryptol
Loading module Cryptol
Cryptol> :m labs::Demos::NQueens
Loading module labs::Demos::NQueens
labs::Demos::NQueens> 
```

### Docker (Windows)
```sh
...\cryptol-course> docker run --read-only --mount type=bind,src=%CD%,dst=/mnt/cryptol-course --env CRYPTOLPATH=/mnt/cryptol-course -it cryptolcourse/cryptol
Loading module Cryptol
Cryptol> :m labs::Demos::NQueens
Loading module labs::Demos::NQueens
labs::Demos::NQueens> 
```

## Overview


Cryptol is not just for crypto. Here, we demonstrate how Cryptol can 
solve the [N-Queens 
puzzle](https://en.wikipedia.org/wiki/Eight_queens_puzzle). This demo 
draws from and cites [Galois, Inc.'s 
example](https://github.com/GaloisInc/cryptol/blob/master/examples/funstuff/NQueens.cry)
[1].

```
module labs::Demos::NQueens where
```

Before proceeding, we define some helper functions. `product` returns 
the [Cartesian 
product](https://en.wikipedia.org/wiki/Cartesian_product) of two 
sequences:

```
/** Cartesian product of sequences `X` and `Y` */
product : {n, m, a, b} (fin m) => [n]a -> [m]b -> [n * m](a, b)
product X Y = [ (i, j) | i <- X, j <- Y ]
```

`distinct` checks whether each element in a sequence is unique. We 
will use this to determine whether queen positions (represented as a 
sequence of column positions within each row of the board) are 
unique.  (If not, then at least two queens are in the same column, 
violating the N-Queens constraint.)

```
/** whether sequence `X` comprises unique items */
distinct : {a, n} (Cmp a, fin n, n >= 1) => [n]a -> Bit
distinct X = U == zero
  where
    M = map ((>>>) (1:[n])) [(1 : [n])...]
    U = foldl (||) zero [ (map ((==) x) X) ^ m | x <- X | m <- M ]
```

There are many interesting ways to write this function.

This `distinct` function works roughly as follows:
- For each element:
  + Map matching elements (one bit per sequence item)
  + Clear the self-matching bit (using rotating bitmask `M`)
- Compute bitwise-or over remaining matches.
  (Remaining matches reflect duplicates.)
- Return `True` iff no matches remain.

(The counterpart in [1] works roughly as follows:
- Enumerate an index over the sequence.
- Check for equality only over positions in index order (emulate a 
  triangular search).
- Return `True` if any duplicate was found.)

(Neither approach short circuits when a duplicate is found, so both 
end up with comparable performance for `nQueens`.)

Next, we declare some type aliases to represent a chess `Board` and 
the `Position`s of queens on the board. A (proposed) `Solution` is a 
`Board` that meets (or violates) the N-Queens constraint.

```
/** row or column position in [0, `n`) */
type Position n = [width (n-1)]

/** `n` x `n` square chess board */
type Board n = [n](Position n)

/** `n`-queens solution */
type Solution n = Board n -> Bit
```

(Note: `Position` could be defined using modular integers `Z`, but its 
dual use for sequence indexing severely hinders performance for 
`nQueens`.)

Next, we define a function to check whether two queens on the board 
can "see" each other diagonally:

```
/**
 * whether queens in rows `i` and `j` of `Q` can see each other
 * diagonally
 */
checkDiag : 
    {n} (fin n, n >= 1) => Board n -> (Position n, Position n) -> Bit
checkDiag Q (i, j) =
    (i < j) ==> (diffR != diffC)
  where
    qi = Q @ i
    qj = Q @ j
    diffR = if qi >= qj then qi - qj else qj - qi
    diffC = j - i                   // we know i < j
```

(This mostly follows [1], but swaps out eager 
`||` for lazy `==>`; this doesn't matter much.)

Next, we follow suit from [1] and define a function to return all 
possible row/column positions on a board (the Cartesian product of 
board positions):

```
/** board positions (Cartesian product of row `Position`s) */
ijs : {n} (fin n, n >= 1) => [_](Position n, Position n)
ijs = product P P
  where
    P = [0 .. (n-1) : Position n]
```

As in [1], we define a function returning whether a `Position` within 
a row is actually on the board:

```
/** whether `x` is in a valid row `Position` of `Board` `Q` */
inRange : {n} (fin n, n >= 1) => Board n -> Position n -> Bit
inRange Q x = x <= `(n - 1)
```

We can now define the N-Queens constraint: a `Board` is a `Solution` 
to N-Queens iff all queens are on the board and cannot see each 
other:

```
/** whether `Board` `Q` satisfies the N-Queens constraint */
nQueens : {n} (fin n, n >= 1, width (n-1) > 1) => Solution n
nQueens Q =
    all (inRange Q) Q /\          // all queens are on board
    distinct Q /\                 // no queens are in same column
    all (checkDiag Q) ijs`{n}     // no queens are on same diagonal
```

The instructions for [1] also work here:

> To see this in action, try:
> 
> ```sh
> > :sat nQueens : (Solution n)
> ```
> where n is the board size.
> 
> You may find that `cvc4` takes a long time for solutions bigger than 5.
> For those sizes, we have had good luck with both `yices` and `Z3`.
> 
> To do that,
> 
> ```sh
> > :set prover=z3
> ```
> 
> or
> 
> ```sh
> > :set prover=yices
> ```

Here is one possible outcome for `n = 8`:

```sh
labs::Demos::NQueens> :s base=10
labs::Demos::NQueens> :sat nQueens : (Solution 8)
(nQueens : Solution 8)
  [1, 7, 5, 0, 2, 4, 6, 3] = True
(Total Elapsed Time: 0.053s, using "Z3")
```

This corresponds to the following arrangement (diagram produced by 
[chessboard.js](https://chessboardjs.com/)) ([Solution 
8](https://en.wikipedia.org/wiki/Eight_queens_puzzle#Solutions) in 
the Wikipedia article):

<img src="NQueensSolution.png" alt="Solution to 8-Queens Puzzle">
