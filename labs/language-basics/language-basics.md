Maybe grab the lang specifuc parts of Jacob's overview


# Basic Use of the Cryptol Language

For examples in this lab I have turned off the warning messages you get when not specifying bit sizes of numbers. This is **not** something you should do when you're new at Cryptol. (In fact, I don't do it except when teaching.)

```sh
Cryptol> :set warnDefaulting = off
```

## Writing Loops

### Enumerations provide the indices to loops

```sh
Cryptol> [1..10]
[1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
Cryptol> [1, 3..10]
[1, 3, 5, 7, 9]
```

### You can have "infinite" enumerations with `...`

```sh
Cryptol> [1...]
[1, 2, 3, 4, 5, ...]
```


### Loops to accumulate a value are simple calculations over indices

```sh
Cryptol> sum [1..100]
5050
```

### Avoid loops when you're lucky

Most binary operators in Cryptol operate on any type compatable objects.

```sh
Cryptol> [[[2,3],[5,7]],[[11,13],[17,19]]] + [[[0,1],[1,2]],[[3,5],[8,13]]]
[[[2, 4], [6, 9]], [[14, 18], [25, 32]]]
```

so you don't need to write a double indexed loop to add these two "arrays".


## Laziness



## Small functions

* Easy to test
* Leverage properties




## Don't let the type system do your work

Cryptol's type system tries to infer the types of functions lacking a type signature. Sometimes it comes up with a general type than you presumed. This causes problems:

* Perhaps you only want your function to be applicable on a smaller set of types. (usually minor, but occasionally major)
* Makes error messages even more incomprehensible! (major)

## Do let the type system work for you

Type signatures for functions are wonderful bits of documentation. It is much easier to see what's going on if you use type synonyms and signatures. 

* makes cleaner code
* easier for other tools to consume/reason about

## Not for faint of heart surely I'll omit this

* Sructural recursion (binaty gcd)
