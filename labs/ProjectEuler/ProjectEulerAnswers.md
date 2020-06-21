```
module labs::ProjectEuler::ProjectEulerAnswers where

import labs::ProjectEuler::cipher1
import labs::ProjectEuler::keylog
import labs::ProjectEuler::cipher2
```

### [Problem 9](https://projecteuler.net/problem=9)

> A Pythagorean triplet is a set of three natural numbers, a < b < c,
> for which a^2 + b^2 = c^2.
>
> For example, 3^2 + 4^2 = 9 + 16 = 25 = 5^2.
>
> There exists exactly one Pythagorean triplet for which a + b +
> c = 1000. Find this triple.

```
pythagoreantriple : Integer -> Integer -> Integer -> Bit
property pythagoreantriple a b c =
    a^^2 + b^^2 == c^^2 /\
    a + b + c == 1000   /\
    a > 0 /\ b > 0 
```


### [Problem 34](https://projecteuler.net/problem=34)

> 145 is a curious number, as 1! + 4! + 5! = 1 + 24 + 120 = 145.
>
> Find all numbers which are equal to the sum of the factorial of
> their digits.  Note: as 1! = 1 and 2! = 2 are not sums they are not
> included.

(Aside: these numbers are called
[factorions](https://en.wikipedia.org/wiki/Factorion).

*Hints*: the factorial function is usually defined recursively, but
 that tends to make SAT solving difficult. Since you only need to
 calculate the factorial of the numbers 0-9, make your function just
 do a case by case calculation. To get the digital representation of
 the number, create a function which takes in a number and a list of
 numbers and returns `True` exactly when the list is the base 10
 representation. Finally, it can be shown that the most number of
 digits a factorion can have is 6.

```
factorial :
    {b}
    (Arith b, Cmp b, Literal 362880 b) =>
    b -> b
factorial n = if n == 2 then      2 else
	      if n == 3 then      6 else
	      if n == 4 then     24 else
	      if n == 5 then    120 else
	      if n == 6 then    720 else
	      if n == 7 then   5040 else
	      if n == 8 then  40320 else
	      if n == 9 then 362880 else
	      1

powersoften :
    {a}
    (Arith a, Literal 10 a) =>
    [inf]a
powersoften = [1] # [ 10 * i | i <- powersoften ]

alldigits :
    {a, b}
    (fin a, Arith b, Cmp b, Literal 10 b) =>
    [a]b -> Bit
alldigits l = [ 0 <= i /\ i < 10 | i <- l ] == ~0

matchdigits :
    {n, a}
    (Arith a, Literal 10 a, fin n) =>
    [n]a -> [n]a
matchdigits l =
    [ i * t
    | i <- reverse l
    | t <- powersoften ]

formnumber :
    {a, b}
    (fin a, Arith b, Literal 10 b) =>
    [a]b -> b
formnumber l =
    sum (matchdigits l)

basetenrep :
    {a, b}
    (fin a, a >= 1, Arith b, Cmp b, Literal 10 b) =>
    b -> [a]b -> Bit
basetenrep n l =
    n == formnumber l /\
    alldigits l       /\
    head l != 0

sumfactorial :
    {a, b}
    (fin a, Arith b, Cmp b, Literal 362880 b) =>
    [a]b -> b
sumfactorial l = sum [ factorial i | i <- l ]

factorionprop :
    {a, b}
    (fin a, a >= 1, Arith b, Cmp b, Literal 362880 b) =>
    b -> [a]b -> Bit
property factorionprop n l =
    basetenrep n l       /\
    sumfactorial l == n
```

```sh
labs::ProjectEuler::ProjectEulerAnswers> :s satNum=all
labs::ProjectEuler::ProjectEulerAnswers> :sat factorionprop`{1, Integer}
factorionprop`{1, Integer} 2 [2] = True
factorionprop`{1, Integer} 1 [1] = True
labs::ProjectEuler::ProjectEulerAnswers> :sat factorionprop`{2, Integer}
Unsatisfiable
labs::ProjectEuler::ProjectEulerAnswers> :sat factorionprop`{3, Integer}
factorionprop`{3, Integer} 145 [1, 4, 5] = True
labs::ProjectEuler::ProjectEulerAnswers> :sat factorionprop`{4, Integer}
Unsatisfiable
labs::ProjectEuler::ProjectEulerAnswers> :sat factorionprop`{5, Integer}
factorionprop`{5, Integer} 40585 [4, 0, 5, 8, 5] = True
labs::ProjectEuler::ProjectEulerAnswers> :sat factorionprop`{6, Integer}
Unsatisfiable
```

*Note*: The runtimes are not being reported correctly so we've removed
 them from the demonstration. Sorry!


### [Problem 36](https://projecteuler.net/problem=36)

> The decimal number, 585 = 1001001001 (binary), is
> [palindromic](https://www.dictionary.com/browse/palindromic) in both
> bases. Find at least three numbers which are palindromic in base 10
> and base 2. (Please note that the palindromic number, in either
> base, may not include leading zeros.)

```
carrymult :
    {a}
    (fin a) =>
    [a] -> [a] -> Bit
carrymult m n = toInteger (m*n) != (toInteger m) * (toInteger n)

carrylist :
    {a, b}
    (fin a, fin b) =>
    [a][b] -> Bit
carrylist l = [ carry s i | s <- sums | i <- l ] != 0
    where
     sums = [0] # [ i + j | i <- l | j <- sums ]

doublepalindrome :
    {a, b}
    (fin a, a >= 1, fin b, b >= 4) =>
    [b] -> [a][b] -> Bit
property doublepalindrome x l =
    basetenrep x l               /\
    nocarryprods                 /\
    ~(carrylist (matchdigits l)) /\
    reverse l == l               /\
    reverse x == x               /\
    x@0
     where
      nocarryprods = [ carrymult i p | i <- l | p <- powersoften ] == 0
```

```sh
labs::ProjectEuler::ProjectEulerAnswers> :s base=10
labs::ProjectEuler::ProjectEulerAnswers> :s satNum=all
labs::ProjectEuler::ProjectEulerAnswers> :sat doublepalindrome`{3, 9}
doublepalindrome`{3, 9} 313 [3, 1, 3] = True
labs::ProjectEuler::ProjectEulerAnswers> :sat doublepalindrome`{3, 10}
doublepalindrome`{3, 10} 585 [5, 8, 5] = True
doublepalindrome`{3, 10} 717 [7, 1, 7] = True
...
```


### [Problem 43](https://projecteuler.net/problem=43)

> The number, 1406357289, is a 0 to 9 pandigital number because it is
> made up of each of the digits 0 to 9 in some order, but it also has
> a rather interesting sub-string divisibility property.
>
> Let d_(1) be the 1^(st) digit, d_(2) be the 2^(nd) digit, and so on. In this way, we note the following:
>
>    * d_(2)d_(3)d_(4)=406 is divisible by 2
>    * d_(3)d_(4)d_(5)=063 is divisible by 3
>    * d_(4)d_(5)d_(6)=635 is divisible by 5
>    * d_(5)d_(6)d_(7)=357 is divisible by 7
>    * d_(6)d_(7)d_(8)=572 is divisible by 11
>    * d_(7)d_(8)d_(9)=728 is divisible by 13
>    * d_(8)d_(9)d_(10)=289 is divisible by 17
>
> Find at least two 0 to 9 pandigital numbers with this property.

```
listhasdigit :
    {a, b}
    (fin a, a >=1, Cmp b) =>
    [a]b -> b -> Bit
listhasdigit l n = [ i == n | i <- l ] != 0

hasalldigits :
    {a}
    (Cmp a, Literal 10 a, Literal 9 a) =>
    [10]a -> Bit
hasalldigits l =
    [ listhasdigit l i | i <- [0..9] ] == ~0

pandigital : Integer -> [10]Integer -> Bit
pandigital n l =
    basetenrep n l  /\
    hasalldigits l  /\
    n234 % 2  == 0  /\
    n345 % 3  == 0  /\
    n456 % 5  == 0  /\
    n567 % 7  == 0  /\
    n678 % 11 == 0  /\
    n789 % 13 == 0  /\
    n890 % 17 == 0
     where
      n234 = formnumber (l @@ ([1,2,3] : [3][16]))
      n345 = formnumber (l @@ ([2,3,4] : [3][16]))
      n456 = formnumber (l @@ ([3,4,5] : [3][16]))
      n567 = formnumber (l @@ ([4,5,6] : [3][16]))
      n678 = formnumber (l @@ ([5,6,7] : [3][16]))
      n789 = formnumber (l @@ ([6,7,8] : [3][16]))
      n890 = formnumber (l @@ ([7,8,9] : [3][16]))
```

```sh
labs::ProjectEuler::ProjectEulerAnswers> :s satNum=all
labs::ProjectEuler::ProjectEulerAnswers> :s base=10
labs::ProjectEuler::ProjectEulerAnswers> :sat pandigital
pandigital 1430952867 [1, 4, 3, 0, 9, 5, 2, 8, 6, 7] = True
pandigital 4130952867 [4, 1, 3, 0, 9, 5, 2, 8, 6, 7] = True
pandigital 1406357289 [1, 4, 0, 6, 3, 5, 7, 2, 8, 9] = True
pandigital 4106357289 [4, 1, 0, 6, 3, 5, 7, 2, 8, 9] = True
pandigital 4160357289 [4, 1, 6, 0, 3, 5, 7, 2, 8, 9] = True
pandigital 1460357289 [1, 4, 6, 0, 3, 5, 7, 2, 8, 9] = True
...
```

### [Problem 52](https://projecteuler.net/problem=52)

> It can be seen that the number, 125874, and its double, 251748,
> contain exactly the same digits, but in a different order.
>
> Find the smallest positive integer, x, such that 2x, 3x, 4x, 5x, and
> 6x, all contain the same digits.

```
twolistssamedigits :
    {a, b}
    (fin a, a >=1, Cmp b) =>
    [a]b -> [a]b -> Bit
twolistssamedigits l1 l2 =
    [ listhasdigit l1 i | i <- l2 ] == ~0

productdigits :
    {a, b}
    (fin a, a >= 1, Cmp b, Arith b, Literal 10 b) =>
    b -> [6][a]b -> Bit
property productdigits n ls =
    basetenrep n l1                 /\
    alltwolists                     /\
    allforms                        /\
    [ alldigits l | l <- ls ] == ~0
  where
    [l1, l2, l3, l4, l5, l6] = ls
    tls = tail ls
    alltwolists = [ twolistssamedigits l1 l
                  | l <- tls ] == ~0
    allforms = [ formnumber li == i * n
               | li <- tls
               | i <- [2..6] ] == ~0
```

```sh
labs::ProjectEuler::ProjectEulerAnswers> :sat productdigits`{6, [32]}
productdigits`{6, [32]}
  142857
  [[1, 4, 2, 8, 5, 7], [2, 8, 5, 7, 1, 4], [4, 2, 8, 5, 7, 1],
   [5, 7, 1, 4, 2, 8], [7, 1, 4, 2, 8, 5], [8, 5, 7, 1, 4, 2]] = True
```

### [Problem 59](https://projecteuler.net/problem=59) (Modified)

> Each character on a computer is assigned a unique code and the
> preferred standard is
> [ASCII](https://en.wikipedia.org/wiki/ASCII). For example, uppercase
> A = 65, asterisk (*) = 42, and lowercase k = 107.
>
> A modern encryption method is to take a text file, convert the bytes
> to ASCII, then XOR each byte with a given value, taken from a secret
> key. The advantage with the XOR function is that using the same
> encryption key on the cipher text, restores the plain text; for
> example, 65 XOR 42 = 107, then 107 XOR 42 = 65.
>
> For unbreakable encryption, the key is the same length as the
> plaintext message, and the key is made up of random bytes. The user
> would keep the encrypted message and the encryption key in different
> locations, and without both "halves", it is impossible to decrypt
> the message.
>
> Unfortunately, this method is impractical for most users, so the
> modified method is to use a password as a key. If the password is
> shorter than the message, which is likely, the key is repeated
> cyclically throughout the message. The balance for this method is
> using a sufficiently long password key for security, but short
> enough to be memorable.
>
> Your task has been made easy, as the encryption key consists of
> three lower case characters. Using cipher1.cry, a file containing
> the encrypted ASCII codes, and the knowledge that the plain text
> must contain common English words, decrypt the message and find the
> sum of the ASCII values in the original text.
>

Note: cipher1.cry contains a different cipher encrypted under a
different key from the original. The original Project Euler problem
can be found in cipher2.cry.

```
containsWords :
    {a, b}
    (fin a, a >= 2, fin b) =>
    [a]Char -> [b][3]Char -> Bit
containsWords l ws = [ checkword w | w <- ws ] == ~0
  where
    checkword w = [ c0 == w@0 /\
    	       	    c1 == w@1 /\
		    c2 == w@2
		  | c0 <- l
		  | c1 <- tail l
		  | c2 <- tail (tail l) ] != 0

isLowercase : Char -> Bit
isLowercase c = c >= 0x61 /\ c <= 0x7a

isLowercaseStr : {a} (fin a) => [a]Char -> Bit
isLowercaseStr s = [ isLowercase c | c <- s ] == ~0

XORtowords :
    {a}
    (fin a, a >= 2) =>
    [a]Char -> [3]Char -> Bit
XORtowords ciphertext key =
    containsWords ct [ "the", "and", "are" ] /\
    isLowercaseStr key
  where
    keys = [ key ] # [ k | k <- keys ]
    jkeys = join keys
    ct = ciphertext ^ (take jkeys)

decrypt :
    {a}
    (fin a, a >= 2) =>
    [a]Char -> [3]Char -> [a]Char
decrypt s key = s ^ (take ks)
  where
    ks = join keys
    keys = [ key ] # [ k | k <- keys ]
```

```sh
labs::ProjectEuler::ProjectEulerAnswers> :s ascii=on
labs::ProjectEuler::ProjectEulerAnswers> :sat XORtowords cipher1
XORtowords cipher1 "abe" = True
XORtowords cipher1 "aba" = True
labs::ProjectEuler::ProjectEulerAnswers> decrypt cipher1 "aba"
"Four score and seven years ago our fathers brought forth on this continent, a new nation, conceived in Liberty, and dedicated to the proposition that all men are created equal.  Now we are engaged in a great civil war, testing whether that nation, or any nation so conceived and so dedicated, can long endure. We are met on a great battle-field of that war. We have come to dedicate a portion of that field, as a final resting place for those who here gave their lives that that nation might live. It is altogether fitting and proper that we should do this.  But, in a larger sense, we can not dedicate-we can not consecrate-we can not hallow-this ground. The brave men, living and dead, who struggled here, have consecrated it, far above our poor power to add or detract. The world will little note, nor long remember what we say here, but it can never forget what they did here. It is for us the living, rather, to be dedicated here to the unfinished work which they who fought here have thus far so nobly advanced. It is rather for us to be here dedicated to the great task remaining before us-that from these honored dead we take increased devotion to that cause for which they gave the last full measure of devotion-that we here highly resolve that these dead shall not have died in vain-that this nation, under God, shall have a new birth of freedom-and that government of the people, by the people, for the people, shall not perish from the earth."
```


### [Problem 79](https://projecteuler.net/problem=79)

> A common security method used for online banking is to ask the user
> for three random characters from a passcode. For example, if the
> passcode was 531278, they may asked for the 2nd, 3rd, and 5th
> characters; the expected reply would be: 317.
>
> The text file, keylog.cry, contains fifty successful login attempts.
>
> Given that the three characters are always asked for in order,
> analyse the file so as to determine the shortest possible secret
> passcode of unknown length.

```
passcode :
    {a}
    (fin a, a >= 1) =>
    [a]Integer -> Bit
passcode l = [ loop l kl != 0 | kl <- keylog ] == ~0
  where 
    loop ll kll = [ [ ll@(i:[a]), ll@j, ll@k] == kll /\
	            i < j                            /\
	            j < k
	          | i <- [0..a-1],
	     	    j <- [0..a-1],
	   	    k <- [0..a-1] ]
```

```sh
labs::ProjectEuler::ProjectEulerAnswers> :s base=10
labs::ProjectEuler::ProjectEulerAnswers> :sat passcode`{1}
Unsatisfiable
labs::ProjectEuler::ProjectEulerAnswers> :sat passcode`{2}
Unsatisfiable
labs::ProjectEuler::ProjectEulerAnswers> :sat passcode`{3}
Unsatisfiable
labs::ProjectEuler::ProjectEulerAnswers> :sat passcode`{4}
Unsatisfiable
labs::ProjectEuler::ProjectEulerAnswers> :sat passcode`{5}
Unsatisfiable
labs::ProjectEuler::ProjectEulerAnswers> :sat passcode`{6}
Unsatisfiable
labs::ProjectEuler::ProjectEulerAnswers> :sat passcode`{7}
Unsatisfiable
labs::ProjectEuler::ProjectEulerAnswers> :sat passcode`{8}
passcode`{8} [7, 3, 1, 6, 2, 8, 9, 0] = True
```

### Throwback

> Find a four-digit number (greater than 999 and less than 10000) such
> that the least significant four digits of the square is that number.
>
> EXTRA CHALLENGE:
> What about five-digit numbers? Other numbers of digits?

```sh
labs::ProjectEuler::ProjectEulerAnswers> :sat \(x : [32]) -> x > 999 /\ x ^^ 2 % 10000 == x
(\(x : [32]) -> x > 999 /\ x ^^ 2 % 10000 == x) 9376 = True
```

```
squaredrop :
    {a}
    (fin a, a>=2, a%2 == 0) =>
    Integer -> [a]Integer -> Bit
squaredrop n l =
    (basetenrep (n*n) l) /\
    formnumber (drop`{back = ((a)/2)} l) == n
```

```sh
labs::ProjectEuler::ProjectEulerAnswers> :sat squaredrop`{8}
squaredrop`{8} 9376 [8, 7, 9, 0, 9, 3, 7, 6] = True
```
