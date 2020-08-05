```cryptol
module scripts::Test::CorrectSat where

orSelf : Bit -> Bit
property orSelf x = x \/ ~x
```

```icry
Cryptol> :m scripts::Test::CorrectSat
Loading module Cryptol
Loading module Cryptol
Loading module scripts::Test::CorrectSat
scripts::Test::CorrectSat> :sat orSelf
Satisfiable
orSelf False = True
(Total Elapsed Time: 0.013s, using "Z3")
```
