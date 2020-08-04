```cryptol
module scripts::Test::CorrectUnsat where

andSelf : Bit -> Bit
property andSelf x = x /\ ~x
```

```icry
Cryptol> :m scripts::Test::CorrectUnsat
Loading module Cryptol
Loading module scripts::Test::CorrectUnsat
scripts::Test::CorrectUnsat> :sat \(x:Bit) -> x /\ (~x)
Unsatisfiable
(Total Elapsed Time: 0.005s, using Z3)
```
