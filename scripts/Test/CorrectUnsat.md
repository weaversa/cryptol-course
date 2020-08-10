```cryptol
module scripts::Test::CorrectUnsat where

andSelf : Bit -> Bit
property andSelf x = x /\ ~x
```

```Xcryptol session
Cryptol> :m scripts::Test::CorrectUnsat
Loading module Cryptol
Loading module Cryptol
Loading module scripts::Test::CorrectUnsat
scripts::Test::CorrectUnsat> :sat andSelf
Unsatisfiable
(Total Elapsed Time: 0.005s, using "Z3")
```
