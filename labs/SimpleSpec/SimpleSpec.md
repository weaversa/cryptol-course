
```
module labs::SimpleSpec::SimpleSpec where
```

```
import labs::CRC::CRCAnswers
import labs::KeyWrapping::NISTSean
```

```
tag : {a} (fin a) => [a] -> [128+a]
tag x = join "LoremIpsumKeyTag" # x
```

```
LorumIpsumKey : [128] -> [128] -> [384]
LorumIpsumKey KEK k = CRCTK
  where
    WrappedKey = KWAE128 KEK k
    CRCWK      = WrappedKey # (CRC32 (split WrappedKey))
    TaggedKey  = tag CRCWK
    CRCTK      = TaggedKey # (CRC32 (split TaggedKey))
```