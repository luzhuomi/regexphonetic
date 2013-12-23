regexphonetic
=============


The Scala Implementation of the regular Expression based phonetic matching algorithm using Partial derivatives.


The Haskell version can be found here: https://code.google.com/p/xhaskell-phonetic



example
==========

```
$ cd example
$ sbt console



scala> import Test._
import Test._

scala> phonetic_distance(singlish_consonant_table)(singlish_vowel_table)("goverment")("gahmen")
res0: Float = 0.6
```

