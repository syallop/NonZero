# Data Structures with a type-indexed, non-zero size.

Abstracts structures which store strictly one or more elements
and whose size can be computed upon at compile-time.

Think Lists, which may never be empty and whose type tells us the number of elements contained.
E.G.

```haskell
    import Data.NonZero.Vector

    --threeElements :: Vector (Suc (Suc One)) Int
    threeElements :: Vector $(toNat 3) Int
    threeElements = 1 :| 2 :| Only 3
```

**Status:** *Personal, incomplete*

