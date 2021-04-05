---
title: Haskell | Polymorphism
---

A polymorphic function is a function that works for many different types. For instance,

```haskell
length :: [a] -> Int
-- here is a more explicit way to indicate that a can be any type
length :: forall a. [a] -> Int
```

You should think of the former signature as an abbreviation for the later one with the `forall` keyword.

This notion, that something is applicable to every type or holds for everything, is called universal quantification. 

## Higher rank types

With explicit `forall`, it now becomes possible to write functions that expect polymorphic arguments, like for instance

```haskell
foo :: (forall a. a -> a) -> (Char,Bool)
foo f = (f 'c', f True)
```