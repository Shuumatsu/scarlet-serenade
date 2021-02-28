---
title: Thinking with Types
--- 

## The Algebra Behind Types

###  Isomorphisms and Cardinalities

**Definition**: we can associate each type with its **cardinality** -- the number of inhabitants it has, ignoring bottoms.

```haskell
data Void 
-- Void has zero inhabitants, and so it is assigned cardinality 0.
-- |Void| = 0
data () = ()
-- |()| = 1
data Bool = False | True
-- |Bool| = 2
```

**Definition**: Two types that have the same cardinality will always be **isomorphic** to one another. An isomorphism between types `s` and `t` is defined as a pair of functions `to` and `from`:

```haskell
to :: s -> t
from :: t -> s
-- to . from = id
-- from . to = id
```

We sometimes write an isomorphism between types `s` and `t` as `s ~= t`.

If two types have the same cardinality, any one-to-one mapping between their elements is exactly these to and from functions.

In general, for any two types with cardinality n, there are n! unique isomorphisms between them. As far as the math goes, any of these is just as good as any other—and for most purposes, knowing that an isomorphism exists is enough.

An isomorphism between types `s` and `t` is a proof that for all intents and purposes, `s` and `t` are the same thing.


### Sum, Product and Exponential Types

In the language of cardinalities, **sum types** correspond to **addition**, **product types** correspond to **multiplication**

```haskell 
-- |Deal a b| = |a| + |b| + |Bool| = |a| + |b| + 2
data Deal a b
  = This a
  | That b
  | TheOther Bool

-- |(a, b)| = |a| × |b|
```

**function types** correspond to **exponentialization**. The type `a -> b` has cardinality `|b| ** |a|`. We can chose any value of b for every value
of a—resulting in the following equality.
```
|a -> b| = |b| * |b| * ... * |b| = |b| ** |a|
           --------|a| times----
```


### The Curry–Howard Isomorphism

| Algebra | Logic   | Types       |
|---------|---------|-------------|
| a + b   | a or b  | Either a b  |
| a * b   | a and b | (a, b)      |
| b ** a  | a => b  | a -> b      |
| a = b   | a <=> b | isomorphism |
| 0       | bottom  | Void        |
| 1       | top     | ()          |

The Curry–Howard isomorphism allows us to analyze mathematical theorems through the lens of functional programming.

To illustrate, consider the theorem `a ** 1 = a`. When viewed through Curry–Howard, it describes an isomorphism between `() -> a` and `a`. Said another way, this theorem shows that there is no distinction between having a value and having a (pure) program that computes that value.

### Canonical Representations

addition on the outside and multiplication on the inside
```
Either a (Either b (c, d)) => |a| + (|b| + |c| * |d|) -- canonical
(a, Either b c) => |a| * (|b| + |c|) -- not canonical
```

As an example, the canonical representation of `Maybe a` is `Either a ()`.

## Terms, Types and Kinds

### The Kind System

We can also refer to `TYPE`s, which is the kind of types that have inhabitants. Historically TYPE has been written as `*`, but this older notation is slated for deprecation in the latest versions of GHC.

#### Arrow Kinds

Higher-kinded types (HKTs) are those which have type variables.
Consider `Maybe`. Because it takes a single `TYPE` parameter, we say that `Maybe` has kind `TYPE -> TYPE` -- it takes a `TYPE` and gives you back one.

#### Constraint Kinds

For example, the type of `show` is `Show a => a -> String`. This `Show` thing exists as part of the type signature, its kind is `CONSTRAINT`.

### Data Kinds (by enabling the -XDataKinds extension

With `-XDataKinds` enabled, almost all² types automatically promote to kinds, including the built-in ones.

```haskell
Prelude> :kind 123
-- 123 :: GHC.Types.Nat
Prelude> :kind True
-- True :: Bool
```