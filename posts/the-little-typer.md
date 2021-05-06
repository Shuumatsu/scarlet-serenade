---
title: The Little Typer
---

Atoms are built from a tick mark directly followed by one or more letters and hyphens.\
e.g. 'this-is-an-atom

The Four Forms of Judgment
1. `___` is a `___`
2. `___` is the same `___` as `___`
3. `___` is a type
4. `___` and `___` are the same type`

Some forms of judgment only make sense after an earlier judgment(pre-supposition).
e.g, To ask whether two expressions are the same type, one must first judge that each expression is, in fact, a type.

A pair abegins with `cons` and ends with two more parts called is `car` and its `cdr`
In lisp, `cons` is used to make lists longer. In Pie, `cons` only constructs pairs.

Expressions that describe other expressions, such as `Atom`, are called **types**

Given a type, every expression described by that type has a **normal form**, which is the most direct way of writing it. If two expressions are the same iff they have identical normal forms. 
`'olive-oil` is the normal form of the Atom `(cdr (cdr (cons 'a (cons 'b 'olive-oil))))`
Types them selves also have normal forms.

Two cons-expressions are the same `(Pair A D)` if their cars are the same `A` and their `cdr` are the same `D`. Here `A` and `D` stand for any type.


Using `define` to associate a name with an expression requires that the expression's type has previously been associated with the name using `claim`
```scheme
(claim one Nat)
(define one (add1 zero))
```

An expression with a constructor at the top is called a **value**

Part of explaining a new type is to say what its constructors are. The constructor expressions are the direct ways of building expressions with the new type.

The constructors of `Nat` are zero and `add1`, while the constructor of `Pair` is cons.
All atoms are constructors. Each atom constructs itself.


Not every value is in normal form. This is because the arguments to a constructor need not be normal.


Finding a value that is the same as some starting expressions is called **evaluation**

Everything is an Expression.


Constructors build values, and eliminator take apart values built by constructors.
Applying a function to argument is the eliminator for functions

two lambda-expressions that expect the same number of arguments are the same if their bodies are the same after consistently renaming heir variables


Expressions that are not values and cannot **yet** be evaluated due to a variable are called **neutral**
`(cons y 'r)` is value
if `x` is a `(Pair Nat Atom)`, `(cdr x)` is a neutral
because `cdr` is an eliminator and eliminator takes apart values. without knowing the value of `x` there is no way to find the value of `(cdr x)`, 

Neutral expressions that are written identically are the same no matter their type.


P43

Th eliminator for `Nat` is `which-Nat`
A `which-Nat` expression has 3 arguments: target, base and step
which-Nat checks whether target is zero, if so, the value of which-Nat expressions is the value of base 
otherwise, if target is `(add1 n)`, the value of which-Nat is the value of `(step n)`


An expression that is described by a type is a value when it has a constructor at its top.
Similarly, an expression that is a type is a value when it has a type constructor at its top.

Type constructors construct types and data constructors construct values that are described by those types.



every expression described by `u` is a type, but not every type is described by `u`
