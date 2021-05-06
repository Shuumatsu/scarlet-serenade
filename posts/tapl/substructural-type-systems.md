---
title: Types and Programming Languages | Substructural Type Systems
---

While abstract types are a powerful means of controlling the structure of data, they are not sufficient to limit the **ordering and number of uses** of functions in an interface. Try as we might, there is no (static) way to prevent a file from being read after it has been closed.


**Substructural type systems** augment standard type abstraction mechanisms with the ability to control the **number and order of uses** of a data structure or operation.

three basic structural properties satisfied by our simply-typed lambda calculus
- **exchange**, indicates that the order in which we write down variables in the context is irrelevant
- **weakening**, indicates that adding extra, unneeded assumptions to the context, does not prevent a term from type checking. 
- **contraction**, states that if we can type check a term using two identical assumptions ($x_2: T_1$ and $x_3: T_1$) then we can check the same term using a single assumption
