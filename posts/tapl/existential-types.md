---
title: Types and Programming Languages | Existential Types
---

Existential quantifiers offering an elegant foundation for data abstraction and information hiding.

- An element of $\{\exists X, T\}$ is a value of type $[X \mapsto S] T$ for some type $S$.
- An element of $\{\exists X, T\}$ is a pair, written $\{*S, t\}$ of type $S$ and a term t of type $[X \mapsto S]T$

An existentially typed value is introduced by pairing a type with a term, written $\{*S, t\}$.
