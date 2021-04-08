---
title: Types and Programming Languages | Existential Types
---

Existential quantifiers offering an elegant foundation for data abstraction and information hiding.

- An element of $\{\exists X, T\}$ is a value of type $[X \mapsto S] T$ for some type $S$.
- An element of $\{\exists X, T\}$ is a pair, written $\{*S, t\}$ of type $S$ and a term t of type $[X \mapsto S]T$

An existentially typed value is introduced by pairing a type with a term, written $\{*S, t\}$. (Another common notation is $\operatorname{pack} X=S \operatorname{with} t.$) \
The type $S$ is often called the **hidden representation type**, or sometimes the **witness type** of the package.

A useful concrete intuition is to think of a value $\{*S, t\}$ of type $\{\exists X, T\}$ as a simple form of package or module with one (hidden) type component and one term component.

 For example, the package $\{* \mathrm{Nat},\{\mathrm{a}=5, \mathrm{f}=\lambda \mathrm{x}:$ Nat. $\operatorname{succ}(\mathrm{x})\}\}$