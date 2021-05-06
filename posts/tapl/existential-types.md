---
title: Types and Programming Languages | Existential Types
---

Existential quantifiers offering an elegant foundation for data abstraction and information hiding.

- An element of $\{\exists X, T\}$ is a value of type $[X \mapsto S] T$ for some type $S$.
- An element of $\{\exists X, T\}$ is a pair, written $\{*S, t\}$ of type $S$ and a term t of type $[X \mapsto S]T$

An existentially typed value is introduced by pairing a type with a term, written $\{*S, t\}$. (Another common notation is $\operatorname{pack} X=S \operatorname{with} t.$) \
The type $S$ is often called the **hidden representation type**, or sometimes the **witness type** of the package.

A useful concrete intuition is to think of a value $\{*S, t\}$ of type $\{\exists X, T\}$ as a simple form of package or module with one (hidden) type component and one term component.

For example, the package $\{* Nat,\{a=5, f=\lambda x:$ Nat. $\operatorname{succ}(x)\}\}$ has the existential type $\{\exists X, \{a: X, f: X \rightarrow X\}\}$ (can also has type $\{\exists X, \{a: X, f: X \rightarrow Nat\}\}$

The ambiguity shows that , the typechecker cannot make an automatic decision about which existential type a given package belongs to: the programmer must specify which one is intended.
- $p = \{* Nat,\{a=5, f=\lambda x: Nat. \operatorname{succ}(x)\}\} \; \operatorname{as} \; \{\exists X, \{a: X, f: X \rightarrow X\}\} \Rightarrow p: \{\exists X, \{a: X, f: X \rightarrow X\}\}$
- $p = \{* Nat,\{a=5, f=\lambda x: Nat. \operatorname{succ}(x)\}\} \; \operatorname{as} \; \{\exists X, \{a: X, f: X \rightarrow Nat\}\} \Rightarrow p: \{\exists X, \{a: X, f: X \rightarrow Nat\}\}$