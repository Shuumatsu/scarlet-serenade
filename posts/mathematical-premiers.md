---
title: Mathematical Premiers
---

## Relations

**Definition**: A relation $R$ on a set $P$ is called a partial order if it is (partial means for a pair of set elements in $P$, they could be incomparable.
- reflexive: $\forall x \in P, x \sqsubseteq x$
- antisymmetric: $\forall x, y \in P, x \sqsubseteq y \wedge y \sqsubseteq x \Rightarrow x = y$
- transitive: $\forall x, y, z \in P, x \sqsubseteq y \wedge y \sqsubseteq z \Rightarrow x \sqsubseteq z$


**Definition**: A binary relation $R$ on a set $P$ is called **an equivalence relation** iff
- reflexive: $\forall x \in P, x \sqsubseteq x$
- symmetric: $\forall x, y \in P$, $xRy \Rightarrow yRx$
- transitive: $\forall x, y, z \in P, x \sqsubseteq y \wedge y \sqsubseteq z \Rightarrow x \sqsubseteq z$

## Structural Induction

The template for inductive reasoning principle, for an inductively defined set A, is as follows.

For any property $P$, if
- base cases: for each axiom $\frac{}{a \in A}$, $P(a)$ holds.
- Inductive cases: For each inference rule $\frac{a_{1} \in A \quad \cdots \quad a_{n} \in A}{a \in A}$, if $P(a_1) \ldots P(a_n) \Rightarrow P(a)$. Then $\forall a \in A, P(a)$ holds