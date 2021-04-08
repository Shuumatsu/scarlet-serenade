---
title: Introduction to the Theory of Computation | Regular Languages
---

**definition**: If $A$ is the set of all strings that machine $M$ accepts, we say that **$A$ is the language of machine $M$**. We say that $M$ **recognizes/accept** $A$.

**Definition**: Let $A$ and $B$ be languages. \
\- we define **union** as $A \cup B=\{x \mid x \in A$ or $x \in B\}$\
\- we define **concatenation** as $A \circ B=\{x y \mid x \in A$ and $y \in B\}$\
\- we define **star** as $A^{*}=\left\{x_{1} x_{2} \ldots x_{k} \mid k \geq 0\right.$ and each $\left.x_{i} \in A\right\}$\

## Deterministic finite automaton

**Definite**: A **deterministic finite automaton** is a 5-tuple $\left(Q, \Sigma, \delta, q_{0}, F\right)$, where
1. $Q$ is a finite set called the **states**.
2. $\Sigma$ is a finite set called the **alphabet**.
3. $\delta: Q \times \Sigma \longrightarrow Q$ is the **transition function**.
4. $q_{0} \in Q$ is the **start state**.
5. $F \subseteq Q$ is the **set of accept/final states**.


**Definition**: Let $M = \left(Q, \Sigma, \delta, q_{0}, F\right)$ be a deterministic  finite automaton and let $w=w_{1} w_{2} \cdots w_{n}$ be a string were each $w_i$ is a member of the alphaet $\Sigma$. Then $M$ **aceepts** $w$ if a sequence of states $r_{0}, r_{1}, \ldots, r_{n}$ in $Q$ exists with three conditions
- $r_{0} = q_{0}$
- $\delta\left(r_{i}, w_{i+1}\right) = r_{i+1}$
- $r_{n} \in F$

## Nondeterministic Finite Automaton

**Definite**: A **nondeterministic finite automaton** is a 5-tuple $\left(Q, \Sigma, \delta, q_{0}, F\right)$, where
1. $Q$ is a finite set called the **states**.
2. $\Sigma$ is a finite set called the **alphabet**.
3. $\delta: Q \times \Sigma_{\varepsilon} \longrightarrow \mathcal{P}(Q)$ is the **transition function**. (Here $P(Q)$ is called the **power set** of $Q$.
4. $q_{0} \in Q$ is the **start state**.
5. $F \subseteq Q$ is the **set of accept/final states**.

**Definition**: Let $M = \left(Q, \Sigma, \delta, q_{0}, F\right)$ be a nondeterministic finite automaton and let $w=w_{1} w_{2} \cdots w_{n}$ be a string were each $w_i$ is a member of the alphaet $\Sigma$. Then $M$ **aceepts** $w$ if a sequence of states $r_{0}, r_{1}, \ldots, r_{n}$ in $Q$ exists with three conditions
- $r_{0} = q_{0}$
- $r_{i+1} \in \delta\left(r_{i}, w_{i+1}\right)$
- $r_{n} \in F$

## Equivalence of Nondeterministic Finite Automaton and Deterministic finite automaton



## Regular Languages

**Definition**: $A$ language is called a **regular language** if some finite automaton recognizes it.

**Theorem**: THe class of regular language is **closed under the union operation**

**Theorem**: THe class of regular language is **closed under the concatenation operation**

**Theorem**: THe class of regular language is **closed under the star operation**

## Regular Expressions


