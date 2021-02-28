---
title: Types and Programming Languages | Definition Styles
---

We will use the untyped arithmetic expression as example.

## BNF Definition

One way to describe our grammar is using **BNF**. Parentheses are not mentioned in the grammar of terms, which defines only their abstract syntax.

```
t ::= true
      false
      if t then t else t
      0
      succ t
      pred t
      iszero t
```

The symbol `t` in the right-hand sides of the rules of this grammar is called a **metavariable**. It is a place-holder for some particular term.
:indent[- "meta" means that it is not a variable of the object language - the language whose syntax we are currently describing but rather of the metalanguage - the notation in which the description is given.]

**Definition**: The term **metatheory** means the collection of true statements that we can make about some particular logical system (or programming language) and, by extension, the study of such statements.

## Inductive Definition

The set of terms is the smallest set $T$ such that
:indent[- $\{$ true, false, 0$\} \subseteq T$]
:indent[- if $t_1 \in T$ then $\operatorname{succ } t_1, \operatorname{pred } t_1, \operatorname{iszero } t_1 \subseteq T$]
:indent[- if $t_1 \in T, t_2 \in T$ and $t_3 \in T$ then $\operatorname{if } t_1 \operatorname{ then } t_2 \operatorname{ else } t_3 \in T$]

This can also be written as the **natural deduction style**:

$$
\begin{gathered}
true \in T \qquad
false \in T \qquad
0 \in T \\

\frac{t_1 \in T}{\text{succ } t_1 \in T} \qquad
\frac{t_1 \in T}{\text{pred } t_1 \in T} \qquad
\frac{t_1 \in T}{\text{iszero } t_1 \in T} \\

\frac{t_1 \in T \quad t_2 \in T \quad t_3 \in T}{\text{if } t_1 \text{ then } t_2 \text{ else } t_3 \in T}
\end{gathered}
$$

Each rule is read "If we have established the statements in the premise(s) listed above the line, then we may derive the conclusion below the line."\
Rules with no premises (like the first three above) are often called axioms. 

To be completely pedantic, what we are calling **inference rules** are actually rule schemas, since their premises and conclusions may include metavariables.
:indent[- for example, you can replace the metavariable $t_1$ by all phrases from the appropriate syntactic category]

Like the BNF grammar, this definition says nothing about the use of parentheses to mark compound subterms. Formally, what's really going on is that we are defining $T$ as a set of trees.


## Concrete Definition

For each natural number $i$, define a set $S_i$ as follows:

$$
\begin{aligned}
S_0 = \;       & \phi \\
S_{i + 1} = \; & \{true, false, 0\} \; \cup \\
               & \{\operatorname{succ} t_1, \operatorname{pred} t_1, \operatorname{iszero } t_1 | t_1 \in S_i \} \; \cup \\
               & \{\operatorname{if } t_1 \operatorname{ then } t_2 \operatorname{ else } t_3 | t_1, t_2, t_3 \in S_i \}
\end{aligned}
$$


Finally, let $S=\bigcup_{i} S_{i}$


### Equivalence between Inductive Definition and Concrete Definition

Use $T$ to represent the one defined by **inductive definition** and $S$ to represent the one defined by **concrete definition**.

$T$ was defined as the smallest set satisfying certain conditions. so our proof consists of two pars:

- $S$ satisfies these conditions

:::indent

1. the constants are trivial

2. if $t \in S$ then $\exists i, s.t.\ t \in S_i$, then by definition of $S_{i+1}$, we have $\text{succ } t \in S_{i + 1} \subseteq S$, similarly, we see that $\text{pred } t \in S_{i + 1} \subseteq S$ and $\text{iszero } t \in S_{i + 1} \subseteq S$

3. if $t_1 \in T, t_2 \in T$ and $t_3 \in T$, similarly $\text{if } t_1 \text{ then } t_2 \text{ else } t_3 \in S{i+1} \subseteq S$
:::

- $S$ is the smallest set, in other words, any set satisfying the conditions has $S$ as a subset

:::indent
suppose that some set $S'$ satisfies the conditions, we will argue by complete induction on $i$, that every $S_i \subseteq S'$, from which it clearly follows $S \subseteq S'$

Suppose that $S_{j} \subseteq S'$ for all $j \lt i$, we are to show that $S_i \subseteq S'$. By the definition of $S_i$, there are two cases to consider

1. $i = 0$: it's trivial
2. $i > 0$: $\exists j, s.t.\ i = j + 1$. Since $S_{j + 1}$ is defined as the union of three smaller sets, an element $t \in S_{j + 1}$ must come from one of these sets:
    - if $t$ is constant, then it's trivial $t \in S'$
    - if $t$ has the form $\text{succ } t_1$ for some $t_1 \ S_j$, then by the induction hypothesis, $t_1 \in S'$ and by condition (2) $t \in S'$
    - if $t$ has the form $\text{if } t_1 \text{ then } t_2 \text{ else } t_3$, for the same reason in (ii), $t \in S'$

Thus, we have shown that each $S_i \subseteq S'$. By the definition of S as the union of all the $S_i$, this gives $S \subseteq S'$, completing the argument.

:::
