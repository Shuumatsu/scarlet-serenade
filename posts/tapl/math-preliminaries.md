---
title: Types and Programming Languages | Mathematical Preliminaries
---

**Definition**: The **domain** of a relation $R$ on sets $S$ and $T$, written $dom(R)$, is the set of elements $s \in S$ such that $(s, t) \in R$ for some $t$

**Definition**: The **codomain or range** of $R$ , written $range(R)$ is the set of elements $t \in T$ such that $(s, t) \in R$ for some $s$.

**Definition**: A relation $R$ on sets $S$ and $T$ is called a **partial function** from $S$ to $T$ if, whenever $(s, t_1) \in R$ and $(s, t_2) \in R$, we have $t_1 = t_2$.
:indent[- in addition, if $dom(R) = S$ then $R$ is called a **total function**.]

**Definition**: A partial function $R$ from $S$ to $T$ is said to be **defined** on an argument $s \in S$ if $s \in dom(R)$ and undefined otherwise.
:indent[- we write $f(x) = \uparrow$ to mean $f$ is undefined on $x$]
:indent[- we write $f(x) = \downarrow $ to mean $f$ is defined on $x$]
We will also need to define functions that may fail on some inputs.  It is important to distinguish failure (which is a legitimate, observable result) from divergence. (怎么没解释什么是 divergence 啊...

**Definition**: Suppose $R$ is a binary relation on a set $S$ and $P$ is a predicate on $S$. We say that $P$ is **preserved** by $R$ if whenever we have $s R s'$ and $P(s)$ we also have $P(s')$

**Definition**: A binary relation $R$ on a set $S$ 
:indent[- **reflexive** if $R$ relates every element of $S$ to itself - that is, $s R s$]
:indent[- **transitive** $s R t$ and $t R u$ together implies $s R u$ ]
:indent[- **s  ymmetric** if $s R t$ implies $t R s$ ]
:indent[- **antisymmetric** $s R t$ and $t R s$ together implies $s = t$ ]

**Definition**: A reflexive, transitive, and symmetric relation on a set $S$ is called an **equivalence** on $S$.

**Definition**: A reflexive and transitive relation $R$ on a set $S$ is called a **preorder** on $S$.
:indent[- preorders are usually written using symbols like $\leq$. We write $s \lt t$ ($s$ is strictly less than $t$) to mean $s \leq t \wedge s \neq t$.]

**Definition**: A preorder (on a set $S$) that is also antisymmetric is called a **partial order** on $S$. 


**Definition**: A partial order $\leq$ is called a **total order** if it also has the property that, for each $s$ and $t$ in $S$, either $s \leq t$ or $t \leq s$.


**Definition**: Suppose R is a binary relation on a set $S$. 
:indent[- The **reflexive closure** of $R$ is the smallest reflexive relation $R'$ that contains R. ]
:indent[- The **transitive closure** of $R$ is the smallest transitive relation $R^+$ that contains R. ]
:indent[- The **reflexive and transitive closure** of $R$ is the smallest reflexive and transitive relation $R^*$ that contains R. ]

**Definition**: Suppose we have a set $S$ with a preorder $\leq$. We say that $\leq$ is **well founded** if it contains no infinite decreasing chains.
:indent[- For example, the usual order on the natural numbers, with $0 < 1 < 2 < 3 < \dots$ is well founded, but the same order on the integers, $\dots < -3 < -2 < -1 < 0 < 1 < 2 < 3 < \dots$ is not. (好像意思虽然序列的长度不是有界的，但是自然数下的这个关系，从任何元素开始，最小只能小到 $0$，而整数上的可以无限小下去？] 
