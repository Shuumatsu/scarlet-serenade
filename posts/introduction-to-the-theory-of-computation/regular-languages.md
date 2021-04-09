---
title: Introduction to the Theory of Computation | Regular Languages
---

**Definition**: If $A$ is the set of all strings that machine $M$ accepts, we say that **$A$ is the language of machine $M$**. We say that $M$ **recognizes/accept** $A$.

**Definition**: Let $A$ and $B$ be languages. \
\- we define **union** as $A \cup B=\{x \mid x \in A$ or $x \in B\}$\
\- we define **concatenation** as $A \circ B=\{x y \mid x \in A$ and $y \in B\}$\
\- we define **star** as $A^{*}=\left\{x_{1} x_{2} \ldots x_{k} \mid k \geq 0\right.$ and each $\left.x_{i} \in A\right\}$\

## Deterministic Finite Automaton

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

## Equivalence of Nondeterministic Finite Automaton and Deterministic Finite Automaton

**从 NFA 到 DFA**：子集构造，证明采用数学归纳法

**从 DFA 到 Reg**：给每个状态编号，$R_{ijk}$ 表示中间状态最高到 $k$ 的路径的正则表示。得到转移方程 $R_{ijk} = R_{ij(k-1)} + R_{ik(k-1)}(R_{kk(k-1)})*R_{kj(k-1)}$

## Regular Languages

**Definition**: $A$ language is called a **regular language** if some finite automaton recognizes it.

**Theorem**: THe class of regular language is **closed under the union operation**

**Theorem**: THe class of regular language is **closed under the concatenation operation**

**Theorem**: THe class of regular language is **closed under the star operation**

## Regular Expressions

**Definition**: Say that $R$ is a **regular expression** if $R$ is
1. $a$ for some $a$ in the alphabet $\Sigma$
2. $\varepsilon$
3. $\phi$
4. $\left(R_{1} \cup R_{2}\right)$, where $R_1$ and $R_2$ are regular expressions
5. $\left(R_{1} \circ R_{2}\right)$, where $R_1$ and $R_2$ are regular expressions
6. $\left(R_{1}^{*}\right)$, where $R_1$ 1 is a regular expression

A definition of this type is called an **inductive definition**.

### Equivalence with Finite Automaton

#### Regular Expressions => Finite Automaton

We consider the six cases in the formal definition of regular expressions.
- the base cases (1), (2), (3) are trivial.
- $R = \left(R_{1} \cup R_{2}\right)$ 用一个 $\epsilon$ 把 DFA(R) 的终态和 DFA(S) 的初态连起来
- $R = \left(R_{1} \circ R_{2}\right)$ 两个 $\epsilon$ 分别转移到R、S的初态，再两个分别从 R、S 终态转移出来
- $R = \left(R_{1}^{*}\right)$ 一个 $\epsilon$ 从 R 的终转移到其始，一个 $\epsilon$ 跳过R

#### Finite Automaton => Regular Expressions

**Definition**: A **generalized nondeterministic finite automaton** is a 5-tuple, $\left(Q, \Sigma, \delta, q_{\text {start }}, q_{\text {accept }}\right)$, where \
:indent[(nfa wherein the transition arrows may have any regular expressions as labels, instead of only members of the alphabet or $\epsilon$.]

1. $Q$ is the finite set of states
2. $\Sigma$ is the input alphabet
3. $\delta:\left(Q-\left\{q_{\text {accept }}\right\}\right) \times\left(Q-\left\{q_{\text {start }}\right\}\right) \longrightarrow \mathcal{R}$ is the transition function
4. $q_{\text{start}}$is the start state
5. $q_{\text{accept}}$ is the accept state


**从 DFA 到 Reg**：通过消除每条初始状态到某个接受状态路径上的状态来构造正则表达式（有多个接受状态的话就有多条路径，最后将每条路径对应的正则表达式合起来
每消除一个状态 $p_s$ 就为其每个前驱 $q_i$ 及每个后继 $q_j$ 添加一条 $r_{i \rightarrow s} r_{s \rightarrow s}^* r_{s \rightarrow j}$
- 消到最后剩一个开始状态和接受状态，看67页


**Theorem**:  A language is regular if and only if some regular expression describes it.