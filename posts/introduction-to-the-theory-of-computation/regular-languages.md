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
4. $q_{0} \in Q$ is the **starting state**.
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
4. $q_{0} \in Q$ is the **starting state**.
5. $F \subseteq Q$ is the **set of accept/final states**.

**Definition**: Let $M = \left(Q, \Sigma, \delta, q_{0}, F\right)$ be a nondeterministic finite automaton and let $w=w_{1} w_{2} \cdots w_{n}$ be a string were each $w_i$ is a member of the alphaet $\Sigma$. Then $M$ **aceepts** $w$ if a sequence of states $r_{0}, r_{1}, \ldots, r_{n}$ in $Q$ exists with three conditions
- $r_{0} = q_{0}$
- $r_{i+1} \in \delta\left(r_{i}, w_{i+1}\right)$
- $r_{n} \in F$

### Equivalence of Nondeterministic Finite Automaton and Deterministic Finite Automaton

**Definition**: The **subset construction** starts from an NFA $N = (Q_N, \Sigma, \delta_N, q_{0}, F_n)$. Its goal is the DFA $D=\left(Q_{D}, \Sigma, \delta_{D},\left\{q_{0}\right\}, F_{D}\right)$ such that $L(D) = L(N)$
- $Q_D$ is the set of subsets of $Q_N$
- $F_D$ is the set of  states that include at lease on accepting state of $N$.
- For each set $S \subseteq Q_{D}$ and for each input symbol $a$ in $\Sigma$, $$\delta_{D}(S, a)=\bigcup_{p \text { in } S} \delta_{N}(p, a)$$

**Proof**: If is the DFA $D=\left(Q_{D}, \Sigma, \delta_{D},\left\{q_{0}\right\}, F_{D}\right)$ constructed from NFA $N = (Q_N, \Sigma, \delta_N, q_{0}, F_n)$, by the subset construction, then $L(D) = L(N)$

// TODO

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

Say that we have a regular expression $R$ describing some language $A$. We show how to convert $R$ into an NFA recognizing $A$. 

We consider the six cases in the formal definition of regular expressions.

The base cases are:

1. $a$: one starting state and one accepting state, with one transition from thpe start to the accepting state corresponding to $a$
2. $\varepsilon$: one starting state that's also accepting
3. $\phi$: no accepting state

The inductive cases are: (We assume $M_1$ and $M_2$ respectively recognizes $L(R_1)$ and $L(R_2)$

4. $\left(R_{1} \cup R_{2}\right)$: a new 'super starting state' with $\epsilon$ transitions to both starting states of $M_1$ and $M_2$
5. $\left(R_{1} \circ R_{2}\right)$: add $\epsilon$ transitions from every accepting states of $M_1$ to the starting state of $M_2$
6. $\left(R_{1}^{*}\right)$: add $\epsilon$ transitions from every accepting states to the starting state and from the starting state to accepting states

#### Finite Automaton => Regular Expressions
 
**Definition**: A **generalized nondeterministic finite automaton** is a 5-tuple, $\left(Q, \Sigma, \delta, q_{\text {start }}, q_{\text {accept }}\right)$, where \
:indent[(nfa wherein the transition arrows may have any regular expressions as labels, instead of only members of the alphabet or $\epsilon$.]

1. $Q$ is the finite set of states
2. $\Sigma$ is the input alphabet
3. $\delta:\left(Q-\left\{q_{\text {accept }}\right\}\right) \times\left(Q-\left\{q_{\text {start }}\right\}\right) \longrightarrow \mathcal{R}$ is the transition function
4. $q_{\text{start}}$is the starting state
5. $q_{\text{accept}}$ is the accepting state

**从 DFA 到 Reg**：给每个状态编号，$R_{ijk}$ 表示中间状态最高到 $k$ 的路径的正则表示。得到转移方程 $R_{ijk} = R_{ij(k-1)} + R_{ik(k-1)}(R_{kk(k-1)})*R_{kj(k-1)}$


    
**从 DFA 到 Reg**：通过消除每条初始状态到某个接受状态路径上的状态来构造正则表达式（有多个接受状态的话就有多条路径，最后将每条路径对应的正则表达式合起来
每消除一个状态 $p_s$ 就为其每个前驱 $q_i$ 及每个后继 $q_j$ 添加一条 $r_{i \rightarrow s} r_{s \rightarrow s}^* r_{s \rightarrow j}$
- 消到最后剩一个开始状态和接受状态，看67页

## The Pumping Lemma For Regular Languages

**Pumping Lemma** states that If $A$ is a regular language, then there is a number $p$ (called the **pumping length**) where if $s$ is any string in $A$ of length at least $p$, then $s$ may be divided into three pieces, $s = xyz$, satisfying the following conditions:
1. $|y|>0$
2. $|x y| \leq p$
3. $\forall i \geq 0, xy^iz \in A$

Proof:  Let $M = \left(Q, \Sigma, \delta, q_{0}, F\right)$ be a DFA recognizing $A$ and $p$ be the number of states of $M$. Let $s=s_{1} s_{2} \cdots s_{n}$ be a string in $A$ of length $n$, where $n \geq p$. 

$M$ accepts $s$, then there is a sequence of states $r_{0}, r_{1}, \ldots, r_{n}$ in $Q$. This sequence has length $n + 1$, which is at least $p + 1$.\
Among the first $p + 1$ elements in the sequence, two must be the same state, by the pigeonhole principle. We call the first of these positioned at $i$ and the second $j$.

Then we can spit $s$ into $s = xyz$ where, 

- $x$ takes $M$ from $r_0$ to $r_i$
- $y$ takes $M$ from $r_i$ to $r_j$ ($r_i$ and $r_j$ are the same state
- $z$ takes $M$ from $r_j$ to $r_n$ which is an accepting state.

which satisfies the 3 conditions: 

1. $|y| > 0$ is by definition
2. $j$ is among the first $p + 1$, then $|x y| \leq p$.
3. $y$ will not change the state as a result, then M must accept $xy^iz$ for all $i \geq 0$



## Regular Languages

**Definition**: $A$ language is called a **regular language** if some finite automaton recognizes it.

**Theorem**: THe class of regular language is **closed under the union operation**

**Theorem**: THe class of regular language is **closed under the concatenation operation**

**Theorem**: THe class of regular language is **closed under the star operation**   