---
title: Introduction to the Theory of Computation | Context-Free Languages
---


The sequence of substitutions to obtain a string is called a **derivation**.

**Definition**: A **context-free grammar** is a 4-tuple $(V, \Sigma, R, S)$, where 
1. $V$ is a finite set called the **variables**
2. $\Sigma$ is a finite set, disjoint from $V$ , called the **terminals**
3. $R$ is a finite set of rules, with each rule being a variable and a string of variables and terminals
4. $S \in V$ is the start variable
The language of the grammar is $\left\{w \in \Sigma^{*} \mid S \stackrel{*}{\Rightarrow} w\right\}$

---

**Method**: Convert DFA into equivalent CFG.
1. make a variable $R_i$ for each state $q_i$ of the DFA
2. add the rule $R_{i} \rightarrow a R_{j}$ to the CFG if $\delta\left(q_{i}, a\right)=q_{j}$
3. add the rule $R_{i} \rightarrow \varepsilon$ to the CFG if $q_i$ is an acceptstate of the DFA.
4. make $R_0$ the start variable of the grammar, where $q_0$ is the start state of the machine.

---

**Definition**: A string $w$ is derived **ambiguously** in context-free grammar $G$ if it has two or more different leftmost derivations.  Grammar G is **ambiguous** if it generates some string ambiguously.

**Definition**: Languages that can be generated only by ambiguous grammars are called **inherently ambiguous**.

---

**Definition**: A  context-free grammar is in **Chomsky normal form** if every rule is of the form
$$
\begin{array}{l}
A \rightarrow B C \\
A \rightarrow a
\end{array}
$$
where $a$ is any terminal and $A$, $B$, and $C$ are any variables—except that $B$ and $C$ may not be the start variable. In addition, we permit the rule $S \rightarrow \varepsilon$, where $S$ is the start variable.

We can convert any grammar $G$ into Chomsky normal form.
1. 去除无用符号，去除空产生式，去除单位产生式
2. 把所有长度大于一的生成式全部改成变元，比如 $A \rightarrow abc$ 改成 $A \rightarrow XYZ$ 
3. 通过二分的方法把所有长度大于 2 的产生式改成 2 的，比如 $A \rightarrow WXYZ$ 改成 $A \rightarrow MN$ 其中 $M \rightarrow WX$ 

乔姆斯基的一大作用是将分析树变成二叉树: 如果分析树的高度为 $n$ 那么产出的最大也就是前 $n - 1$ 层是满二叉树的情况，产出长度为 $2^{n-1}$

---

**Definition**: A pushdown automaton is a 6-tuple $\left(Q, \Sigma, \Gamma, \delta, q_{0}, F\right)$, where $Q$, $\Sigma$, $\Gamma$, and $F$ are all finite sets, and 
1. $Q$ is the set of states
2. $\Sigma$ is the input alphabet
3. $\Gamma$ is the stack alphabet
4. $\delta: Q \times \Sigma_{\varepsilon} \times \Gamma_{\varepsilon} \longrightarrow \mathcal{P}\left(Q \times \Gamma_{\varepsilon}\right)$ is the transition function 
5. $q_{0} \in Q$ is the start state 
6. $F \subseteq Q$ is the set of accept states

---

**Theorem**: A language is context free if and only if some pushdown automaton recognizes it.

---

**Pumping lemma for context-free languages** states that, if $A$ is a context-free language, then there is a number $p$ (the pumping length) where, if $s$ is any string in $A$ of length at least $p$, then $s$ may be divided into five pieces $s = uvxyz$ satisfying the conditions
1. $\forall i \geq 0, u v^{i} x y^{i} z \in A$
2. $|v y|>0$
3. $|v x y| \leq p$

---

Nondeterministic pushdown automata are more powerful than their deterministic counterparts. The languages that are recognizable by deterministic pushdown automata (DPDAs) are called **deterministic context-free languages (DCFLs)**.


**Lemma**: Every DPDA has an equivalent DPDA that always reads the entire input string.


For simplicity, we'll assume henceforth that DPDAs read their input to the end.

---

The class of DCFLs is closed under complementation.
