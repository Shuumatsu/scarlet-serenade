---
title: Programming Language Foundations
---

## Program Equivalence

Two expressions are **behaviorally equivalent** if they evaluate to the same result in every state. 对指令而言，若两个指令在任何给定的初始状态下，要么发散， 要么在相同的状态下停机，则二者行为等价。” 简单来说，就是： “若其中一个指令在某状态下停机，那么另一个也在该状态下停机，反之亦然。

```coq
Definition aequiv (a1 a2 : aexp) : Prop := 
    forall (st : state), aeval st a1 = aeval st a2.
Definition bequiv (b1 b2 : bexp) : Prop := 
    forall (st : state), beval st b1 = beval st b2.
Definition cequiv (c1 c2 : com) : Prop := 
    forall (st st' : state), (st =[ c1 ]=> st') <-> (st =[ c2 ]=> st').
```

行为等价满足**一致性（congruence）**。That is, the equivalence of two subprograms implies the equivalence of the larger programs in which they are embedded.
:::indent
它最主要意义在于，allow us to replace a small part of a large program with an equivalent small part and know that the whole large programs are equivalent without doing an explicit proof about the non-varying parts
:::

### Program Transformations

**程序变换（program transformation）** 是一种以某个程序作为输入， 产生该程序的某种变体作为输出的函数。如果一个程序变换保留了其原始行为，那么它就是' 可靠（sound）' 的。

```coq
Definition atrans_sound (atrans : aexp -> aexp) : Prop :=
    forall (a : aexp), aequiv a (atrans a).
Definition btrans_sound (btrans : bexp -> bexp) : Prop :=
    forall (b : bexp), bequiv b (btrans b).
Definition ctrans_sound (ctrans : com -> com) : Prop :=
    forall (c : com), cequiv c (ctrans c).
```

## Hoare Logic

要讨论程序的规范，我们需要的首先是一种在程序执行过程中某个时刻， 关于程序性质做出 **断言 (assertions)** 的方法。也就是说，我们要讨论执 行到某处时，当时的内存状态。形式化地说，一项断言就是一系列关于 state 的命题。
```coq
Definition Assertion := state -> Prop.
```

给出两断言 `P` 与 `Q`，我们说 `P` '蕴含' `Q`， 写作 `P ->> Q`，如果当 `P` 在 `st` 下成立，`Q` 也成立。 
```coq
Definition assert_implies (P Q : Assertion) : Prop :=
    forall st, P st -> Q st.

Notation "P ->> Q" := (assert_implies P Q) (at level 80) : hoare_spec_scope.
Notation "P <<->> Q" := (assert_implies P Q /\ assert_implies Q P) (at level 80) : hoare_spec_scope.
```

---

广泛而言，一个命令的行为就是把一个状态转变成另一个状态，所以 我们可以自然地通过命令运行前后的断言来描述一个命令。A **Hoare triple** is a claim about the state before and after executing a command. A standard notation is ${P} \; c \; {Q}$ \
meaning: 
- If command `c` begins execution in a state satisfying assertion `P`,
- and if c eventually terminates in some final state,
- then that final state will satisfy the assertion `Q`.
Assertion P is called the **precondition** of the triple, and Q is the **postcondition**

```coq
Definition hoare_triple (P : Assertion) (c : com) (Q : Assertion) : Prop :=
  forall st st', st =[ c ]=> st' -> P st -> Q st'.
Notation "{{ P }} c {{ Q }}" := (hoare_triple P c Q) (at level 90, c at next level) : hoare_spec_scope.
```

For example, $\{X = 0\} \; X := X+1 \; \{X = 1\}$ is a valid Hoare triple, stating that command X := X + 1 would transform a state in which X = 0 to a state in which X = 1.

---

**Definition**: $P$ is a **weakest precondition** of command $c$ for postcondition $Q$ if:
- $P$ is a precondition, that is, $\{P\} \; c \; \{Q\}$
- $P$ is at least as weak as all other preconditions, that is, if $\{P'\} \; c \; \{Q\}$ then $P' \rightarrow P$.

### Hoare logic Proof Rules

The goal of Hoare logic is to provide a **compositional** method for proving the validity of specific Hoare triples.

#### Assignment

If you want statement $X := a$ to terminate in a state that satisfies assertion $Q$, then it suffices to start in a state that also satisfies $Q$, except where $a$ is substituted for every occurrence of $X$.

:::indent
introduce a notation for this idea of replacing occurrences: Define $Q [X \mapsto a]$ to mean "$Q$ where $a$ is substituted in place of $X$". (which we refer to as assertion substitution
```
Definition assn_sub X a (P:Assertion) : Assertion := 
    fun (st : state) => P (X !-> aeval st a ; st).
Notation "P [ X |-> a ]" := (assn_sub X a P) (at level 10, X at next level).
```

That yields the Hoare logic rule for assignment:
$$ \frac{}{ \{Q [X \mapsto a]\} \quad X ::= a \quad \{Q\} }$$

We can prove formally that this rule is indeed valid.
```
Theorem hoare_asgn : forall Q X a,
{{ Q [X |-> a] }} (CAss X a) {{ Q }}.
Proof.
    unfold hoare_triple. unfold assn_sub.
    intros Q X a st st' HE HQ. inversion HE. subst.
    assumption.
Qed.
```
:::

#### Consequence

有的时候我们从其它证明规则中得到的前置条件和后置条件可能并不是我们想使用的那个情形：
- 它们可能在逻辑上符合需要，但是有着不同的形式而无法和期望的情形匹配；
- 或者我们所得到的这个三元组的前条件 太弱，抑或是后条件太强。

例如，$\{(X = 3) [X \mapsto 3] \} \; X := 3 \; \{X = 3\}$ 可以直接由赋值规则所得，而 $\{ True \} \; X := 3 \; \{X = 3\}$ 却不行。这个三元组是成立的，不过它并不是 hoare_asgn 的实例，因 为 $True$ and $(X = 3) [X \mapsto 3]$ 在语法上并不是相同的断言。

我们可以看到对一个成立的三元组 **加强前置条件** 或者 **减弱后置条件** 总是能得到一个成立的三元组。这种想法可以用两条 '缩放规则（Rules of Consequence）' 来描述：

::::div{style="display: flex; justify-content: space-around"}

:::div
hoare_consequence_pre_equiv
$$
\frac{
    \begin{gathered}
    \{ P' \} \quad c \quad \{ Q \} \\
    P \rightarrow P'
    \end{gathered}
}{
    \{ P \} \quad c \quad \{ Q \}
}
$$
```
Theorem hoare_consequence_pre : forall (P P' Q : Assertion) c,
    {{ P' }} c {{ Q }} ->
    P ->> P' ->
    {{ P }} c {{ Q }}.
Proof.
    intros P P' Q c Hhoare Himp.
    intros st st' Hc HP. apply (Hhoare st st').
    assumption. apply Himp. assumption. 
Qed.
```
:::

:::div
hoare_consequence_post
$$
\frac{
    \begin{gathered}
    \{ P \} \quad c \quad \{ Q' \} \\
    Q' \rightarrow Q
    \end{gathered}
}{
    \{ P \} \quad c \quad \{ Q \}
}
$$
```
Theorem hoare_consequence_post : forall (P Q Q' : Assertion) c,
    {{ P }} c {{ Q' }} ->
    Q' ->> Q ->
    {{ P }} c {{ Q }}.
Proof.
    intros P Q Q' c Hhoare Himp.
    intros st st' Hc HP.
    apply Himp.
    apply (Hhoare st st').
    assumption. assumption. 
Qed.
```
:::
::::


为了证明中的方便，我们有一个组合起来的缩放规则，可以让 我们同时改变前置条件和后置条件。
```
Theorem hoare_consequence : forall (P P' Q Q' : Assertion) c,
    {{ P' }} c {{ Q' }} ->
    P ->> P' ->
    Q' ->> Q ->
    {{ P }} c {{ Q }}.
Proof.
    intros P P' Q Q' c Hht HPP' HQ'Q.
    apply hoare_consequence_pre with (P' := P').
    apply hoare_consequence_post with (Q' := Q').
    assumption. assumption. assumption. 
Qed.
```


#### Skip

因为 SKIP 并不改变当前状态，它会保持 P：

$$ 
\frac{}{ \{P\} \quad skip \quad \{P\} }
$$

```
Theorem hoare_skip : forall P,
    {{ P }} CSkip {{ P }}.
Proof.
    intros P st st' H HP. inversion H. subst.
    assumption. 
Qed.
```

#### Sequencing
如果命令 c1 将一个 P 成立的状态转变为 Q 成立的状态，而如果 c2 将 Q 成立的状态转变为 R 成立的， 那么先执行 c1 然后执行 c2 将会把一个 P 成立的状态转变 为一个 R 成立的状态：

$$
\frac{
    \begin{gathered}
    \{ P \} \quad c_1 \quad \{ Q \} \\
    \{ Q \} \quad c_2 \quad \{ R \} 
    \end{gathered}
}{
    \{ P \} \quad c_1; c_2 \quad \{ R \}
}
$$

```
Theorem hoare_seq : forall P Q R c1 c2,
    {{ Q }} c2 {{ R }} ->
    {{ P }} c1 {{ Q }} ->
    {{ P }} (CSeq c1 c2) {{ R }}.
Proof.
    intros P Q R c1 c2 H1 H2 st st' H12 Pre.
    inversion H12; subst.
    apply (H1 st'0 st'); try assumption.
    apply (H2 st st'0); assumption. 
Qed.
```

#### Conditionals

首先我们可以给出一个较弱的规则：如果断言 Q 在两个分支执行后都成立，它就对整个条件命令成立。
$$
\frac{
    \begin{gathered}
    \{ P \} \quad c_1 \quad \{ Q \} \\
    \{ P \} \quad c_2 \quad \{ Q \} 
    \end{gathered}
}{
    \{ P \} \quad \operatorname{TEST}\; b \;\operatorname{THEN}\; c1 \;\operatorname{ELSE}\; c2 \;\operatorname{FI} \quad \{ R \}
}
$$

这没有利用到在 "$THEN$ 分支中，$b$ 化简为 $true$，而在 $ELSE$ 分支中我们知道它化简为 $false$" 这个信息。我们可以让这个信息作为 c1 和 c2 的假设出现。
$$
\frac{
    \begin{gathered}
    \{ P \land b \} \quad c_1 \quad \{ Q \} \\
    \{ P \land \neg b \} \quad c_2 \quad \{ Q \} 
    \end{gathered}
}{
    \{ P \} \quad \operatorname{TEST}\; b \;\operatorname{THEN}\; c1 \;\operatorname{ELSE}\; c2 \;\operatorname{FI} \quad \{ R \}
}
$$

```
Theorem hoare_if : forall (P Q : Assertion) (b : bexp) c1 c2,
    {{ fun st => P st /\ (beval st b = true) }} c1 {{ Q }} ->
    {{ fun st => P st /\ (beval st b = false) }} c2 {{ Q }} ->
    {{ P }} (CIf b c1 c2) {{Q}}.
Proof.
    intros P Q b c1 c2 HTrue HFalse st st' HE HP.
    inversion HE; subst; eauto.
Qed.
```

#### While Loops

The Hoare rule for while loops is based on the idea of an **invariant**: an assertion whose truth is guaranteed before and after executing a command. An assertion P is an invariant of c if $\{ P \} \quad c \quad \{ P \}$ holds. 
:::indent
Note that in the middle of executing c, the invariant might temporarily become false, but by the end of c, it must be restored.
:::

$$
\frac{
    \begin{gathered}
    \{ P \} \quad c \quad \{ P \} 
    \end{gathered}
}{
    \{ P \} \quad \operatorname{WHILE}\; b \;\operatorname{DO}\; c \;\operatorname{END} \quad \{ P \}
}
$$

But the rule also omits two crucial pieces of information. 
- the loop terminates when b becomes false. So we can strengthen the postcondition in the conclusion.
- the loop body will be executed only if b is true. So we can also strengthen the precondition in the premise.

$$
\frac{
    \begin{gathered}
    \{ P \land b \} \quad c \quad \{ P \} 
    \end{gathered}
}{
    \{ P \} \quad \operatorname{WHILE}\; b \;\operatorname{DO}\; c \;\operatorname{END} \quad \{ P \land \neg b \}
}
$$

断言 $P$ 叫做**循环不变式（invariant of the loop）**。例如，如果 $P$ 是断言 $X = 0$，那么 $P$ 是下述循环的不变式：$\operatorname{WHILE}\; (X = 2) \;\operatorname{DO}\; (X := 1) \;\operatorname{END}$\
:indent[(我们把断言 $P$ 叫做循环不变式并不代表它只是由 $\{ P \} \quad c \quad \{ P \}$ 所保证。Being a loop invariant is different from being an invariant of the body]

```
Theorem hoare_while : forall (P : Assertion) (b : bexp) c,
    {{ fun st => P st /\ (beval st b = true) }} c {{ P }} ->
    {{ P }} (CWhile b c) {{ fun st => P st /\ (beval st b = false) }}.
Proof.
    intros P b c Hhoare st st' He HP.
    remember (CWhile b c) as wcom eqn:Heqwcom.
    induction He;
        try (inversion Heqwcom); subst; clear Heqwcom.
    - (* E_WhileFalse *)
        split; assumption. 
    - (* E_WhileTrue *)
        apply IHHe2. reflexivity.
        apply (Hhoare st st'). assumption.
        split; assumption. 
Qed.
```


##### example

```
{{ X = m /\ Y = n }}
while ~(X = 0) do
    Y := Y - 1;
    X := X - 1
end
{{ X = m /\ Y = n }}
```

Once the outermost precondition and postcondition are chosen, the only creative part in verifying programs using Hoare Logic is finding the right loop invariants. 

To verify this program, we need to find an invariant Inv for the loop. As a first step we can leave Inv as an unknown and build a skeleton for the proof
:indent[(working **from the end of the program to the beginning**, as usual, and without any thinking at all yet).]
```
{{ X = m /\ Y = n }} ->> {{ Inv }} (a)
while ~(X = 0) do
{{ Inv /\ X != 0 }} ->> {{ Inv [X |-> X-1] [Y |-> Y-1] }} (b)
    Y := Y - 1;
{{ Inv [X |-> X-1] }}
    X := X - 1
{{ Inv }}
end
{{ Inv /\ ~(X != 0) }} ->> {{ Y = n - m }} (c)
```
我们需要找到一个 `Inv` 同时满足 (a)、(b)、(c)。One way to find an invariant that simultaneously satisfies these three conditions is by using an iterative process: 

start with a "candidate" invariant (e.g., a guess or a heuristic choice) and check the three conditions above; if any of the checks fails, try to use the information that we get from the failure to produce another.

Guess `Inv = (Y = n - m)`, 
```
{{ X = m /\ Y = n }} ->> {{ Y = n - m }} (a)
while ~(X = 0) do
{{ Y = n - m /\ X != 0 }} ->> {{ Y - 1 = n - m }} (b)
    Y := Y - 1;
{{ Y = n - m }}
    X := X - 1
{{ Y = n - m }}
end
{{ Y = n - m /\ ~(X != 0) }} ->> {{ Y = n - m }} (c)
```
condition (c) is trivially satisfied, conditions (a) and (b) are wrong: the variable `Y` changes during the loop, while `m` and `n` are constant, so the assertion we chose didn't have much chance of being an invariant!\
但是观察程序可以发现，`X` 和 `Y`以相同的速率减小，我们可以利用 (c) 中 `~(X != 0)` 的条件将 `Inv` 改为 `Y - X = n - m`

```
{{ X = m /\ Y = n }} ->> {{ Y - X = n - m }} (a)
while ~(X = 0) do
{{ Y - X = n - m /\ X != 0 }} ->> {{ (Y - 1) - (X - 1) = n - m }} (b)
    Y := Y - 1;
{{ Y - (X - 1) = n - m }}
    X := X - 1
{{ Y - X = n - m }}
end
{{ Y - X = n - m /\ ~(X != 0) }} ->> {{ Y = n - m }} (c)
```

Success! Conditions (a), (b) and (c) all hold now.

---

Property (Strong Progress): If t is a term, then either t is a value or else there exists a term t' such that t --> t'.
:indent[tell us something interesting about values: they are exactly the terms that cannot make progress (normal forms) in this sense.]
