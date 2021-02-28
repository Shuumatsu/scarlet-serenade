---
title: Types and Programming Languages
---

## Introduction

**Definition**: A **type system** is a tractable syntactic method for proving the absence of certain program behaviors by classifying phrases according to the kinds of values they compute. \
:indent[- this definition identifies type systems as tools for reasoning about programs.]

**Definition**: A **safe language** is one that protects its own abstractions. Every high-level language provides abstractions of machine service. Safety refers to the language's ability to guarantee the integrity of these abstractions and of higher-level abstractions introduced by the programmer using the definitional facilities of the language.


Language safety can be achieved by static checking, but also by run-time checks that trap nonsensical operations just at the moment when they are attempted and stop the program or raise an exception. For example, Scheme is a safe language, even though it has no static type system


|        | statically checked      | dynamically checked      |
|--------|-------------------------|--------------------------|
| safe   | ML, Haskell, Java, etc. | Lisp, Scheme, Perl, etc. |
| unsafe | C, C++, etc.            |                          |


Language safety is seldom absolute. Safe languages often offer programmers "escape hatches," such as foreign function calls to code written in other, possibly unsafe, languages.


## Evaluation

**Definition**: An **instance** of an inference rule is obtained by consistently replacing each metavariable by the same term in the rule's conclusions and all its premises.
- for example, `if true then true else false -> true` is an instance of $\text{if } v_1 \text{ then } v_2 \text{ else } v_3$

**Definition**: **evaluation relation** on terms, written $t \rightarrow t'$, is defined by inference rules.

:br

**Definition**: A rule is **satisfied** by a relation if, for each instance of the rule, either the conclusion is in the relation or one of the premises is not.
- 没懂...

:br

**Definition**: A term $t$ is **normal form** if no evaluation rule applies to it - i.e., there is no $t'$ such that $t \rightarrow^{*} t'$


**Definition**: A term is **stuck** if it is in normal form but not a value.
:div(indent="1")
    "Stuckness" gives us a simple notion of run-time error for our simple machine.
    Intuitively, it characterizes the situations where the operational semantics does not know what to do because the program has reached a "meaningless state." 
    - (segmentation faults, execution of illegal instructions, etc.

:br
:p --- 这两个定理好像是针对特定的规则的

**Determinacy of One-Step Evaluation** Theorem: If $t \rightarrow u$ and $t \rightarrow v$, then $u = v$
Proof: by induction on a derivation of $t \rightarrow u$.
- We can as well say that we are performing induction on the structure of $t$, since t he structure of an "evaluation derivation" directly follows the structure of the term being reduced.
- TODO...

**Uniqueness of Normal Forms** Theorem: If $t \rightarrow^{*} u$ and $t \rightarrow^{*} v$, where $u$ and $v$ are both normal forms, then $u = v$
Proof: Just a corollary of the determinacy of single-step evaluation.

## An ML Implementation of Arithmetic Expressions

:p 
    :span implementation: 
    :a(href="https://github.com/Shuumatsu/TAPL/tree/chapter-4") https://github.com/Shuumatsu/TAPL/tree/chapter-4

:div(class="flex")
    :div 
        syntactic forms:
        $$
        \begin{aligned}
        t \; ::= \;  & \ldots                   \quad \quad & terms \\
                    & 0                        \quad \quad & \text{constant zero} \\
                    & \operatorname{succ} t    \quad \quad & \text{successor} \\
                    & \operatorname{pred} t    \quad \quad & \text{pred} \\
                    & \operatorname{iszero} t  \quad \quad & \text{iszero} \\
        \\
        v \; ::= \;  & \ldots                   \quad \quad & values \\
                    & nv                       \quad \quad & \text{numeric value} \\
        \\
        nv \; ::= \; & \ldots                   \quad \quad & \text{numeric values} \\
                    & 0                        \quad \quad & \text{zero value} \\
                    & \operatorname{succ} nv   \quad \quad & \text{successor value} \\
        \end{aligned}
        $$

    :div
        evaluation rules:
        $$
        \begin{aligned}
        & \frac
            {t _{1} \longrightarrow t _{1}'}
            {\operatorname{succ} t_{1} \rightarrow \operatorname{succ} t_{1}'}
        & \text{(E-Succ)} \\
        \\
        & \operatorname{pred} 0 \rightarrow 0
        & \text{(E-PredZero)} \\
        \\
        & \operatorname{pred}\left(\operatorname{succ} n v_{1}\right) \rightarrow n v_{1}
        & \text{(E-PredSucc)} \\
        \\
        & \frac
            {t _{1} \longrightarrow t _{1}'}
            {\operatorname{pred} t_{1} \rightarrow \operatorname{pred} t_{1}'}
        & \text{(E-Pred)} \\
        \\
        & \operatorname{iszero} 0 \rightarrow true
        & \text{(E-IszeroZero)} \\
        \\
        & \operatorname{iszero}(\operatorname{succ} nv_1) \rightarrow false
        & \text{(E-IszeroSucc)} \\
        \\
        & \frac
            {t _{1} \longrightarrow t _{1}'}
            {\operatorname{iszero} t_{1} \rightarrow \operatorname{iszero} t_{1}'}
        & \text{(E-IsZero)} \\
        \end{aligned}
        $$

## The Untyped Lambda Calculus
In the lambda-calculus **everything is a function**: the arguments accepted by functions are themselves functions and the result returned by a function is another function

When discussing the syntax of programming languages, it is useful to distinguish two levels of structure.
- - The **concrete syntax** of the language refers to the string of characters that programmers directly read and write.
- - The **abstract syntax** is a much simpler internal representation of programs as labeled trees (called abstract syntax trees).

:br

**Definition**: An occurrence of the variable $x$ is said to be **bound** when it occurs in the body t of an abstraction $\lambda x.t$.
**Definition**: An occurrence of $x$ is **free** if it appears in a position where it is not bound by an enclosing abstraction on $x$. 

**Definition**: A term with no free variables is said to be **closed**; closed terms are also called **combinators**


### Recursion 

**Definition**: Terms with no normal form are said to **diverge**.
- e.g., $\omega = (\lambda x. x x) (\lambda x. x x)$


TODO: p61


## Simple Types



## The Curry-Howard Correspondence (propositions as types analogy)

The $\rightarrow$ type constructor comes with typing rules of two kinds:
- 1. an "introduction rule (T-ABS)" describing how elements of the type can be created
- 2. an "elimination rule (T-APP)" describes how elements of the type can be used

The introduction/elimination terminology arises from a connection between type theory and logic known as the **Curry-Howard Correspondence**.
The idea is that, in constructive logics, a proof of a proposition $P$ consists of concrete **evidence** for P.
- For example, a proof of a proposition $P \supset Q$ can be viewed as a  mechanical procedure that, given a proof of $P$, constructs a proof of $Q$ - or, if you like, a proof of $Q$ **abstracted** on a proof of $P$

:br 
:br 

Most compilers for full-scale programming langues actually avoid carrying annotations at rutile: they are used during typechecking

**Definition**: The **erasure** of a simply typed term $t$ is defined as follows
$$
\begin{aligned}
\operatorname{erase}(x) \quad &= x \\
\operatorname{erase}(\lambda x: T_1. t_2) \quad &= \lambda x. t_2 \\
\operatorname{erase}(t_1 t_2) \quad &= \operatorname{erase}(t_1) \operatorname{erase}(t_1) \\
\end{aligned}
$$

**evaluation commutes with erasure**: we reach the same term by evaluating and then erasing as we do by erasing and then evaluating.
- If $t \rightarrow t'$ under the typed evaluation relation, then $\operatorname {erase}( t ) \rightarrow \operatorname{erase} (t')$


**Definition**: A term $m$ in the untyped lambda-calculus is said to be **typable** in $\lambda_{\rightarrow}$ if there are some simply typed term $t$, type $T$, and context $\Gamma$ such that $\operatorname{erase}(t) = m$ and $\Gamma \vdash t: T$

:h3 Curry-Style vs. Church-Style 

**Curry-Style**: We first define the terms, then define a semantics showing how they behave, then give a type system that rejects some terms whose behaviors we don't like. Semantics is prior to typing.

**Church-Style**: We first define terms, then identify the well-typed terms, then give semantics just to these. Typing is prior to semantics.
- In deed, strictly speaking, what we actually evaluate in Church-style systems is typing derivations, not terms.

implicitly typed presentations of lambda-calculi are often given in the Curry style, while Church-style presentations are common only for explicitly typed systems.


