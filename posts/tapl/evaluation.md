---
title: Types and Programming Languages | Evaluation
---

**Definition**: An **instance** of an inference rule is obtained by consistently replacing each metavariable by the same term in the rule's conclusions and all its premises.
:indent[- for example, `if true then true else false -> true` is an instance of $\text{if } v_1 \text{ then } v_2 \text{ else } v_3$]

**Definition**: A rule is **satisfied** by a relation if, for each instance of the rule, either the conclusion is in the relation or one of the premises is not. 

**Definition**: **evaluation relation** on terms, written $t \rightarrow t'$, is defined by inference rules.

**Definition**: The **one-step evaluation relation** $\longrightarrow$ is the smallest binary relation on terms satisfying the given evaluation rules.
:::indent
The force f word smallest here is that a statement $t \longrightarrow t'$ is derivable iff
- it is an instance of one of the axioms
- or e it is the conclusion of an instance of rule whose premise is derivable
:::

**Definition**: The **multi-step evaluation relation** $\longrightarrow^{*}$ is the reflexive, transitive closure of one-step evaluation. 

**Definition**: A term $t$ is **normal form** if no evaluation rule applies to it 
- i.e., there is no $t'$ such that $t \rightarrow^{*} t'$

**Definition**: A term is **stuck** if it is in normal form but not a value.
"Stuckness" gives us a simple notion of **run-time error** for our simple machine.
Intuitively, it characterizes the situations where the operational semantics does not know what to do because the program has reached a "meaningless state." (segmentation faults, execution of illegal instructions, etc.


