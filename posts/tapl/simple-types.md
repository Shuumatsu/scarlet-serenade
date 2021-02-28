---
title: Types and Programming Languages | Simple Types
---


**Definition**: A term $t$ is **typable (or well typed)** if there is some $T$ such that $t: T$

## Safety = Progress + Preservation

**Progress** theorem: A well-typed term is not stuck (either it is a value or it can take a step according to the evaluation rules)
**Preservation** theorem: If a well-typed term takes a step of evaluation, then the resulting term is also well typed.
- If $e: \tau$ and $e \longmapsto e'$, then $e': \tau$.

**Definition**: Languages in which type annotations in terms are used to guide the typechecker are called **explicitly typed**.
**Definition**: Languages in which we ask the typechecker to infer or reconstruct his information are called **implicitly typed**.

**Definition**: A **typing context** (also called a **type enviroment**) $\Gamma$ is a sequence of variables and their types.

We write $dom(\Gamma)$ for the set of variables bound by $\Gamma$

An **inversion lemma** records a collection of observations about how typing derivations are built: the clause for each syntactic form tell use "if a term of this form is well typed, then its subterms must have types of these forms..."
- e.g., if $\Gamma \vdash \lambda x : T _{1} \cdot t _{2}: R$, then $R = T _{1} \rightarrow R _{2}$ for some $R_2$ with $\Gamma, x : T _{1} \vdash t _{2}: R _{2}$