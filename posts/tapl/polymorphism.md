---
title: Types and Programming Languages | Polymorphism
---

**Definition**: A **type substitution** is a finite mapping from type variables to types.

For example, we write $[X \mapsto T, Y \mapsto U]$ for the substitution that associates $X$ with $T$ and $Y$ with $U$. 

Application of a substitution to a type is defined in the obvious way.
$$
\sigma(\mathrm{X}) \quad=\left\{\begin{array}{ll}
\mathrm{T} & \text { if }(\mathrm{X} \mapsto \mathrm{T}) \in \sigma \\
\mathrm{X} & \text { if } \mathrm{X} \text { is not in the domain of } \sigma
\end{array}\right.
$$

## Two Views of Type Variables

Suppose that $t$ is a term containing type variables and $\Gamma$ is an associated context (possibly also containing type variables).

- "Are all substitution instances of $t$ well typed?" That is, for every $\sigma$, do we have $\sigma \Gamma \vdash \sigma \mathrm{t}: \mathrm{T}$ for some $T$?

:::indent
Holding type variables abstract in this way leads us to **parametric polymorphism**.
:::

- "Is some substitution instance of $t$ well typed?" That is, can we find a $\sigma$such that $\sigma \Gamma \vdash \sigma \mathrm{t}: \mathrm{T}$ for some $T$?

:::indent
The original term t may not even be well typed. e.g., $\lambda f: Y . \lambda a: X . f(f a)$ is not typable as it stands, but if we replace $Y$ by $X \rightarrow X$, we obtain $\lambda f: X \rightarrow X . \lambda a: X . f(f a)$ which is well typed.

Looking for valid instantiations of type variables leads to the idea of **type reconstruction (sometimes called type inference)**.

**Definition**: Let $\Gamma$ be a context and $t$ a term. A **solution** for $(\Gamma, t)$ is a pair $(\sigma, T)$ such that $\sigma \Gamma \vdash \sigma \mathrm{t}: \mathrm{T}$
:::

## Constraint-Based Typing

## Unification

## Principal Types

If there is some way to instantiate the type variables in a term so that it becomes typable, then there is a **most general or principal** way of doing so. We now formalize this observation.

**Definition**: A **principal solution** for ...
P351

## Implicit Type Annotations

Languages supporting type reconstruction typically allow programmers to completely omit type annotations. 


One way to achieve this is simply to make the parser fill in omitted annotations with freshly generated type variables.

A better alternative is to add un-annotated abstractions to the syntax of terms and a corresponding rule to the constraint typing relation


## Let-Polymorphism (or DamasMilner polymorphism

For example, if we write
```ocaml
let double = fun (f: X -> X) -> fun (a: X) -> f(f(a)) in
let a = double (fun (x: Nat) -> succ (succ x)) 1 in
let b = double (fun (x: Bool) -> x) false in ...
```

Without let-polymorphism, constraints `X -> X = Nat -> Nat` and `X -> X = Bool -> Bool` will be generated. These constraints cannot be satisfied.

What we'd like is to do is to associate a different variable `X` with each use of `double`.

In essence, what we’ve done is to change the typing rules for let so that they perform a step of evaluation before calculating types.