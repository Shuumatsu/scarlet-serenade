---
title: Types and Programming Languages | Subtyping
---

**Definition**: We say that $S$ is a **subtype** of $T$, written $S <: T$, to mean that any term of type $S$ can safely be used in a context where a term of type $T$ is expected.
- This view of subtyping is often called the **principle of safe substitution**.

This leads to a new typing rule - the so-called rule of **subsumption**

$$
\frac{
  \Gamma \vdash t : T \quad S <: T
}{
  \Gamma \vdash t : T
}
$$

## The Subtype Relation 

The subtype relation is **reflexive** and **transitive**.