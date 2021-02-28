---
title: Types and Programming Languages | The Curry-Howard Correspondence (propositions as types analogy)
---


The $\rightarrow$ type constructor comes with typing rules of two kinds:
- 1. an "introduction rule (T-ABS)" describing how elements of the type can be created
- 2. an "elimination rule (T-APP)" describes how elements of the type can be used

The introduction/elimination terminology arises from a connection between type theory and logic known as the **Curry-Howard Correspondence**.
The idea is that, in constructive logics, a proof of a proposition $P$ consists of concrete **evidence** for P.
- For example, a proof of a proposition $P \supset Q$ can be viewed as a  mechanical procedure that, given a proof of $P$, constructs a proof of $Q$ - or, if you like, a proof of $Q$ **abstracted** on a proof of $P$