---
title: Types and Programming Languages | The Simply Typed Lambda Calculus
---

**static semantics**: grammar and typing rules of the language

**dynamic semantics**: evaluation rules

---

The structure of the rules that we will see: each statement, whether a premise or a conclusion, typically involves a context,
a term and a classification. 

For example, here is the rule $\rightarrow-e l i m$. Both the premises and the conclusion take the form $\Gamma \vdash M: A$, which we
can read "In the context $\Gamma$, $M$ has type $A$."

$$
\begin{array}{c}
\Gamma \vdash M: A \rightarrow B \\
\Gamma \vdash N: A \\
\hline \Gamma \vdash M N: B
\end{array} \rightarrow-\mathrm{elim}
$$



---

The name "simply typed lambda calculus" is rather unwieldy, so we’ll use
the traditional and shorter name $\lambda^{\rightarrow}$

For uniformity we will present both the grammar of the language and the type rules as inference rules.

The elements of the lambda calculi described here are divided into three "sorts":

- **terms**
- **types**,  We write $M : A$ to say that the term $M$ has the type $A$.
- **kinds**, which you can think of as the types of type expressions.  We write $T :: K$ to say that the type expression $T$ has the kind $K$.

## Kinding rules in $\lambda^{\rightarrow}$


In $\lambda^{\rightarrow}$,  there is a single kind, called $*$. 


There are two kinding rules, which describe how to form types:

The $\text { kind- } \mathcal{B}$ rule introduces a base type $\mathcal{B}$ of kind $*$. Base types correspond to primitive types available in most programming languages.

$$
\frac{}{\Gamma \vdash \mathcal{B}:: *} \text { kind-} \mathcal{B}
$$

The $\text{kind-}\rightarrow$ gives us a way of forming function types $A \rightarrow B$ from types $A$ and $B$.

$$
\frac{\Gamma \vdash A:: * \quad \Gamma \vdash B:: *}{\Gamma \vdash A \rightarrow B:: *} \text { kind-} \rightarrow
$$

For example, the type $(\mathcal{B} \rightarrow \mathcal{B}) \rightarrow \mathcal{B}$ can be derived as follows:

$$
\frac{
    \frac{
        \frac{}{\Gamma \vdash \mathcal{B}:: *} kind- \mathcal{B} \quad \frac{}{\Gamma \vdash \mathcal{B}:: *} kind- \mathcal{B}
    }{
        \Gamma \vdash \mathcal{B} \rightarrow \mathcal{B}:: *
    } \text { kind- } \rightarrow
    \quad
    \frac{}{\Gamma \vdash \mathcal{B}:: *} \text { kind- } \mathcal{B}
}{
    \Gamma \vdash(\mathcal{B} \rightarrow \mathcal{B}) \rightarrow \mathcal{B}:: *
} \text { kind- } \rightarrow
$$


## Environment rules in $\lambda^{\rightarrow}$

Environments associate variables with their classifiers — i.e. 
- term variables with types
- and type variables with kinds.

In $\lambda^{\rightarrow}$ there are two rules for forming environments:

$$
\frac{}{\cdot \text { - is an environment }} \Gamma-
$$

$$
\frac{\Gamma \text { is an environment } \quad \Gamma \vdash A:: *}{\Gamma, x: A \text { is an environment }} \Gamma-:
$$

## Typing rules in $\lambda^{\rightarrow}$

The tvar rule shows how to type open terms (i.e. terms with free variables).

$$
\frac{x: A \in \Gamma}{\Gamma \vdash x: A} \text { tvar }
$$

The introduction rule $\rightarrow \text { -intro }$ shows how to form a term $\lambda x: A . M$ of type $A \rightarrow B$

$$
\frac{\Gamma, x: A \vdash M: B}{\Gamma \vdash \lambda x: A \cdot M: A \rightarrow B} \rightarrow \text { -intro }
$$

The elimination rule $\rightarrow \text{ -elim }$ shows how to apply terms of function type.

$$
\begin{array}{c}
\Gamma \vdash M: A \rightarrow B \\
\Gamma \vdash N: A \\
\hline \Gamma \vdash M N: B
\end{array} \rightarrow \text{-elim}
$$

The $\rightarrow \text { -intro }$ and $\rightarrow \text{ -elim }$ form the first **introduction-elimination pair**.

---

## Extensions

### Adding products

We introduce a new type: product type $A \times B$

#### Kinding rules for $\times$

There is a new way of forming types, so we need a new kinding rule.

$$
\frac{\Gamma \vdash A:: * \quad \Gamma \vdash B:: *}{\Gamma \vdash A \times B:: *} \text { kind- } \times
$$

#### Typing rules for $\times$

There are three new typing rules:

The $\times \text { -intro }$ rule shows how to build pairs: a pair $\langle M, N\rangle$ of type $A \times B$ is built from terms $M$ and $N$ of types $A$ and $B$.

$$
\begin{array}{c}
\Gamma \vdash M: A \\
\Gamma \vdash N: B \\
\hline \Gamma \vdash\langle M, N\rangle: A \times B
\end{array} \times \text { -intro }
$$

The $\times- \text{elim-1}$ and $\times- \text{elim-2}$ rules show how to deconstruct pairs.

$$
\frac{\Gamma \vdash M: A \times B}{\Gamma \vdash \mathrm{fst} M: A} \times- \text{elim-1}
$$

$$
\frac{\Gamma \vdash M: A \times B}{\Gamma \vdash \mathrm{snd} M: B} \times- \text{elim-2}
$$


### Adding sums

We next extend $\lambda^{\rightarrow}$ with sum types, which correspond to a simple version of variants.

#### Kinding rules for $+$

There is a new way of forming types, so we need a new kinding rule.

$$
\frac{\Gamma \vdash A:: * \quad \Gamma \vdash B:: *}{\Gamma \vdash A+B:: *} \text { kind- }+
$$

#### Typing rules for $+$

There are three new typing rules:

The $+ \text { -intro-1 }$ and $+ \text { -intro-2 }$ rules show how to build values of sum type by injecting with $inl$ or $inr$\
In order to maintain the property that each term has a unique type we also require a type argument to $inl$ and $inr$

$$
\frac{\Gamma \vdash M: A}{\Gamma \vdash \operatorname{inl}[B] M: A+B} + \text { -intro-1 }
$$

$$
\frac{\Gamma \vdash N: B}{\Gamma \vdash \operatorname{inr}[A] N: A+B} + \text { -intro- } 2
$$

The $+ \text { -elim }$ rule shows how to deconstruct sums.

$$
\begin{array}{c}
\Gamma \vdash L: A+B \\
\Gamma, x: A \vdash M: C \\
\Gamma, y: B \vdash N: C \\
\hline \Gamma \vdash \text { case L of x.M | y.N : C }
\end{array} + \text { -elim }
$$

---

In the lambda-calculus **everything is a function**: the arguments accepted by functions are themselves functions and the result returned by a function is another function

When discussing the syntax of programming languages, it is useful to distinguish two levels of structure.
- The **concrete syntax** of the language refers to the string of characters that programmers directly read and write.
- The **abstract syntax** is a much simpler internal representation of programs as labeled trees (called abstract syntax trees).


**Definition**: An occurrence of the variable $x$ is said to be **bound** when it occurs in the body $t$ of an abstraction $\lambda x.t$.
**Definition**: An occurrence of $x$ is **free** if it appears in a position where it is not bound by an enclosing abstraction on $x$. 

**Definition**: A term with no free variables is said to be **closed**; closed terms are also called **combinators**

## Operational Semantics

redex... / beta-reduction...

---

Evaluation Strategies:
- **full beta-reduction**
- **normal order strategy**, the left most outer most
- **call by name strategy**

## Recursion

**Definition**: Terms with no normal form are said to **diverge**.
- e.g., $\omega = (\lambda x. x x) (\lambda x. x x)$

TODO: p61

