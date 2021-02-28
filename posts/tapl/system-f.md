---
title: Types and Programming Languages | System F
---

Idea: lambda abstraction over type variables, defining functions
over types.

$$
\begin{aligned}
\text { id }=\Lambda X . \lambda x: X . x \\
id : \forall X. X \rightarrow X 
\end{aligned}
$$

---

**System F (also known as the polymorphic lambda calculus)** = $\lambda^{\rightarrow}$ +  parametric polymorphism.



As an example, the fact that the identity function can have any type of the form $\mathrm{A} \rightarrow \mathrm{A}$ would be formalized in System F as the judgment

$$
\vdash \Lambda \alpha . \lambda x^{\alpha} . x: \forall \alpha . \alpha \rightarrow \alpha
$$

The upper-case $\Lambda$ is traditionally used to denote type-level functions, as opposed to the lower-case $\lambda$ which is used for value-level functions. (The superscripted $\alpha$ means that the bound x is of type $\alpha$; the expression after the colon is the type of the lambda expression preceding it.)


## Environment rules for $\forall$

$$
\frac{\Gamma \text { is an environment } \quad K \text { is a kind }}{\Gamma, \alpha:: K \text { is an environment }} \Gamma-::
$$

## Kinding rules for $\forall$

System F extends $\lambda^{\rightarrow}$ with a type-level operator $\forall \alpha:: *.$ − that binds a variable $\alpha$ within a particular scope.

There are two new kinding rules:

The $\text { kind- } \forall$ rule builds **universal** types $\forall \alpha:: K.A$

$$
\frac{\Gamma, \alpha:: K \vdash A:: *}{\Gamma \vdash \forall \alpha:: K . A:: *} \text { kind- } \forall
$$

The tyvar rule is a type-level analogue of the tvar rule: it allows type variables to appear within type expressions, making it possible to build open types (i.e. types with free variables).

$$
\frac{\alpha:: K \in \Gamma}{\Gamma \vdash \alpha:: K} \text { tyvar }
$$

These rules involve a new kind of variable into the language. Type variables are bound by $\forall$ and (as we shall see) by $\Lambda$, and can be used in place of concrete types in expressions.

## Typing rules for $\forall$

Since we have a new type constructor $\forall$, we need a new pair of introduction and elimination rules:

The $\forall \text { -intro }$ rule shows how to build values of type $\forall \alpha:: K . A$ — that is, polymorphic values.

$$
\frac{\Gamma, \alpha:: K \vdash M: A}{\Gamma \vdash \Lambda \alpha:: K . M: \forall \alpha:: K . A} \forall \text { -intro }
$$

The $\forall \text { -elim }$ rule shows how to use values of polymorphic type via a second form of application: applying a (suitably-typed) term to a type.

$$
\frac{\Gamma \vdash M: \forall \alpha:: K . A \quad \Gamma \vdash B:: K}{\Gamma \vdash M[B]: A[\alpha:=B]} \forall \text { -elim }
$$ 

## Extensions

### Adding Existentials

A value of type $\exists X.T$ is a package with a witness type $T'$ for $X$  and a value term $t : [X => T']T$
$$
pack X = T' \text{ with } t \text{ as } T : \exists X.T
$$

It turns out that there is indeed a useful notion of $\exists$ types: just as $\forall$ is used for terms which can be instantiated at any type, $\exists$ can be used to form types for which we have some implementation, but prefer to leave the details abstract.

In fact, it is possible to encode existential types using universal types. But for the time being, will find it more convenient to introduce them directly as an extension to System F.

#### Kinding rules for $\exists$

$$
\frac{\Gamma, \alpha:: K \vdash A:: *}{\Gamma \vdash \exists \alpha:: K . A:: *} \text {kind}-\exists 
$$

#### Typing rules for $\exists$

The $\exists-intro$ rule shows how to build values of existential type using a new construct, `pack`.

$$
\frac{
    \Gamma \vdash M: A[\alpha:=B] \quad
}{
    \Gamma \vdash \operatorname{pack} B, M \text { as } \exists \alpha:: K . A: \exists \alpha:: K . A
} \exists-intro
$$

The $\exists-elim$ rule shows how to use values of existential type using a new construct open

$$
\begin{array}{c}
\Gamma \vdash M: \exists \alpha:: K . A \\
\Gamma, \alpha:: K, x: A \vdash M^{\prime}: B \\
\hline \Gamma \vdash \text { open } M \text { as } \alpha, x \text { in } M^{\prime}: B
\end{array} \exists-elim
$$

## Encoding data types in System F