---
title: Types and Programming Languages | Nameless Representation of Terms
---

We can devise some "canonical" representation of variables and terms that does not require renaming.

De Bruijn's idea was that we can represent terms by making variable occurrences point directly to their binders, rather than referring to them by name.  This can be accomplished by replacing named variables by natural numbers, where the number k stands for "the variable bound by the k'th enclosing $\lambda$.
- e.g., $\lambda x.x$ corresponds to the nameless term $λ.0$
- e.g., $\lambda x. \lambda y. x (y x)$ corresponds to the nameless term $\lambda. \lambda. 1 (0 1)$

**Nameless terms** are also sometimes called **de Bruijn terms**,  and the numeric variables in them are called **de Bruijn indices**. Compiler writers use the term **static distances** for the same concept.


## Converting to Nameless Representation

We will define a function $db_{\Gamma}$ mapping terms to nameless representation, and to deal with terms containing free variables, we need the idea of a **naming context** $\Gamma$. 
- For example, suppose we want to represent $\lambda x. y x$ as a nameless term. But we cannot see the binder for y, so it is not clear how "far away" it might be and we do not know what number to assign to it. 
- suppose we have $\Gamma=x \rightarrow 4 ; y \rightarrow 3 ; z \rightarrow 2 ; a \rightarrow 1 ; b \rightarrow 0$ then 
-- - $x (y z)$ would be represented as $4 \, (3 \, 2)$
-- - $\lambda w. y w$ would be represented as $\lambda. 4 \, 0$. because of the abstraction of $w$, the distance of $y$ is increased by 1, hence 3 + 1
Let's simplify $\Gamma$ to be a sequence of var names: 
- $\Gamma=x_{n-1}, \ldots, x_{1}, x_{0}$
Then we'll define $\operatorname{dom}(\Gamma)=\left\{x_{n-1}, \ldots, x_{1}, x_{0}\right\}$ and $\Gamma(x) = \text{rightmost index of } x \text{ in } \Gamma$
So $db_{\Gamma}$ can be defined as:
- - $db_{\Gamma}(x)                         = \Gamma(x)$
- - $db_{\Gamma}(\lambda x . t)             = \lambda . d b_{\Gamma, x}(t)$
- - $db_{\Gamma}\left(t_{1} t_{2}\right)    = d b_{\Gamma}\left(t_{1}\right) d b_{\Gamma}\left(t_{2}\right)$
The we have
$$
\begin{aligned}
d b_{x, y, z}(\lambda x \cdot y x) &=\lambda \cdot d b_{x, y, z, x}(y x) \\
&=\lambda \cdot d b_{x, y, z, x}(y) d b_{x, y, z, x}(x) \\
&=\lambda \cdot 20
\end{aligned}
$$

:br 



Note that each closed term has just one de Bruijn representation, and two ordinary terms are equivalent modulo renaming of bound vars iff they have the same de Bruijn representation.
- (没懂，modulo 是啥 

:br

We need to keep track of how many free vars each term may contain. That is, we distinguish the sets of terms with no free vars (called the 0-terms), terms with at most one free vars (1-terms), and so on.

**Definition**: let $T$ be the smallest family of sets $\{ T _{0}, T _{1}, T _{2}, \ldots\}$ such that 
- 1. $k \in T_n$ whenever $0 \leq k \leq n$
- 2. if $t_1 \in T_n$ and $n > 0$ then $\lambda. t_1 \in T_{n - 1}$
- 3. if $t_1 \in T_n$ and $t_2 \in T_n$ then $(t_1 t_2) \in T_n$
The elements of $T_n$ are terms with at most $n$ free vars, numbered between $0$ and $n - 1$.


## Substitution on Nameless Terms

When a substitution goes under a λ-abstraction, as in $([x \rightarrow s] \lambda y. x)$, the context in which the substitution is taking places becomes one var longer than the original, so we need to increment the indices of the free vars inside $s$. We define one auxiliary operation called **shifting** to do this.

```
(* 每当 `term` 被代入到一个 abs 的 body 内，都需要将其所有自由变量移动一位 *)
let shift term =
  (* 根据与当前深度判断是否是自由变量 *)
  let rec h depth t =
    match t with
    (* 如果变量的编号大于等于当前深度，说明 its binder is outside of `term` *)
    (* 那么对于 `term` 来说它是一个自由变量，否则不是 *)
    | TmVar n -> TmVar (if n >= depth then n + 1 else n)
    (* 当前深度 + 1 *)
    | TmAbs (arg_name, body) -> TmAbs (arg_name, h (depth + 1) body)
    | TmApp (t1, t2) -> TmApp (h depth t1, h depth t2)
  in
  (* it starts off at 0 (meaning all vars should be shifted) *)
  h 0 term

let rec substitute from to' term =
  match term with
  (* 如果刚好是目标变量，那么替换 *)
  | TmVar n -> if from = n then to' else TmVar n
  (* 因为更深入了一层，所以需要 shift 一次 `to'` *)
  (* 因为更深入了一层，所以目标变量离 its binder 的距离也增加了 1 *)
  | TmAbs (arg_name, body) ->
      TmAbs (arg_name, substitute (from + 1) (shift to') body)
  | TmApp (t1, t2) -> TmApp (substitute from to' t1, substitute from to' t2)

let app (TmApp (TmAbs (_, body), t2)) =
  (* 这里 `t2` 进入了更深的一层，所以需要 `shift` 一次 *)
  (* 当 reducing redex，function body 的深度减小了一，则其中所有的自由变量需要负向移动一位，
     我们可以通过与 `shift` 相同的方式定义一个 `unshift` *)
  unshift (substitute 0 (shift t2) body)

```