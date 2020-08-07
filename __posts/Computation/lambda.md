ast for lambda terms

bound var: a var that's associated with some $\lambda$
free var: a var not associated with any $\lambda$
    直观上来说，如果 x is bound 则 x 在一个左边为 x 的 $\lambda$ 的子树中

substitution:

naive: 将 function body 中所有的 x 替换掉
    - problem 1. $(\lambda x.\lambda x. xy) z$ 里层的 x 背错误的替换了
    - problem 2. $(\lambda x.\lambda y.xy)y$ 

解决 problem 1：仅替换 function body 中的 free var
    e.g. $(\lambda x.\lambda x. xy) z$ 的 function body 为 $x.\lambda x. xy$ 最外层的 x 为 free var，里层的为 bound，则仅替换最外层的 x

解决 problem 2: alpha-reduction
    function arg name 是无所谓的，将其替换为 function body 中从未出现的变量
    e.g. $(\lambda x.\lambda y.xy) y$ 中里层的 y，其 function body $xy$ 中没有出现过 z，则将参数名替换成 z 有 $(\lambda x.\lambda z.xz) y$

beta-reduction：$(\lambda x . M ) N \rightarrow_{\beta} M [ N / x ]$ (从右到左称作 beta expansion)
	结合上面两种方式. The left-hand side ((λx.M)N) is called the **redex**. The right-hand side (M[N/x]) is called the **contractum** and the notation means M with all free occurrences of x replaced with N in a way that avoids capture. 
 e.g. 

```
((λx.λy.x)y)z
        ->  ((λy.x)[y/x])z   // substitute y for x in the body of "λy.x"
        ->  ((λy'.x)[y/x])z  // after alpha reduction
        ->  (λy'.y)z         // first beta-reduction complete!
        ->  y[z/y']          // substitute z for y' in "y"
        ->  y                // second beta-reduction complete!
```

Note that the term "beta-*reduction*" is perhaps misleading, since doing beta-reduction does not always produce a smaller lambda expression. In fact, a beta-reduction can:

- not change: `(λx.xx)(λx.xx) → (λx.xx)(λx.xx)`
- increase: `(λx.xxx)(λx.xxx) → (λx.xxx)(λx.xxx)(λx.xxx) → (λx.xxx)(λx.xxx)(λx.xxx)(λx.xxx)`
- decrease: `(λx.xx)(λa.λb.bbb) → (λa.λb.bbb)(λa.λb.bbb) → λb.bbb`

A computation is finished when there are no more redexes (no more applications of a function to an argument).

We say that a lambda expression without redexes is in **normal form**, and that a lambda expression **has**  a normal form iff there is some sequence of beta-reductions and/or expansions that leads to a normal form.

`(λx.λy.y)((λz.zz)(λz.zz))`. This lambda expression contains two redexes:
    - the whole expression
    - the argument itself: `((λz.zz)(λz.zz))`
如果我们先对 argument 做 beta reduction 则永远不会得到 norm form 但是如果先对第一个 redex 做则可以

**leftmost-outermost** or **normal-order-reduction (NOR)** 能够保证得到 norm form (如果存在的话)
Definition: An outermost redex is a redex that is not contained inside another one. (Similarly, an innermost redex is one that has no redexes inside it.)
In terms of the abstract-syntax tree, an "apply" node represents an outermost redex iff
    - it represents a redex (its left child is a lambda), and
        - it has no ancestor "apply" node in the tree that also represents a redex.

```
                            apply  <-- not a redex
                           /     \
an outermost redex --> apply      apply <-- another outermost redex
                      /    \      /    \
                     λ     ...   λ      apply  <-- redex, but not outermost
                    / \         / \     /   \     
                  ... ...      ... ... λ    ...
```
Normal-order reduction is like call-by-name parameter passing. The intuition is that 
> If it is a function that ignores its argument, then reducing that redex can make other redexes (those that define the argument) "go away"; however, reducing an argument will never make the function "go away".

- call by value: leftmost-innermost (applicative-order reduction (AOR))
- call by name: leftmost-outermost (normal-order-reduction (NOR))
- call by need: like call by name but the result of the evaluation is saved and is then reused for each subsequent use of the formal.