##                去除无用符号

1. 去除非产生符号和所有包含这些符号的产生式
    （$A \rightarrow B | C$ 是两条产生式，不能因为 $B$ 是非产生符号就全删了
2. 去除不可达符号

需要注意顺序，因为去除非产生符号后可能会产生新的不可达符号，例如某语法如下

$$
\begin{aligned}
S &\rightarrow BC \\
C &\rightarrow a
\end{aligned}
$$

$B$ 是非产生符号，则需要将其以及包含他的产生式删掉，则 C 将变得不可达

###  去除非产生符号和所有包含这些符号的产生式

找到所有的产生符号，则剩下的为非产生符号

1. 所有的终结符都是产生符号，他们产生自己
2. 对 $A \rightarrow \alpha$，若其中 $\alpha$ 为所有产生符号构成的串，则 $A$ 也是产生符号

可以用数学归纳法证明这样可以找到所有的产生符号
    （*主要证明这个掌握一下技巧，我们对推导步数归纳的时候除了从初始变元开始计算步数，也可以反向从结果倒推步数*
1. 某个符号可以 0 步推导到达产生符号（其实就是终结符），则它一定是产生符号且可以被我们的步骤发现
2. 假设命题对 n 步以内推导可以到达产生符号的情况成立。
现考虑符号 A，它需要 n + 1 步推导到达产生符号。不失一般性的，假设第一步推导形如 $A \rightarrow X_1 X_2 \cdots X_k$。
因为 A 可由  n + 1 步到达产生符号，那么 $X_i$ 一定在最多不超过 n - 1 步内也可到达产生符号，则可套用假设，$X_i$ 是产生符号且被我们的步骤发现。
则 A 也会被我们的步骤发现

### 去除不可达符号

从初始变元开始做一个图的遍历即可

## 去除 $\epsilon$ 产生式

**首先我们定义什么是“可空的”**：
1. $A \rightarrow \epsilon$ 则 A 是可空的
2. $A \rightarrow X_1 X_2 \cdots X_k $，若所有的 $X_i$ 都是可空的，则 A 是可空的

去除 $\epsilon$ 产生式的步骤如下：
1. 去除所有形如 $A \rightarrow \epsilon$  的产生式
2. 对所有产生式 $A \rightarrow X_1 X_2 \cdots X_k $，假设其中有 m 个 $X_i$ 是可空的，则根据产生式中出现或者不出现这些 $X_i$，将该产生式改写为 $2^m$ 个新的产生式
特殊的，如果 $m == k$ 则有 $2^m - 1$ 个新的产生式，因为每个 $X_i$ 都不出现的产生式 $A \rightarrow \epsilon$ 是不允许的

$$
\begin{aligned}
S &\rightarrow AB \\
A &\rightarrow aAA | \epsilon \\
B &\rightarrow bBB | \epsilon \\
\end{aligned}
$$

因为 $A$ 与 $B$ 是可空的，所以对 $S$ 应用步骤2：$S \rightarrow A | B | AB$
同样的对 $A$ 有：$A \rightarrow a | aA | aA | aAA = a | aA | aAA$
同样的对 $B$ 有：$B \rightarrow b | bB | bB | bBB = b | bB | bBB$

## 去除单位产生式 

最基本的方法是对每个单位产生式 $A \rightarrow B$ 用 B 的产生式代替 $A \rightarrow B$ 中的 B，直到 A 的这个产生式不再是单位产生式。
    例如 $B \rightarrow ab$，则 $A \rightarrow B$ 改写为 $A \rightarrow ab$

但这个方法无法处理环的情况，例如 $A \rightarrow B \rightarrow C \rightarrow A$

解决方案是找到语法中所有的序对 (A, B) 满足只用一系列单位产生式可以有 $A \Rightarrow^* B$，即有 $A \Rightarrow B_1 \Rightarrow B_2 \Rightarrow \cdots \Rightarrow B$
   1. (A, A) 是这样的序对，由 0 步推导到自身
   2. 若有 (A, B) & (B, C) 则有 (A, C)

所有这样的序对 (A, B)，对 B 的每一个非单位的产生式 $B \rightarrow \alpha$，为 A 改写出产生式 $A \rightarrow \alpha$

根据序对的传递性，我们也做到了为序列中的每一个变元改写出 $B_i \rightarrow \alpha$

用为下面这个语法消除单位产生式为例讲解具体的步骤：

$$
\begin{aligned}
I &\rightarrow a | b | IA | Ib | I0 | I1    \\
F &\rightarrow I | (E)  \\
T &\rightarrow F | T * F    \\
E &\rightarrow T | E + T    \\
\end{aligned}
$$

可以写出每个变元的直接的非单位产生式

$$
\begin{aligned}
I &\rightarrow a | b | IA | Ib | I0 | I1    \\
F &\rightarrow (E)  \\
T &\rightarrow T * F    \\
E &\rightarrow E + T    \\
\end{aligned}
$$

我们可以找到序对

```
(E, E)  (F, F)  (T, T)  (I, I)
(E, T)  (F, I)  (T, F)
(E, F)          (T, I)
(E, I)
```

直接将两个结果组合起来就有目标语法

$$
\begin{aligned}
E &\rightarrow E + T | T * F | (E) | a | b | IA | Ib | I0 | I1  \\
F &\rightarrow (E) | a | b | IA | Ib | I0 | I1  \\
T &\rightarrow T * F | (E) | a | b | IA | Ib | I0 | I1  \\
I &\rightarrow a | b | IA | Ib | I0 | I1    \\
\end{aligned}
$$

改写的时候发现，我们先是通过 (E, F) 为 E 写出了某个产生式，在后续依据序对 (F, *) 写出了新的非单位产生式。
    但是这是完全没有问题的，为 F 新写出的产生式也是由原语法中本就存在的非单位产生式而来的，根据序对定义的传递性，(E, F) 不会漏掉产生式