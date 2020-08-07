**从 CFG 构造 PDA**

$$ M = \left(\{q_0\}, \Sigma, \Sigma \cup \Gamma, \delta, q_0, S \right) $$

这是一个空栈接受的 PDA。其中 $\Sigma$ 为目标 CFG 的终结符，$\Gamma$ 为非终结符。

$\delta$ 为转移函数，对任何状态（其实本来就只有一个状态），
    - Rule 1: 若栈顶元素为非终结符，则有一个空的转移：$\delta(q_0, \epsilon, T) = \{ (q_0, ABC), (q_0, CDE) \}$
        - Rule 2: 若栈顶为终结符（设为 a），则有转移 $ \delta (q_0, aw, a \alpha) = (q_0, w, \alpha )$

**从 PDA 构造 CFG**

要求是以空栈状态接受的 PDA $P = (Q = \{ q_0, q_1, \cdots, q_n \}, \Sigma, \Gamma, q_0, Z_0)$，所构造的 CFG 表示为 $G = (V, T, P, S)$

CFG 的变元为三元组 (p, X, q)，其意义为从 $p$ 到 $q$ 净效应为消耗一个栈内元素 X 的串。
If there is a move $(p_1, A_1 A_2 \cdots A_n) \in \delta(q, x, A)$ with $x = \epsilon$ or $x \in \Sigma$, this adds productions:
$$
(q, A, p_{n+1}) \Rightarrow x (p_1, A_1, p_2) (p_2, A_2, p_3) \cdots (p_n, A_n, p_{n+1})
$$
We consume x from the input (generate it in the grammar), going to state $p_1$, from which we now are committed to get rid of $A_1 \dots A_n$. We don't know what states $p_2 \dots p_{n+1}$ are involved, we just know that after consuming e.g. $A_i$ we will be in some state $q_i$ from which we have to consume $A_{i+1 }$. So we will have to consider all possible collections of states for them.

则 $N(P)$ 接受的串为，所有的 $q \in Q$，$(q_0, Z_0, q)$ 能产出的串。则有产生式
$$
\begin{aligned}
    S \Rightarrow q_0, Z_0, q_0 \\
    S \Rightarrow q_0, Z_0, q_1 \\
    \cdots \\
    S \Rightarrow q_0, Z_0, q_n \\
\end{aligned}
$$

$P$ 中每一个转移 $(p_1, X_1 X_2 \cdots X_n) \in \delta(q, a, X)$ 需要净效应为将 $X$ 完全消除，则首先需要 输入 $a$ 转移到状态 $p_1$，然后需要某些输入从 $p_1$ 到某个状态，净效应为消除 $X_1$，依次类推
    $(q, X, p_{n+1}) \Rightarrow a (p_1, X_1, p_2) (p_1, X_1, p_2) \cdots (p_n, X_n, p_{n+1})$ 其中 $p2$ 到 $p_{n+1}$ 都是未知的，我们需要尝试每一种状态



e.g. 根据 PDA $P = (\{q_0, q_1\}, \{0, 1\}, \{X, Z_0\}, \delta, q_0, Z_0)$ 构造 CFG：
$$
\begin{aligned}
&<q_0, 0, Z_0, q_0, XZ_0> \\
&<q_1, 1, X, q_1, \epsilon> \\
&<q_0, 0, X, q_0, XX> \\
&<q_1, \epsilon, X, q_1, \epsilon> \\
&<q_0, 1, X, q_1, \epsilon> \\
&<q_1, \epsilon, Z_0, q_1, \epsilon> \\
\end{aligned}
$$

首先关于 $S$ 的产生式：
$$
\begin{aligned}
&S \Rightarrow (q_0, Z_0, q_0) \\
&S \Rightarrow (q_0, Z_0, q_1)
\end{aligned}
$$

将所有形如 $(q, Z, *)$ 的三元组一起考虑，因为可使用的相关产生式仅由状态和栈顶元素确定

$(q_0, Z_0, *)$：$<q_0, 0, Z_0, q_0, XZ_0>$ 对应产生式 $(q_0, Z_0, p_3) \Rightarrow a (q_0, X, p_2) (p_2, Z_0, p_3)$ 其中 $p_2 \in \{q_0, q_1\}$ 
    - $(q_0, Z_0, q_0)$：$p_3 = q_0$
$$
\begin{aligned}
&(q_0, Z_0, q_0) \Rightarrow 0 (q_0, X, q_0) (q_0, Z_0, q_0) \\
&(q_0, Z_0, q_0) \Rightarrow 0 (q_0, X, q_1) (q_1, Z_0, q_0) \\
\end{aligned}
$$
​    - $(q_0, Z_0, q_1)$： $p_3 = q_1$
$$
\begin{aligned}
&(q_0, Z_0, q_1) \Rightarrow 0 (q_0, X, q_0) (q_0, Z_0, q_1) \\
&(q_0, Z_0, q_1) \Rightarrow 0 (q_0, X, q_1) (q_1, Z_0, q_1) \\
\end{aligned}
$$

$(q_0, X, *)$：$<q_0, 0, X, q_0, XX>$ 对应产生式 $(q_0, X, p3) \Rightarrow 0 (q_0, X, p_2) (p_2, X, p_3)$ 其中 $p_2 \in \{q_0, q_1\}$ 

    - $(q_0, X, q_0)$：$p_3 = q_0$

$$
\begin{aligned}
&(q_0, X, q_0) \Rightarrow 0 (q_0, X, q_0) (q_0, X, q_0) \\
&(q_0, X, q_0) \Rightarrow 0 (q_0, X, q_1) (q_1, X, q_0) \\
\end{aligned}
$$
​    - $(q_0, X, q_1)$： $p_3 = q_1$
$$
\begin{aligned}
&(q_0, X, q_1) \Rightarrow 0 (q_0, X, q_0) (q_0, X, q_1) \\
&(q_0, X, q_1) \Rightarrow 0 (q_0, X, q_1) (q_1, X, q_1) \\
\end{aligned}
$$

$(q_0, X, *)$：$<q_0, 1, X, q_1, \epsilon>$ 对应产生式 $(q_0, X, *) \Rightarrow 1$ 
    - $(q_0, X, q_1) \Rightarrow 1$

$(q_1, Z_0, *)$：$<q_1, \epsilon, Z_0, q_1, \epsilon>$ 对应产生式 $(q_0, X, p3) \Rightarrow \epsilon$
    - $(q_1, Z_0, q_1) \Rightarrow \epsilon$

$(q_1, X, *)$：$<q_1, 1, X, q_1, \epsilon>$ 对应产生式 $(q_1, X, p3) \Rightarrow 1$
    - $(q_1, X, q_1) \Rightarrow 1$

$(q_1, X, *)$：$<q_1, \epsilon, X, q_1, \epsilon>$ 对应产生式 $(q_0, X, p3) \Rightarrow \epsilon$
    - $(q_1, X, q_1) \Rightarrow \epsilon$

全部联立起来则有：
$$
\begin{aligned}
S &\Rightarrow (q_0, Z_0, q_0) | (q_0, Z_0, q_1) \\
(q_0, Z_0, q_0) &\Rightarrow 0 (q_0, X, q_0) (q_0, Z_0, q_0) | 0 (q_0, X, q_1) (q_1, Z_0, q_0) \\
(q_0, Z_0, q_1) &\Rightarrow 0 (q_0, X, q_0) (q_0, Z_0, q_1) | 0 (q_0, X, q_1) (q_1, Z_0, q_1) \\
(q_0, X, q_0) &\Rightarrow 0 (q_0, X, q_0) (q_0, X, q_0) | 0 (q_0, X, q_1) (q_1, X, q_0) \\
(q_0, X, q_1) &\Rightarrow 0 (q_0, X, q_0) (q_0, X, q_1) | 0 (q_0, X, q_1) (q_1, X, q_1) \\
(q_0, X, q_1) &\Rightarrow 1 \\
(q_1, Z_0, q_1) &\Rightarrow \epsilon \\
(q_1, X, q_1) &\Rightarrow 1 \\ 
(q_1, X, q_1) &\Rightarrow \epsilon \\
\end{aligned}
$$

变量重命名化简后得到 

$$
\begin{aligned}
    S &\Rightarrow 0B \\
    B &\Rightarrow 0B | 0BD \\
    B &\Rightarrow 1 \\
    D &\Rightarrow 1 \\
\end{aligned}
$$




