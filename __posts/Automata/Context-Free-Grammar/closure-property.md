# Closure Property of CFG

## 上下文无关语言在代换下是封闭的

代换的定义为：
    $\Sigma$ 中的一个字符 $a$ 在 $s$ 的做用下为 $\Gamma$ 上的一个语言 $L(a)$，即 $s(a) = L(a)$
    扩展 $s$ 的定义到字符串：
        - $s(\epsilon) = \epsilon$
        - $s(xa) = s(x)s(a)$
    扩展 $s$ 的定义到语言：$s(L)=\bigcup_{x \in L} s(x)$

如果有 $\Sigma$ 上的 CFL $L$ 和代换 $s$，且每个 $a \in \Sigma$ 的 $s(a)$ 都是 CFL，那么 $s(L)$ 也是 CFL.

通过构造一个 CFG 的方式来证明这个命题，
    - 用 $G = (V, T, P, S)$ 表示 L，
    - 用 $G' = (V', T', P', S)$ 来表示构造的 CFG，
    - 用 $G_{a_i} = (V_{a_i}, T_{a_i}, P_{a_i}, S_{a_i})$ 表示每一个字符变换得到的语言

构造的思想是，$s(L)$ 是由 $L$ 的终结符代换得到的语言中的串拼接而来，则将 $G$ 生成式中的每一个终结符 $a_i$ 改为 $S_{a_i}$。则有
$$
\begin{aligned}
V' &= V \cup (\bigcup_{a_i \in T} V_{a_i}) \\
T' &= \bigcup_{a_i \in T} T_{a_i} \\
P' &= \{ 将 P 的产生式中每个终结符 a_i 替换为 S_{a_i} \} \cup \bigcup_{a_i \in T} P_{a_i} 
\end{aligned}
$$

现在需要证明构造出的 $G'$ 的确是 $s(L)$，即证
    - $\forall w \in s(L) \Rightarrow w \in L(G')$
    - $\forall w \in L(G') \Rightarrow w \in s(L)$

证明第一部分 $\forall w \in s(L) \Rightarrow w \in L(G')$：
证明 $w \in L(G')$ 即证 $S \Rightarrow^* w$。
因为 w 要么是一个字符直接变换得到的语言中的一个串，要么是多个字符变换得到的语言中的串连接而成的，则 w 可写作 $w = w_1 w_2 \cdots w_k$ 且有对应的 
$$
S \Rightarrow^* x = x_1 x_2 \cdots x_k \in L \tag{1}
$$
其中 $w_i \in s(x_i)$ 则有 $S_{x_i} \Rightarrow^* w_i$，则有 
$$
S_{x_1} S_{x_2} \cdots S_{x_k} \Rightarrow^* w_1 w_2 \cdots w_k \tag{2}
$$
则由 $(1), (2)$ 以及 $x_i \rightarrow S_{x_i}$
$$
\begin{aligned}
S &\Rightarrow^* x_1 x_2 \cdots x_k \\
  &\rightarrow S_{x_1} S_{x_2} \cdots S_{x_k} \\
  &\Rightarrow^* w_1 w_2 \cdots w_k = w
\end{aligned}
$$

则有 $\forall w \in s(L) \Rightarrow w \in L(G')$

证明第二部分 $\forall w \in L(G') \Rightarrow w \in s(L)$：
