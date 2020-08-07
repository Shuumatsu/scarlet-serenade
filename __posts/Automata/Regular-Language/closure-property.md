**正则语言关于并是封闭的**：根据语法，中间加一个加号就行

**正则语言关于补是封闭的**：根据原自动机 DFA $M = (Q, \Sigma, \sigma, s_0, F)$ 构造一个新的 DFA $N = (Q, \Sigma, \sigma, s_0, Q - F)$
    (需要注意的点是 M 需要对每个输入都有转移，也就是说会走死的状态不能省略，需要显式的表示出来。

**正则语言关于交是封闭的**：由得摩根率可以写成补与并的形式

**正则语言关于差是封闭的**：由得摩根率可以写成补与并的形式

**正则语言关于反转是封闭的**：字符串 $w = a_1 a_2 \cdots a_n $ 的反转，记为 $w^R = a_n \cdots a_2 a_1$.语言 $L$ 的反转记为 $L^R = \{ w^R | w \in L \}$

可利用 DFA 证明
    1. 将初始状态改为唯一的接受状态
    2. 新增一个初始状态，以及从初始状态到原所有接收状态的 $\epsilon$ 转移
    3. 将所有的边反向
构造出的 DFA 即是一个接受原正则语言的反转的 DFA

有正则表达式的相关结果

$$
\begin{aligned}
&\epsilon^R = \epsilon \\
&\phi^R = \phi \\
&a^R = a \\
&(E_1E_2)^R = E_2^R E_1^R \\
&(E_1 + E_2)^R = E_1^R + E_2^R \\
&(E^*)^R = (E^R)^*
\end{aligned}
$$

以 $(E^*)^R = (E^R)^*$ 为例，给出一个证明，主要是为了说明正则表达式相关的证明可能要从对应的语言来考虑

$$
\begin{aligned}
L(E^*)^R &= \{w_1 w_2 \cdots w_n | n \geq 0, w_i \in L(E) \}^R \\
         &= \{(w_1 w_2 \cdots w_n)^R | n \geq 0, w_i \in L(E) \} \\
         &= \{w_n^R \cdots w_2^R w_1^R | n \geq 0, w_i \in L(E) \} \\
         &= \{w_n \cdots w_2 w_1 | n \geq 0, w_i \in L(E^R) \} \\
         &= \{w_1 w_2 \cdots w_n | n \geq 0, w_i \in L(E^R) \} \text{ \\\\ rename variables} \\
         &= L(E^R)
\end{aligned}
$$

### 正则语言的同态也是正则的

若 $\Sigma$ 和 $\Gamma$ 是两个字母表，同态定义为函数 $h:\Sigma \rightarrow \Gamma^*$ 
$$
\forall a \in \Sigma. h(a) \in \Gamma^*
$$

扩展 $h$ 的定义到字符串，
$$
\begin{aligned}
&(1). h(\epsilon) = \epsilon \\
&(2). h(xa) = h(x)h(a) \\
\end{aligned}
$$

扩展 $h$ 的定义到语言 $\forall L \subseteq \Sigma^*, h(L) = \{h(w) | w \in L \}$

---

正则语言的同态封闭性是指，若 $L$ 是字母表 $\Sigma$ 上的正则语言, $h$ 是 $\Sigma$ 上的同态,则 $h(L)$ 也是正则的.

根据同态定义的作用方法，有以下保证（这个要证的话好像可以用结构归纳法证出来？算了不证了）：

$$
\begin{aligned}
& h(r+s) = h(r) + h(s) \\
& h(rs) = h(r) h(s) \\
& h(r^*) = h(r)^* \\
\end{aligned}
$$

设 $L$ 的正则表达式为 $E$，即 $L = L(E)$.
因为 $h(E)$ 显然是正则表达式，若能证明 $h(L(E)) = L(h(E))$，则原命题得证

采用结构归纳法，
基础情况: $E = \epsilon$, $E = \phi$ 以及 $\forall a \in \Sigma, E = a$ 时是显然的
归纳递推：假设对正则表达式 F, G 有 
$$
L(h(F)) = h(L(F)), L(h(G)) = h(L(G))
$$

当 $E = F + G$ 时

$$
\begin{aligned}
h(L(E)) = h(L(F + G)) &= h(L(F) \cup L(G)) \text{ \\\\根据正则表达式 + 的定义} \\
                      &= h(L(F)) \cup h(L(G)) \text{ \\\\将 h 做用到每个集合上} \\
                      &= L(h(F)) \cup L(h(G)) \text{ \\\\根据归纳假设}  \\
                      &= L(h(F) + h(G)) \text{ \\\\根据正则表达式 + 的定义}  \\
                      &= L(h(F + G)) \text{ \\\\根据 h 的构造规则} \\
                      &= L(h(E))  \\
\end{aligned}
$$

### 正则语言的逆同态是正则的

若 $h$ 是字母表 $\Sigma$ 到 $\Gamma$ 的同态, 且 $L$ 是 $\Gamma$ 上的语言, 那么使 $h(w) \in L$ 的 $w (w \in \Sigma^*)$ 的集合, 称为语言 $L$ 的 $h$ 逆, 记为 $h^{−1}(L)$, 即

$$
h^{-1}(L)=\left\{w \in \Sigma^{*} | h(w) \in L\right\}
$$

---

如果 $h$ 是字母表 $\Sigma$ 到 $\Gamma$ 的同态, $L$ 是 $\Gamma$ 上的正则语言, 那么 $h^{−1}(L)$ 也是正则语言.

由 $L$ 的 DFA $A=\left(Q, \Gamma, \delta, q_{0}, F\right)$ 构造识别 $h^{−1}(L)$ 的 DFA
$$
B=\left(Q, \Sigma, \delta', q_{0}, F\right)
$$
其中 $\delta'(q, a)=\delta(q, h(a))$

然后需要证明 $L(B) = h^{-1}(L)$，即$\forall w \in \Sigma^{*}$ 被 B 接受当且仅当 $h(w)$ 被 A 接受，
    即 $\delta(q, h(w)) \in F$ 当且仅当 $\delta'(q, w) \in F$。

1. 从右到左是显然的，$\forall w \in \Sigma^{*}$ 被 B 接受显然有 $h(w)$ 被 A 接受。

2. 用结构归纳法证明从左到右的情况：

    归纳基础：若 $w = \epsilon$ 时或 $w$ 长度为 1 时，是显然的
    归纳递推：假设命题对 $s$ 成立，设 $w = sa$，则 $h(w) = h(s)h(a)$

    $$
    \begin{aligned}
    \delta(q, h(w)) &= \delta(q, h(s)h(a)) \\
                    &= \delta(\delta(q, h(s)), h(a)) \\
                    &= \delta(\delta'(q, s), h(a)) \\
                    &= \delta'(\delta'(q, s), a) \\
                    &= \delta'(q, sa)  \\
    \end{aligned}
    $$

    则 $\delta(q, h(w)) \in F$ 一定有 $\delta'(q, w) \in F$

---

例题：若语言 $L=( 0 0 + 1 )^{*}$，同态 $h:\{a, b\} \rightarrow\{0,1\}^{*}$ 为 
$$
h(a)=01, \quad h(b)=10$
$$
证明 $h^{-1}(L)=( b a )^{*}$

An：即证明 $h^{-1}(L) \subset ( b a )^*$，以及 $( b a )^* \subset h^{-1}(L)$

1. $\forall w \in (ba)^*, h(w) = (1001)^*$ 显然是属于 L 的，所以有 $( b a )^* \subset h^{-1}(L)$

2. 假设有 $h^{-1}(L)$ 中的串 $w$ 不属于 $(ba)^*$，则只能有以下四种形式：
   1. 是以 a 开头的串，则 h(w) 是以 01 开头的串，不属于 L 这与 $w \in h^{-1}(L)$ 矛盾
   2. 是以 b 结尾的串，则 h(w) 是以 10 结尾的串，不属于 L 这与 $w \in h^{-1}(L)$ 矛盾
   3. 是有连续 a 的串，则 h(w) 是有 0101 的串，不属于 L 这与 $w \in h^{-1}(L)$ 矛盾
   4. 是有连续 b 的串，则 h(w) 是有 1010 的串，不属于 L 这与 $w \in h^{-1}(L)$ 矛盾

所以有 $h^{-1}(L) \subset ( b a )^*$

