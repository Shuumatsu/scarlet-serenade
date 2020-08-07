$$
G = \left(Q, \Sigma, \Gamma, \delta, q_{0}, Z, F \right)
$$

**Instantaneous description**：一个三元组 (state, remaining_input, stack_content)
    习惯上 stack_content = abcd 的形式 左边 a 为栈顶 右边 d 为底

$(q, a, x) \rightarrow^* (p, m, z)$ then $(q, aw, xy) \rightarrow^* (p, mw, zy)$
    - 从转移函数可以看出，下推自动机的每次转移只关注当前的状态，当前的输入，以及栈顶元素。也就是说自动机从来都看不到的数据不会影响我们的转移
    - 反之则不成立，因为自动机可以弹出栈内某些东西再弹回来，看起来是从来没用到这部分，但是可能其中某些步用到了。(当然只是输入部分的话还是可以反过来

**Def. 歧义性**：一个文法歧义的意味着对某串有不同的分析树或不同的最左/右推导

**对于 G 中的串 w，一定有一颗对应的分析树**

**每一个最右推导都有等价的最左推导，反之亦然**

**下推自动机的接收方式**
    - 按指定状态接受
    - 按空栈接受
这两种接收方式是等价的

Def. $N(P) = \{ w | (q_0, w, z_0) \rightarrow^* (p, \epsilon, \epsilon) \}$
    即所有能在消耗完的同时让栈为空的字符串的集合

Def. $L(P) = \{ w | (q_0, w, z_0) - (q, \epsilon, \alpha) \}$ 其中 $q$ 为 $F$ 中的某个状态，$\alpha$ 为任意堆栈符合。
    即能在消耗完的同时进入终结状态的栈的集合

**空栈到终结**：对某个以空栈接受的 PDA 添加一个新的状态指定为接受状态，为每个栈为空的id添加一个到该状态的 $\epsilon$ 转移

**空栈到空栈**：某空栈接受的 PDA 不接受 $\epsilon$，将其改成接受 $L \cup \{ \epsilon \}$ 的。添加一个新的初始状态，有一个到自己的 pop 一个元素的 $\epsilon$ 转移，一个到原初始状态的 $\epsilon$ 转移

**终结到空栈**：给所有终极状态添加到一个新状态的 $\epsilon$ 转移，并且转移的时候 pop 一个栈顶元素，并且这个状态到自己也有 $\epsilon$ 转移
    需要注意原自动机中到达空栈而并不是接受状态的情况。我们给原自动机的堆栈额外加一个底，这个底在原自动机中永远不会被用到，所以不会影响对应的语言。

**前缀性质**：指不存在两个串 x y，x是y的前缀

<!-- ? -->
<!-- 语言 $L$ 是某个 DPDA 的空栈状态接受的语言当且仅当其具有前缀性质，且是某个以状态接受的dpda的语言 -->

DPDA 以终结状态接受的语言真包含正则语言，真包含于 CFL

DPDA 接受的语言都是无歧义的

**受限 PDA**：如果 PDA 一个的任何转移都只能让它的栈高度增长至多一个符号。则被称为受限的。也就是说，对于任何包含 $(p, \gamma)$ 的规则 $\delta(q, a, Z)$ 一定有 $|\gamma| \leqslant 2$。每一个 PDA $P$ 都存在一个受限的 PDA $P_r$ 使得 $L(P) = L(P_r)$ 
	消除每个 $\delta(q, a, Z) = (p, \gamma_1 \gamma_2 \cdots \gamma_k)$：
        1. 添加中间状态 $q_1, q_2 \cdots q_{k - 2}$ 
        2. 添加转移 $\delta(q, a, Z) = (q_1, \gamma_{k-1} \gamma_k)$
        3. 添加转移 $\forall 1 \leq i \lt k - 2, \delta(q_i, \epsilon, \gamma_{k - i}) = (q_{i+1}, \gamma_{k - i - 1} \gamma_{k - i})$
        4. 添加转移 $\delta(q_{k-2}, \epsilon, \gamma_2) = (p, \gamma_1 \gamma_2)$
即通过将中间状态用 $\epsilon$ 转移连接起来 PUSH 额外的高度