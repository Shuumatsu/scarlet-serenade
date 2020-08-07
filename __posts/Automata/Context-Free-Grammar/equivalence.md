## CFG 与 PDA 是等价的

需要证明构造出的 PDA 与目标 CFG 是等价的：
    （虽然这非常直观，使用这个定理的时候也不需要知道原理，但是我觉得这种利用最左推导和推导步数证明的方法应该是很重要的

#### 所有 PDA 接受的串都是在 CFG 中的

即证若有 $(q, w, S) \vdash^* (q, \epsilon, \epsilon)$ 则有 $S \Rightarrow^* w$
    （这里 S 可以是任意变元，这其实是一个更强的证明，不局限于这个 CFG，而是立足于每个变元。若对 CFG 的初始变元成立则说明 w 在这个 CFG 中

对推导步数用数学归纳法:

若这是一步推导得到的，那么这是显然的。唯一的可能性是 S 有到空的转移且 w 是空串。

假设对 $n - 1$ 步推导得到的串成立，考虑由 $n >= 2$ 步推导得到的串 w.
    - 第一步显然是展开 $S \Rightarrow Y_1 Y_2 \cdots Y_k$。将 x 分解为 $x_1 x_2 \cdots x_k$，栈内依次消耗 $Y_i$ 的时候消耗对应的 $x_i$lp;
    - 很明显消耗每个y的时候用到的步数都不会超过 n - 1 步，则可套用假设有
    $S \Rightarrow Y_1 Y_2 \cdots Y_k \Rightarrow x_1 Y_2 \cdots Y_k \Rightarrow x_1 x_2 \cdots Y_k \Rightarrow \cdots \Rightarrow x_1 x_2 \cdots x_k$

#### 所有 CFG 中的串都会被该 PDA 接受 

（我觉得有点跳，可能需要一个更好的证明，感觉微妙就算了吧，记住用最左推导证明的技巧即可

令 w 为 CFG 中的任意一个串，则 w 有最左推导:
    $S = \gamma_1 \Rightarrow \gamma_2 \Rightarrow \cdots \Rightarrow \gamma_k = w$
$\gamma_i$ 必有形式 $x_i \alpha_i$，其中 
    - $x_i$ 为仅由终结符构成的串
    - $\alpha_i$ 为首位为非终结符的，由终结符与非终结符构成的串
据此可以定义 $y_i$ 为输入中去掉剩下的部分，即 $w = x_i y_i $

特殊的，$S$ 可以看作 $\gamma_0$
    - $\gamma_n$ 中 $\alpha_n = \epsilon$, $x_n = w$，并有 $y_n = \epsilon$
    - $\gamma_0$ 中 $\alpha_0 = S$, $x_n = \epsilon$，并有 $y_n = w$

若我们能证明对任意 i 有 $(q, w, S) \vdash^* (q, y_{i}, \alpha_{i})$，则 i 取 k 有 $(q, w, S) \vdash^* (q, \epsilon, \epsilon)$，即可得证
    - 对于当前的推导是第几步用数学归纳法，当第 0 步时这是显然的
    - 假设对 i 成立，则有 $(q, w, S) \vdash^* (q, y_{i}, \alpha_{i})$，现需要证明 $(q, w, s) \vdash^* (q, y_{i + 1}, \alpha_{i + 1})$
    根据我们构造 PDA 的步骤可以知道，
        - 根据规则一，有空转移处理 $\alpha_i$ 开头的变元 A，展开后栈顶变成可能为空的终结符串（采用的 A 的生成式中可能还有变元，被划入 $\alpha_{i+1}$ 中
        - 根据规则二，我们能够用输入串来和栈顶的终结符串进行匹配，(因为是从最左推导的序列中取出来的 $\alpha_i$，则一定能够匹配)

因此最后的结局是我们到达了 $(q, y_{i + 1}, \alpha_{i + 1})$
