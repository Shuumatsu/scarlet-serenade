 自顶向下的分析无法处理的情况：
    1. 同一变元的多个产生式有着相同的开始符号($T \cup V$)，会产生回溯影响性能
    2. 左递归，会产生无限循环

**消除间接左递归**
$$\begin{aligned}
A &\Rightarrow T\alpha \\ 
T &\Rightarrow A\beta \\
\end{aligned}$$
通过替换的方式将间接左递归重写为直接左递归
$$
\begin{aligned}
&A \Rightarrow A \beta \alpha \\
&T \Rightarrow T \alpha \beta
\end{aligned}
$$

**消除直接左递归** $A \Rightarrow A\alpha | \beta$
通过将直接左递归重写为右递归
$$
\begin{aligned}
\Rightarrow &A \Rightarrow \beta \alpha^* \\
\Rightarrow &A \Rightarrow \beta A'* \\
&A' \Rightarrow \alpha A' | \epsilon
\end{aligned}
$$

**消除所有左递归的算法**
```rust
// 给每个变元编上有序的序号
for i in 0..n {
    for j in 0..i {
        // 变元的生成式的首符号不允许是序号比其低的变元
        for (vi -> vj alpha) in production_rules[vi] {
            substitute_with_production_rules(vj)
        }
        // 
    }
    eliminate_direct_left_recursion(vi)
    // 为每个变元消除了直接左递归
}
```
因为每层循环仅影响当前序号的变元的产生式，所以可以保证算法完成后，每个变元的生成式的首符号不可能是序号比其低的变元，因而不可能有间接左递归。

---

**Def.** $Follow(A) = \{a | S \Rightarrow^* \alpha A a \beta \}$ 
    代表在某个推导下可紧接着出现在 A 后的终结符的集合。

**Def.** 简单确定性文法：同一个变元的每一个产生式都以不同的终结符开头。
    要求每个产生式有着不同的 $Select$（则可以无回溯的实现自顶向下的分析
**Def.** 这种文法下的 $Select(A \Rightarrow \alpha)$ 定义为
    - 对于以 $\epsilon$ 开头的：$A \Rightarrow \epsilon$，
        -  $Select(A \Rightarrow \alpha) = Follow(A)$
    - 对于以终结符开头的：$A \Rightarrow a\beta$，
        -  $Select(A \Rightarrow \alpha) = \{a\}$

**Def.** $First(A\Rightarrow\alpha) = \{a | \alpha \Rightarrow^* a\beta \}$，代表所有在某个推导下可能出现在开头的终结符的集合，如果 $\alpha$ 可以导出 $\epsilon$ 的话，则集合中也包含 $\epsilon$
    1. 若 $\alpha = a\beta$，则 $First(A\Rightarrow\alpha) = {a}$
    2. 若 $\alpha = V\beta$，则 $First(A\Rightarrow\alpha) = First(V)$
        - 若 $\epsilon \in First(V), First(A\Rightarrow\alpha) = First(A\Rightarrow\alpha) \cup First(V)$
($First(V) = \cup_{V \Rightarrow \beta} First(V \Rightarrow \beta)$

**Def.** LL(1) 文法，在简单确定性文法的基础上，允许产生式以非终结符开头
    要求每个产生式有着不同的 $Select$（则可以无回溯的实现自顶向下的分析
**Def.** 这种文法下的 $Select(A \Rightarrow \alpha)$ 定义为
   - 若 $\epsilon \notin First(\alpha)$，则 $Select(A \Rightarrow \alpha) = First(\alpha)$
   - 若 $\epsilon \in First(\alpha)$，则 $Select(A \Rightarrow \alpha) = (First(\alpha) - \{\epsilon\}) \cup Follow(A)$
        （因为 $\epsilon$ 是不会作为输入出现的，则 $Select(A \Rightarrow \alpha)$ 不会包含 $\epsilon$，所以需要减掉

这个条件隐含着，任意两个产生式 $A \Rightarrow \alpha$ 以及 $A \Rightarrow \beta$
    1. 不可能同时为可空的，若同时可空，则 $Select(A \Rightarrow \alpha) = Select(A \Rightarrow \beta) = Follow(A)$
        2. 若 $\alpha$ 可空，则有 $First(A \Rightarrow \beta) \cap Follow(A) = \phi$



(我觉得这两个计算 $First$ 和 $Follow$ 的算法就是暴力算法啊。。

计算 $First$：重复以下步骤直到不再有变动
    对每个变元 $A$ 的每个产生式，
        - 如果第一个符号是终结符 $a$，则 $First(A) = First(A) \cup \{a\}$
        - 如果第一个符号是变元 $V_1$，则 $First(A) = First(A) \cup First(V_1)$，如果 $V_1$ 是可空的则考虑第二个符号，以此类推
          - (其实这就是 $First(string)$ 的定义方式

计算 $Follow$：需先计算出每个 $First$，重复以下步骤直到不再有变动
    对每个变元 $A$
        1. 若 $A$ 是终结符，则 $Follow(A) = {A}$
        2. 若 $A$ 是变元，则找到每一个右侧包含 $A$ 的产生式 $V \Rightarrow \alpha A \beta$，则 $Follow(A) = First(\beta)$
           - 一个变元的 $Follow$ 取决于自己的位置，也取决于能产生它的变元的位置。若 $\beta$ 是可空的或者 $\beta = \epsilon$，则 $Follow(A) = Follow(A) \cup Follow(V)$




