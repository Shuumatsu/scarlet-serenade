# Pumping Lemma of CFG

关于上下文无关文法 $G$，存在某个 $n$ 使得所有长度不小于 $n$ 的串 $s$ 可以写作 $w = uvwxy$，其中
1. $\forall i \geq 0, uv^iwx^iy \in G$
2. $|vwx| \leq n$
3. $vx \neq \epsilon$

Proof: 
对于该文法一定有对应的乔姆斯基范式形式，用 $m$ 表示其中的变元数量。对于所有长度 $\geq 2^m = n$ 的串 $s$，其分析树中的最长路径的长度一定不小于 $m + 1$。

长度不小于 $m + 1$ 的路径上则至少有 $m + 1$ 个变元，然而总共只有 $m$ 种变元，则至少有某个变元出现了两次，不妨用 A 表示这个变元。
我们从下往上找到这个变元出现的位置，分别称作 $T_2$ 和 $T_1$。

![image-20200611005650020](D:\learning-notes\Automata\Context-Free-Grammar\pumping-lemma.assets\image-20200611005650020.png)

Rule 1.
   - $T_2$ 是 $T_1$ 的一个子节点，则一定有生成关系 $A \Rightarrow^* vAx$
   - $T_2$ 的产出称作 $w$，则一定有生成关系 $A \Rightarrow^* w$

则一定有生成关系 $\forall i \geq 0, A \Rightarrow^* v^iAx^i$，则有 $\forall i \geq 0, uv^iwx^iy \in G$ 

Rule 2.
    因为我们是从下朝上找到的 $T_2$ 和 $T_1$， 则以 $T_1$ 为根的子树的最长路径一定不超过 $m + 1$，则其产出 $|vwx| \leq 2^{m + 1 - 1} = n$

Rule 3.
    $T_2$ 一定是在 $T_1$ 的左右子树中的一个，根据乔姆斯基范式的定义，另一个子树的产出一定不为空，则有 $vx \neq \epsilon$