 **乔姆斯基范式**：$A \Rightarrow BC$ 和 $A \Rightarrow a$ 两种形式构成
化为乔姆斯基范式：
    1. 去除无用符号，去除空产生式，去除单位产生式
    2. 把所有长度大于一的生成式全部改成变元，比如 $A \Rightarrow abc$ 改成 $A-XYZ$ 
    3. 通过二分的方法把所有长度大于 2 的产生式改成 2 的，比如 $A \Rightarrow WXYZ$ 改成 $A \Rightarrow MN$ 其中 $M \Rightarrow WX$ 

乔姆斯基的一大作用是将分析树变成二叉树
    - 如果分析树的高度为 $n$ 那么产出的最大也就是前 $n - 1$ 层是满二叉树的情况，产出长度为 $2^{n-1}$

**格雷巴赫范式**：$A \Rightarrow b$ 和 $A \Rightarrow b C_1 C_2 \cdots C_n$ 两种形式构成
化为格雷巴赫范式
    1. 消除空产生式以及单位产生式后即可用（为了使用消除左递归的算法
        2. 给每个变元重命名为 $A_1 A_2 \cdots A_n$
        3. 消除左递归的算法。消除后的每个产生式 $A_i \Rightarrow A_j \alpha$ 都是满足 $i <= j$ 的。
        4. 消除后每个 $A_i$ 的产生式必定形如 $A_i \Rightarrow A_j w$ 和 $A_i \Rightarrow aw$ 二者之一，且 $A_n$ 一定形如 $A_n \Rightarrow aw$（消除直接递归产生的新变元不会出现在开头
        (假设 $w \in V^*$，若 $w \in (V \cup T)^*$ 则为 $w$ 中每个终结符新建一个变元
        后者本来就是符合条件的，现处理前者
           - `for i in 1..=n` 依次将 $A_i$ 代入 $A_{i-1}$，则可每个 $A$ 的产生式一定形如 $A_i \Rightarrow aw$
           - 为消除过程中产生的新变元代入 $A_i$
e.g. 给定上下文无关文法 $G$，将其化为 GNF

$$
\begin{aligned}
    A_1 &\Rightarrow A_2 A_2 | 0    \\
    A_2 &\Rightarrow A_1 A_2 | 1
\end{aligned}
$$

消除左递归后得到：

$$
\begin{aligned}
    A_1 &\Rightarrow A_2 A_2 | 0    \\
    A_2 &\Rightarrow 0 A_2 | 1 | 0 A_2 B | 1B   \\
    B &\Rightarrow A_2 A_2 | A_2 A_2 B
\end{aligned}
$$

为 $A_i$ 们带入得到：

$$
\begin{aligned}
    A_1 &\Rightarrow 0 A_2 A_2 | 1 A_2 | 0 A_2 B A_2 | 1 B A_2 | 0  \\
    A_2 &\Rightarrow 0 A_2 | 1 | 0 A_2 B | 1B   \\
    B &\Rightarrow A_2 A_2 | A_2 A_2 B
\end{aligned}
$$

为 $B$ 带入得到

$$
\begin{aligned}
    A_1 &\Rightarrow 0 A_2 A_2 | 1 A_2 | 0 A_2 B A_2 | 1 B A_2 | 0  \\
    A_2 &\Rightarrow 0 A_2 | 1 | 0 A_2 B | 1 B   \\
    B &\Rightarrow 0 A_2 A_2 | 1 A_2 | 0 A_2 B A_2 | 1 B A_2 | 0 A_2 A_2 B | 1 A_2 B | 0 A_2 B A_2 B | 1B A_2 B 
\end{aligned}
$$