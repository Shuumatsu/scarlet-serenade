---
title: Introduction to the Theory of Computation | Space Complexity
---

定义图灵机的空间复杂性

-   DTM 在长度为 $n$ 的输入上最多扫描 $f(n)$ 个不同的方格
-   NTM 在长度为 $n$ 的输入上在任何分支最多扫描 $f(n)$ 个不同的方格
    则 M 的空间复杂度为 $f(n)$

定义图灵机的空间复杂性类

-   SPACE(f(n)) 表示所有能在 $O(f(n))$ 空间上被 DTM 判定的语言
-   NSPACE(f(n)) 表示所有能在 $O(f(n))$ 空间上被 NTM 判定的语言

---

可满足性问题 SAT 属于 SPACE(f(n))： \
对于输入的布尔公式 $\Phi$，我们对其每个变量 $x_1, x_2, \dots, x_m$ 赋值，计算公式在该赋值下的值。 \
存储每种复制需要 $O(m)$ 的空间，每次计算需要 $O(n)$ 的空间，所以总的空间复杂度为 $O(m) + O(n) = O(n)$

---

定义语言 $ALL_{NFA}$ 为所有 {其语言为字母表上全集的 NFA}，我们可以给出一个 $O(n)$ 空间的算法 decide 其补集，核心思想为：

-   用非确定性来猜测被 NFA 拒绝的串
-   用线性空间存储在特定时刻 NFA 可能处于的状态

若一个 NFA 有拒绝的串，则必有长度小于 $2^q = C_q^0 + C_q^1 + C_q^2 + \cdots + C_q^q$ 的串被拒绝，其中 $q$ 为状态数：若被拒绝的串长度大于等于 $2^q$，则根据鸽笼原理，其经过的状态路径必有重复，则我们可以通过删除部分来得到更短的串 `...a[...a]...`

Algorithm: 对于输入 $<M>$，$M$ 为一个 NFA

```
mark start_state of M
for _ in 0..2**q {
    随机选择一个输入（NonDeterministic 体现在这里），并标记下一个状态来模拟读取
    若拒绝状态被标记，则输出 Accept
}
输出 Reject
```

我们的算法中用到的空间只有：状态标记以及 loop counter，所以我们的算法的空间复杂度是线性的。

---

我们定义一个 `can_yield` 过程，`can_yield` 被用来判断 NTM 的一个 Configuration 能否在给定步数内到达另一个 Configuration

```
can_yield c1 c2 t =
    if t = 1
        return (c1 = c2) or (c1 -> c2 in one step)
    for cm in machine
        if (can_yield c1 cm t/2) and (can_yield cm c2 t/2)
            return accept
    return reject
```

对每层递归，我们需要 $f(n)$ 的空间来存储 Configuration；基于二分，我们的递归深度为 $O(log(t))$ 的。所以我们总的空间复杂度为 $O(f(n)log(t))$ 的。

---

萨维奇定理说，用确定型图灵机模拟非确定图灵机，空间的增长是平方级别的，$\operatorname{NSPACE}(f(n)) \subseteq \operatorname{SPACE}\left(f^{2}(n)\right)$

给定一个 NTM $N$，我们构造一个 DTM $M$，对于输入 $w$，\
:indent[Output the result of $\operatorname{can\_yield} \; c_{\text {start }} \; c_{\text {accept }} \; 2^{d * f(n)}$ ]

我们可以利用 `can_yield` 过程 来证明萨维奇定理。\
这里我们限制的步数为 $2^{d f(n)}$，因为给定一个最大空间占用为 $f(n)$ 的 NTM，其状态数是给定的，则其 Configuration 的数量是 $2^{d f(n)}$ 级别的，则根据鸽笼原理，从初始 Configuration 到接受 Configuration 一定是可以在 $2^{d * f(n)}$ 步内完成的
这样我们就构造出了空间复杂的为 $O(f(n) * log(2^{d f(n)})) = f^{2}(n)$ 的 DTM

我们目前给出的步骤的一个问题是，在算法开始前，我们没有 $2^{d f(n)}$ 的确切值。
