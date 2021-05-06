---
title: Introduction to the Theory of Computation | Decidability
---

首先我们回顾一下定义

A language is Recognizable iff there is a Turing Machine which will halt and accept only the strings in that language and for strings not in the language, the TM either rejects, or does not halt at all. \
Note: there is no requirement that the Turing Machine should halt for strings not in the language.

A language is Decidable iff there is a Turing Machine which will accept strings in the language and reject strings not in the language.

显然，一个他图灵课判定的语言的补也是图灵可判定的

如果一个语言是图灵可识别的语言的补集，则我们称它为补图灵可识别的

---

我们可以有一个图灵机 $U$ 对于输入 $<M, w>$ 在 $M$ 上模拟 $w$，若 M 进入接收状态，则接受，若 M 进入拒绝状态

为了解决无限集之间规模大小的问题，Cantor 引入了一种不基于计数的方法来确定无限集间的相对规模：对于两个无限集合 A 和 B，若存在之间的一个双射 f，则称 A 于 B 有相同的规模

我们定义一个集合是可数的当且仅当其为有限的或其于自然数集等规模

---

例如，有理数集是可数的。
我们可以将所有的分数排列成一个矩阵，所有以 1 为分子的分数在第一行，所有以 2 为分子的分数在第二行，依次类推
```
1/1 1/2 1/3 1/4 ...
2/1 2/2 2/3 2/4 ...
3/1 3/2 3/3 3/4 ...
4/1 4/2 4/3 4/4 ...
... ... ... ...
```
从 1/1 开始沿对角线依次选取得到排列，`1/1, 1/2, 2/1, 1/3, 2/2 ...`

---

例如，实数集是不可数集。

我们用反证法证明，假设有一个自然数集到实数集的双射，我们可以构造出一个 0 到 1 之间的实数 x 并没有被映射到。其第 i 位小数为任选的一个与 f(i) 中的第 i 位不同的数。
可以证明不存在自然数 n 使得 f(n) = x。则产生矛盾，得证。

基于同样的方法我们可以得出结论，所有无限长的二进制序列构成的集合是不可数的。

---

对任意字母表，其上所有串的集合是可数的。因为固定长度的串来说，其数量是有限的，我们可以先协下长度为 1 的串，再写下长度为 2 的串，依次类推从而得到一个排列

---

所有语言构成的集合 L 是不可数的。

我们可以通过给出一个 L 与 B 之间的双射来证明，L 是不可数的。（这里 B 表示所有无限长的二进制序列构成的集合，

用 $\Sigma^* = \{s_1, s_2, \dots \}$ 表示 $\Sigma$ 上的串，每个语言 $A \in L$ 在 $B$ 中有唯一一个对应序列：若 $s_i \in A$ 则其对应序列的第 i 位为 1 否则为 0。这个序列被称为 A 的特征序列

在之前我们已经说过，B 是不可数的，所以 L 也是不可数的。

---


停机问题是不可判定的，即 $A_{tm} = \{ <M, w> | M$ is a turing machine and $w \in L(M) \}$ 是可识别的但不可判定的

我们用反证法证明，假设 $A_{tm}$ 是可判定的，设 $H$ 为其判定器。H 在输入 <M, w> 上，若 M 接受 w 则 H 停机接受，否则 H 就停机拒绝。

基于 $H$ 我们可以构造出一个新的图灵机 $D$，对于输入 $<M>$，用 $H$ 运行该输入并输出与 H 相反的结果。

当运行 `D(<D>) = ~H(<D, <D>>)` 时则会产生矛盾：若 D 接受 `D`，则 `H(<D, <D>>)` 输出 accept，则 D(<D>) 输出 reject

---

除了不可判定的语言，还有不可识别的语言。
一个语言是可判定的当且仅当他既是图灵可识别的也是补图灵可识别的。
正向的证明是显然的，我们考虑逆向
$A$ 与 $~A$ 都是图灵可识别的，令 $M_1$ 为 $A$ 的识别器，$M_2$ 为 $~A$ 的识别器。我们可用 $M_1$ 与 $M_2$ 构造 $A$ 的判定器 $M$，对于输入 $w$：
$M$ 在输入 $w$ 上平行的运行 $M_1$ 与 $M_2$，(平行指的是，M有两个纸带，一个模拟 M_1 一个模拟 M_2 直到其中一个停机)
若 $M_1$ 输出接受，则 $M$ 输出接受，$M_2$ 输出接受，则 $M$ 输出拒绝。$w$ 比属于 $A$ 或 $~A$ 中的一个，则 $A$ 与 $~A$ 中必有一个输出接受，则 $M$ 必停机