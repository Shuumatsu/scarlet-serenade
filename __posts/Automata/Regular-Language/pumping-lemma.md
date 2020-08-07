L 是一个正则语言，则存在一个与 L 有关的 $n \geq 0$，使得所有的 L 上的长度大于等于 n 的串都能被写成 $w = xyz$，并满足
    - $|y| \geq 1$
    - $|x y| \leq n$
    - $(\forall i \geq 0)\left(x y^{i} z \in L\right)$

利用这个定理证明的时候我们经常取长度为 2n 的串，这样的话 $xy$ 一定是在前半段的