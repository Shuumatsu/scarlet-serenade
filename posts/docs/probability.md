# probability 

排列数公式 $A = \frac{n !}{(n-k) !}$

组合数公式，因为 k 哥元素有 $k!$ 种排列，所以 

$$ 
\left(\begin{array}{l}n \\ k\end{array}\right) = \frac{A}{k!} = \frac{n !}{k !(n-k) !}
$$

也被称为二项系数，

$$
(x+y)^{n}=\sum_{k=0}^{n}\left(\begin{array}{l}n \\ k\end{array}\right) x^{k} y^{n-k}
$$

，当 $x = y = 1$ 时有 

$$
2^{n}=\sum_{k=0}^{n}\left(\begin{array}{l}n \\ k\end{array}\right)
$$

二项式的界，有下界 

$$
\left(\begin{array}{l}
n \\
k
\end{array}\right)=\frac{n(n-1) \cdots(n-k+1)}{k(k-1) \cdots 1}=\left(\frac{n}{k}\right)\left(\frac{n-1}{k-1}\right) \cdots\left(\frac{n-k+1}{1}\right) \geqslant\left(\frac{n}{k}\right)^{k}
$$

由斯特灵公式


可得上界

$$\left(\begin{array}{l}
n \\
k
\end{array}\right)=\frac{n(n-1) \cdots(n-k+1)}{k(k-1) \cdots 1} \leqslant \frac{n^{k}}{k !} \leqslant\left(\frac{e n}{k}\right)^{k}$$



