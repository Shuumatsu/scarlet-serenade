$C = AB$

If $B$ is seen as a sequence of columns $b_1, b_2, \cdots, b_n$, then $C$ can be seen as a sequence of columns $c_1, c_2, \cdots, c_n$ where $c_i = Ab_i$ only relates to the corresponding column $b_i$ in $B$ 

If $A$ is seen as a sequence of rows $a_1, a_2, \cdots, a_n$, then $C$ can be seen as a sequence of row $c_i, c_2, \cdots, c_n$ where $c_i = a_iB$ only relates to the corresponding row $a_i$ in $A$

---

2 vectors are orthogonal iff $x^T y = y^T x = 0$

&emsp;&emsp;&emsp;&emsp;$x^Tx = 0$ iff $x = 0$

2 subspaces $S$ and $T$ are orthogonal iff all $x \in S$ and $y \in T$ are orthogonal


---

一组正交的非 0 向量 $a_1, a_2, \cdots, a_n$ 一定是线性无关的

&emsp;&emsp;&emsp;&emsp;假设这组向量是线性相关的，则存在一组不全为0的系数系数 $x_i$ 使得 $x_1a_1 + x_2a_2 + \dotsc + x_na_n = 0$。

&emsp;&emsp;&emsp;&emsp;等式两边同时乘以 $x_1^T$ 得到 $x_1^Tx_1a_1 = 0$ 则 $a_1 = 0$，用同样的方法可以得到 $\forall i, a_i = 0$，与题设产生矛盾。

---

NullSpace is perpendicular to RowSpace

&emsp;&emsp;&emsp;&emsp;$Ax = 0$ and $A$ can be seen as a sequence of row vectors. All these row vector dot-products x equal to 0. So All vectors in RowSpace are orthogonal to vectors in NullSpace.

For the same reason, LeftNullSpace is perpendicular to ColSpace

NullSpace and RowSpace are Complementary in $R^n$

&emsp;&emsp;&emsp;&emsp;NullSpace is spanned by $n - rank(A)$ independent vectors

&emsp;&emsp;&emsp;&emsp;RowSpace is spanned by $rank(A)$ independent vectors

&emsp;&emsp;&emsp;&emsp;Since $x^Tx = 0$ iff $x = 0$, we know that, the intersection of two spaces is ${0}$

&emsp;&emsp;&emsp;&emsp;So (NullSpace + RowSpace) is spanned by $n - rank(A) + rank(A)$ independent vectors. Whici means that (NullSpace + RowSpace) = $R^n$

For the same reason, LeftNullSpace and ColSpace are Complementary

So we say 

&emsp;&emsp;&emsp;&emsp;**NullSpace and RowSpace are orthogonal Complementary in $R^n$**

&emsp;&emsp;&emsp;&emsp;**LeftNullSpace and ColSpace are orthogonal Complementary in $R^m$**

---

3 fundamental theorems of algebra

- Dimensions of the 4 subspaces
- Those spaces come in orthogonal pairs
- The orthogonal base for these subspaces`

---

$rank(A^TA) = rank(A)$

Proof: Since KernalSpace and RowSpace are orthogonal complement to each other, if we can prove $A^TA$ and $A$ have equal KernalSpace, then we can prove that $rank(A^TA) = rank(A)$

Part 1. $Ax = 0 \Rightarrow A^TAx = 0$ 

Part 2. $A^Tx = 0 \Rightarrow x^TA^TAx = 0 \Rightarrow (Ax)^TAx = 0$

&emsp;&emsp;&emsp;&emsp;Because $x^Tx = 0$ iff $x = 0$ we know that $Ax = 0$

So from two parts above, $Ker(A^TA) = Ker(A)$ which means $rank(A^TA) = rank(A)$

From above proof we can know that **$A^TA$ is invertible iff $rank(A) = n$ in other words, A has independent columns**

---

**Fredholm’s Alternative?**

---

Projection Matrix: If we are to project 1 vector $b$ to a subspace $S$, we need to find the projected vector $p$. 

$Pb = p$

$S$ is spanned by vectors $\{a_1, a_2, \cdots, a_n\}$, so it can be seen as the ColSpace of a matrix $A$ whose columns are basises of $S$, so $p$ can be written as $x_1a_1 + x_2a_2 + \cdots + x_na_n = Ax$

$b - p$ should be perpendicular to all vectors inside ColSpace(A), which means 

$\forall i, a_i^T \cdot (p - b) = 0$ which can also be written as $A^T(p - b) = A^T(Ax - b) = 0$

By solving this equation we can get $x = (A^TA)^{-1}A^Tb$, then $Pb = Ax = A(A^TA)^{-1}A^Tb \Rightarrow P = A(A^TA)^{-1}A^T$

So $P = A(A^TA)^{-1}A^T$ is the projection matrix that maps a vector onto ColSpace(A)

左零空间和列空间是正交补，所以空间中的每一个向量 $e$ 都可以分解为 
$e = u + v$ 其中 $u$ 是列空间的向量，$v$ 是左零空间的向量

$$
u = Pe \\
u = e - u = e - Pe = (I- P)e
$$

So $I - P$ is the projection matrix that maps a vector onto LeftNull(A)


It’s easy to verify by calculation that, if $P$ is a projection matrix then $P^2 = P$ and $P^T = P$

---

Least Square Method

For an inconsistent equation $Ax = b$，it has no solution, so we may want to find such a solution that makes the distance between $Ax$ and $b$ be minimum. Such $Ax$ must be the projection vector from $b$ to ColSpan(A) because $Ax$ can be any vector inside ColSpan(A).

$A^T(b - AX) = 0 \Rightarrow A^TAx = A^Tb$

So instead of solving $Ax=b$, we just need to solve $A^TAx = A^Tb$ which must have at least one solution.

$A^TAx = A^Tb$ can be seen as a translate of the solution set of the homogeneous equation $A^TAx = 0$. 

Since $A^TA$ is a square matrix, then if $A^TA$ is invertible (which means $A$ has independent columns), the equation has a unique solution.

The equation can have multiple solutions.

Best fit

我们可能拿到一些有误差的数据，使得我们的方程没有解，比如我们拿到三个点 $(0, 6), (1, 0) (2, 0)$ 因为某些测量什么的原因，他们并不在一条直线上，所以我们的方程 $y = Mx + B$ 无解。可以写成矩阵形式:

$$
A=\left(\begin{array}{ll}
0 & 1 \\
1 & 1 \\
2 & 1
\end{array}\right) \quad x=\left(\begin{array}{c}
M \\
B
\end{array}\right) \quad b=\left(\begin{array}{l}
6 \\
0 \\
0
\end{array}\right)
$$

使用最小二乘法可以找到一个 $x$ 使得 $Ax$ 与 $b$ 最近，即 $||Ax - b||^2$ 最小。
Every entries of Ax is one value of the resulting function

$$
A\left(\begin{array}{c}
-3 \\
5
\end{array}\right)=\left(\begin{array}{c}
-3(0)+5 \\
-3(1)+5 \\
-3(2)+5
\end{array}\right)=\left(\begin{array}{c}
f(0) \\
f(1) \\
f(2)
\end{array}\right)
$$

即$||b - Ax||^2 = (6 - y(0))^2 + (0 - y(1))^2 + (0 - y(2))^2$ 最小，即最小二乘法得到的解使得在 y 轴上距离平方和最小



通过上面的例子我们可以看出来，Ax的每一项其实都和我们求得的系数后的样子无关，
也就是说 对于 $y=B_{1} g_{1}(x)+B_{2} g_{2}(x)+\cdots+B_{m} g_{m}(x)$ 这样的函数，我们同样可以使用最小二乘法得到一个近似解。因为同样的理由，该解同样是使 y 轴上距离平方和最小的解

基于同样的理由，我们可以更进一步，我们可以使用最小二乘法来得到椭圆图像的一个近似解。
The general equation for an ellipse (actually, for a nondegenerate conic section) is
$x^{2} + By^{2} + Cxy + Dx + Ey + F=0$
此时可以看作 $z = f(x, y) = x^{2} + By^{2} + Cxy + Dx + Ey$，我们的每个数据点 $(x_0, y_0)$ 可以升维后看作 $(x_0, y_0, z = 0)$。得到矩阵

$$A=\left(\begin{array}{rrrrr}
4 & 0 & 0 & 2 & 1 \\
1 & 2 & 2 & 1 & 1 \\
1 & -1 & 1 & -1 & 1 \\
4 & 2 & -1 & -2 & 1 \\
1 & -3 & -3 & 1 & 1 \\
1 & 1 & -1 & -1 & 1
\end{array}\right) \quad x=\left(\begin{array}{c}
B \\
C \\
D \\
E \\
F
\end{array}\right) \quad b=\left(\begin{array}{r}
0 \\
-4 \\
-1 \\
-1 \\
-9 \\
-1
\end{array}\right)$$

我们可以使用最小二乘法得到一个到在 z 轴上距离平方和最小的解 (数据点和 a nondegenerate conic section 图像在 z 轴上距离平方和最小的解

