# Power Series

## Abel's Theorem

给定一个级数 $A = \sum_{n=1}^{n} a_{n}\left(x-x_{0}\right)^{n}$

- 如果 A 在 b 处收敛，那么一定有所有的 $\{x | |x - x_0| < |b - x_0|\}$，A 绝对收敛
- 如果 A 在 b 处发散，那么一定有所有的 $\{x | |x - x_0| > |b - x_0|\}$，A 发散
 
一个常用的结论是，若幂级数再某点处条件收敛，则该点一定是收敛区间的一个端点

例题： $A = \sum_{n=1}^{\infty} \frac{(x-a)^{n}}{n}$ 在 $x = -2$ 处条件收敛，判断 $B = \sum_{n=1}^{\infty} n^{2}(x-a)^{n}$ 在 $x = ln\frac{1}{2}$处的收敛情况


An: 首先很容易得到 A 的收敛半径为 $1$，则 $|-2 - a| = 1$，可以知道 $a = -1$ 或 $a = -3$，带入验证后可排除 $a = -3$ 的情况。已知 $a$ 的情况下很容易得到 $B$ 的收敛区间为 $(-2, 0)$，则在 $x = ln\frac{1}{2}$ 处绝对收敛




- 收敛域与收敛区间：收敛域与收敛区间区别只有一个：区间是否闭合。
  - 如果幂级数半径为 $R$ 那么马上可以得到收敛区间 $(-R, R)$。如果进一步讨论，该级数在点 $-R$或 $R$ 处的收敛性，比如在点 $-R$ 收敛，在点 $R$ 不收敛，则称该幂级数的收敛域为 $[-R,R)$。
  - 简而言之，收敛区间直接根据收敛半径而得，收敛域是讨论收敛区间两端点收敛性后的结论。收敛区间可能同于收敛域，可能是收敛域的子集。

## 求幂级数的收敛域

通常的方法是先计算出收敛区间，再判断两个端点的收敛情况。

求收敛区间最常用的两个方法是利用公式 

$$\frac{1}{R} = \lim_{n \rightarrow \infty} | \frac{a_{n+1}}{a_n} |$$

$$\frac{1}{R} = \lim _{n \rightarrow \infty} \sqrt[n]{\left|a_{n}\right|}$$

---

例如求 $\sum_{n=1}^{\infty} (-1)^n \frac{\sqrt n x^n}{n!}$ 的收敛域

$$
\lim _{n \rightarrow \infty}\left|\frac{a_{n+1}}{a_{n}}\right|=\lim _{n \rightarrow \infty} \frac{\sqrt{n+1}}{(n+1) !} \cdot \frac{n !}{\sqrt{n}}=\lim _{n \rightarrow \infty} \frac{1}{n+1} \sqrt{1+\frac{1}{n}}=0
$$

则有 $\frac{1}{R} = 0$，那么可以知道收敛域为 $(-\infty, +\infty)$

---

例如求 $A = \sum_{n=1}^{\infty}(-1)^{n} \frac{n}{2^{n}}(x-1)^{2 n}$ 的收敛域
用变量替换，定义 $t$ 为 $(x-1)^2$，则有 $A = \sum_{n=1}^{\infty}(-1)^{n} \frac{n}{2^{n}}t^n$

$$
\lim _{n \rightarrow \infty} \sqrt[n]{\left|a_{n}\right|}=\lim _{n \rightarrow \infty} \frac{\sqrt[n]{n}}{2}=\frac{1}{2}
$$

则有收敛半径 $R = 2$，因为 $t$ 的取值范围为 $[0, +\infty)$，则有收敛区间为 $[0, 2)$，将 $t = (x-1)^2$ 带入回去得到，$x$ 的幂级数的收敛区间为 $(1- \sqrt 2, 1 + \sqrt 2)$。检查两个端点的情况都是发散，则A的收敛域为 $(1- \sqrt 2, 1 + \sqrt 2)$

## 常用的麦克劳林展开式

$$ 
\frac{1}{1-x} = 1 + x + x^2 + \cdots + x^n + \cdots, \quad x \in (-1, 1) 
$$ (1)

$$
\frac{1}{1+x} = 1 - x + x ^ 2 + \cdots + (-1)^n x ^n + \cdots, \quad x \in (-1, 1)
$$ (2)

## 将函数展开为幂级数


- 直接法
  - 求出函数在 $x_0$ 点各阶导数，写出在 $x_0$ 处的泰勒级数
  - 考察余项极限是否为 $0$

- 间接法
  - 利用常见的麦克劳林展开公式通过适当变量代换或者借助幂级数性质（四则运算，逐项求导，逐项积分）将函数展开为幂级数
    - 四则运算是显然的
    - 因为幂级数展开后都是多项式的形式，易于积分，那么便可以利用逐项积分的性质，将原函数看作某个函数的积分。利用逐项求导的性质同理。
    
例如可通过四则运算将 $f(x)=\frac{3 x}{2+x^{2}}$ 展开为 $x$ 的幂级数 
    
$$
\begin{aligned}
f(x) &=\frac{3 x}{2} \cdot \frac{1}{1+\frac{x^{2}}{2}}=\frac{3 x}{2} \sum_{n=0}^{\infty}(-1)^{n}\left(\frac{x^{2}}{2}\right)^{n} \\
&=\sum_{n=0}^{\infty} \frac{(-1)^{n} 3 x^{2 n+1}}{2^{n+1}}, x \in(-\sqrt{2}, \sqrt{2})
\end{aligned}
$$

例如可通过逐项积分的性质将 $f(x)=\arctan \frac{1+x}{1-x}$ 展开为 $x$ 的幂级数 

$$
\begin{aligned}
&f^{\prime}(x)=\frac{1}{1+x^{2}}=\sum_{n=0}^{\infty}(-1)^{n}\left(x^{2}\right)^{n}=\sum_{n=0}^{\infty}(-1)^{n} x^{2 n}, x \in(-1,1)\\
&f(x)-f(0)=\int_{0}^{x} f^{\prime}(t) \mathrm{d} t=\sum_{n=0}^{\infty} \int_{0}^{x}(-1)^{n} t^{2 n} \mathrm{d} t=\sum_{n=0}^{\infty} \frac{(-1)^{n} x^{2 n+1}}{2 n+1}
\end{aligned}
$$
而 $f(0)$ 是可以直接求得的，那么通过上式就可得到 $f(x)$ 

例如可通过一定的变换，通过麦克劳林展开求函数 $f(x)=\frac{1}{(x+2)^{2}}$ 在 $x = -1$ 处的幂级数展开

$$
\begin{aligned}
f(x) &=-\left(\frac{1}{x+2}\right)^{\prime}=-\left(\frac{1}{1+(x+1)}\right)^{\prime}=-\left(\sum_{n=0}^{\infty}(-1)^{n}(x+1)^{n}\right)^{\prime} \\
&=-\sum_{n=1}^{\infty}(-1)^{n} n(x+1)^{n-1}, \quad x \in(-2,0)
\end{aligned}
$$

利用幂级数展开与泰勒展开的对应关系，还可以求函数的高阶导数。例如将 $f(x)=x^{2} \ln (1+x)$ 展开为 $x$ 的幂级数，求 $f^{(n)}(0)(n>2)$

$$
f(x)=x^{2} \ln (1+x)=\sum_{n=1}^{\infty} \frac{(-1)^{n-1} x^{n+2}}{n}
$$

$$
\begin{array}{c}
a_{n}=\frac{(-1)^{n-3}}{n-2}=\frac{(-1)^{n-1}}{n-2} \cdot(n>2) \\
\frac{f^{(n)}(0)}{n !}=\frac{(-1)^{n-1}}{n-2} \Rightarrow f^{(n)}(0)=\frac{(-1)^{n-1} n !}{n-2}  
\end{array}
$$


## 级数求和

- 可以给常数项级数替换或者添加 $x$ 转变为函数项级数
- 利用几个常用的麦克劳林展开式以及幂级数的性质

例如