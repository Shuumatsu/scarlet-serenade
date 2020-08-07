estimate the probability of parameters given a parametric model and observed data drawn from it.

## Maximum Likelihood Estimates

Maximum Likelihood Estimate (MLE) answers the question:

> For which parameter value does the observed data have the biggest probability?

The MLE is an example of a point estimate because it gives a single value for the unknown parameter.

（注意我们假设每个样本点都是独立同分布的。

### 离散的情况

考虑显性基因 A 与隐性基因 B，在人群中 frequency of A is θ, frequency of a is 1 - θ. Suppose we test a random sample of people and find that $k_1$ are AA, $k_2$ are Aa, and $k_3$ are aa. Find the MLE of θ.
    即我们要找到参数 θ 使得在 $k_1 + k_2 + k_3$ 个人的抽样中，$k_1$ are AA, $k_2$ are Aa, and $k_3$ are aa 的概率最高即

$$P\left(k_{1}, k_{2}, k_{3} | \theta\right)=\left(\begin{array}{c}
k_{1}+k_{2}+k_{3} \\
k_{1}
\end{array}\right)\left(\begin{array}{c}
k_{2}+k_{3} \\
k_{2}
\end{array}\right)\left(\begin{array}{c}
k_{3} \\
k_{3}
\end{array}\right) \theta^{2 k_{1}}(2 \theta(1-\theta))^{k_{2}}(1-\theta)^{2 k_{3}}$$

​    对该以 θ 为参数的函数求最值即可

### 连续的情况

这时我们不能像离散那样直接用概率去计算，因为连续的情况下，单个点的概率是 0。这里我们使用 pdf. 我们不考虑每个样本点的概率，我们考虑每个样本点所在的小区间

以指数分布为例，假设我们样本中有一个点的值为 $x_1 = 2$，则在其附近非常小的一个区间的概率为 $f_{X_{1}}\left(x_{1} | \lambda\right) d x_{1}$。假设有另一个点 $x_2 = 3$，则有

$$P\left(X_{1} \text { in range, } X_{2} \text { in range } | \lambda\right) \approx \lambda e ^{-2 \lambda} d x_{1} \cdot \lambda e ^{-3 \lambda} d x_{2}=\lambda^{2} e ^{-5 \lambda} d x_{1} d x_{2}$$

可以发现 $d x_{1} d x_{2}$ 并不影响在哪取到最值，So for the MLE we drop it: likelihood = $f\left(x_{1}, x_{2} | \lambda\right)=\lambda^{2} e ^{-5 \lambda}$

---

因为经常的情况是几个事件同时发生（比如上面这个例子），所以我们列出来的式子里通常是很多个数相乘，所以我们通常取对数后再求最值

### 考虑几个常见的分布

#### Binomial distribution

MLE 得到的参数 p 与所有样本中，目标事件在样本中的频率相同
    e.g. 掷 100 次某种硬币，其中有 55 次正面，那么最可能的情况是该硬币有 0.55 的概率投出正面

#### Normal Distribution

正态分布有两个参数，均值 $\mu$ 以及标准差 $\sigma$。均值等于样本的均值，标准差等于样本的标准差。

#### Exponential Distribution

λ turned out to be the reciprocal of the sample mean，也就是均值等于样本的均值

#### Uniform distribution

均匀分布有两个参数 [left, right]，有 $P(sample|left, right) = (\frac{1}{right - left})^{size(sample)}$. 所以 left 取最小的样本，right 取最大的样本。
    均匀分布的参数只和相距最远的两个样本有关


























