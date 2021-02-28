---
title: 'Algorithms & Data Structures | Computational Geometry | Sorting by Polar Angle'
---

极角排序，就是平面上有若干点，选一点作为极点，那么每个点有极坐标 $(\rho, \theta)$ ，将它们关于极角 $\theta$ 排序。进行极角排序有两种方法。

第一种是直接计算极角，

$$
\begin{aligned}
r &= \sqrt{y^{2}+x^{2}} \\

\theta &= \left\{\begin{array}{ll}\arctan \left(\frac{y}{x}\right) & \text { if } x>0 \\ \arctan \left(\frac{y}{x}\right)+\pi & \text { if } x<0 \text { and } y \geq 0 \\ \arctan \left(\frac{y}{x}\right)-\pi & \text { if } x<0 \text { and } y<0 \\ \frac{\pi}{2} & \text { if } x=0 \text { and } y>0 \\ -\frac{\pi}{2} & \text { if } x=0 \text { and } y<0 \\ 0 & \text { if } x=0 \text { and } y=0\end{array}\right.

\end{aligned} 
$$

第二种方法利用叉乘。叉乘的正负遵循右手定则，按旋转方向弯曲右手四指，则若拇指向上叉乘为正，拇指向下叉乘为负。也就是说，如果一个向量通过劣角旋转到另一个向量的方向需要按逆时针方向，那么叉乘为正，否则叉乘为负。

![cross-prod](./cross-prod.png)

劣角要求两个向量在相邻的的两个象限内，否则的话该性质不符合偏序关系的条件。 

