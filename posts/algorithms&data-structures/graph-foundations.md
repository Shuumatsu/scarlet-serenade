---
title: Algorithms & Data Structures | Graph Foundations
---

**Proposition**: 若图的每个节点都有 2 个或以上的边则一定存在回路

**Proof**: 对每个连通分量，假设有 `n` 个点，从某点开始访问其邻居。若无环，则一定有不出现在 `visited[]` 中的邻居，当访问到第 `n` 个点时，第 `n + 1` 个点，矛盾。

---

**Proposition**: 有 2 个或以上顶点的树一定有叶

**Proof**: 若没有叶，则每个顶点的度数不小于 2，则一定有环，矛盾

---

**Proposition**: 有 `n` 个顶点的树有 `n - 1` 个边

**Proof**: 用归纳法，假设命题对 `n - 1` 个顶点的树成立。考虑 有 `n` 个顶点的树，删掉其某一个叶（同时少了一条边），则得到一个 `n - 1` 个顶点的树，有 `n - 2` 条边，则原树有 `n - 2 + 1` 条边。

---

**Euler's formula of Planar graph**：`vertex_cnt + face_cnt - edge_cnt = 2`

**Proof**: 
- `face_cnt = 1`，则该图为树，命题显然成立。
- `face_cnt > 1`，则该图一定有环，打破一个环（删掉其中一边）则减少一个面，该操作不影响等式左侧的值。重复此操作直到 `face_cnt = 1`
    - `vertex_cnt + face_cnt - edge_cnt = vertex_cnt + (face_cnt - 1) - (edge_cnt - 1)`

---

**Proposition**: 所有的树都是二分图

**Proof**: 按层宽搜即可

---

**Proposition**: 两个及以上顶点的图定有 2 个顶点有相同的度数

**Proof**: 考虑有 `n >= 2` 个顶点的图，用反证法，假设所有顶点的度数都不相同，那么有度数序列 `0..=n - 1`，删除度数为 0 的点不影响其它点的度数，而有 `n - 1` 个顶点的图的最大度数为 `n - 2`，矛盾 

---  

**Definition**: $V_1$ 中的每个点与 $V_2$ 中的每个点之间都有边的二分图称为完全二分图

完全二分图有 `vertex_cnt1 * vertex_cnt2` 条边

---

**Definition**:  所有顶点都有度数 `K` 的图称为 `K-Regular Graph`

`K-Regular Graph` 有 `vertex_cnt * K / 2` 条边

---

**Definition**: 任意两个顶点间都有边的图称为完全图

完全图有 `vertex_cnt * (vertex_cnt - 1) / 2` 条边

---

**Score Theorem**:
