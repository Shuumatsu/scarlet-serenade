# Minimum Spanning Tree

Given a connected and undirected graph, a spanning tree of that graph is a subgraph that is a tree and connects all the vertices together.

MST: a spanning tree with weight less than or equal to other spanning trees'.

## Kruskal’s Minimum Spanning Tree Algorithm

The main idea is to repeatedly pick the smallest edge.

1. Sort all the edges in non-decreasing order of their weight.
2. Pick the smallest edge. Check if it forms a cycle with the spanning tree formed so far. If cycle is not formed, include this edge. Else, discard it. (based on Union-Find algorithm)
3. Repeat step#2 until there are (V-1) edges in the spanning tree.

### Practice

> https://www.hackerrank.com/challenges/kruskalmstrsub/problem

In this shitty problem, the node number starts from 1

```js
const kruskals = (nodes_c, g_from, g_to, g_weight) => {
    const edges = []
    for (let i = 0; i < g_from.length; i += 1) {
        edges.push([g_weight[i], g_from[i] - 1, g_to[i] - 1])
    }
    edges.sort(([wa], [wb]) => wb - wa)

    const roots = []
    const ranks = []
    for (let i = 0; i < nodes_c; i += 1) {
        roots.push(i)
        ranks.push(1)
    }
    const find_root = a => {
        let curr = a
        while (curr != roots[curr]) {
            const next = roots[curr]
            roots[curr] = roots[next]
            curr = next
        }
        return curr
    }
    const is_connected = (a, b) => find_root(a) == find_root(b)
    const connect = (a, b) => {
        if (is_connected(a, b)) {
            return
        }
        const ra = find_root(a)
        const rb = find_root(b)

        if (ranks[ra] >= ranks[rb]) {
            roots[rb] = ra
            ranks[ra] += ranks[rb]
        } else {
            connect(b, a)
        }
    }

    let chosen_c = 0
    let weight = 0
    while (chosen_c < nodes_c - 1) {
        const [w, a, b] = edges.pop()
        if (is_connected(a, b)) {
            continue
        }
        chosen_c += 1
        connect(a, b)
        weight += w
    }
    return weight
}
```

## Prim’s Minimum Spanning Tree Algorithm

Different from Kruskal's algorithm, its main idea is to continuously select the nearest point from the remaining points.

### Practice 

> https://www.hackerrank.com/challenges/primsmstsub/problem

In this shitty problem, the node number starts from 1

```python
from heapq import heappush, heappop

def prims(vertices_c, edges, start):
    start -= 1
    adjacency = []
    for _ in range(0, vertices_c):
        adjacency.append({})
    for [left, right, weight] in edges:
        adjacency[left - 1][right - 1] = weight
        adjacency[right - 1][left - 1] = weight

    mst = set()
    mst_w = 0

    distance = []
    for _ in range(0, vertices_c):
        distance.append(2**32 - 1)
    distance[start] = 0

    pending = []
    heappush(pending, (0, start))

    while len(mst) < vertices_c:
        [curr_d, curr] = heappop(pending)

        if distance[curr] < curr_d or curr in mst:
            continue
        print(curr_d, curr + 1)

        mst.add(curr)
        mst_w += curr_d

        for [neighbor, w] in adjacency[curr].items():
            if neighbor in mst:
                continue

            if distance[neighbor] > w:
                distance[neighbor] = w
                heappush(pending, (w, neighbor))

    return mst_w
```

## Boruvka’s Minimum Spanning Tree algorithm

The idea is kinda like a combination of Prim's and Kruskal's algorithm. Every vertex is initially regarded as a component (of a disjoint-set). 
Calc the shortest edge from one component to some other component (so when comparing, use the root in disjoint-set) and then connect them together. 
It iterates over the edges while `components_c != 0`, 如果当前边连接的两个节点不属于同一个 component 的话，并且如果这个距离对于其中一个 component 来说是最近的话就需要将这两个 component 连接起来。

### Practice

> https://www.hackerrank.com/challenges/primsmstsub/problem

(this link is the same with the link given in the Prim's part, anyway it can also be used here for practice

In this shitty problem, the node number starts from 1

```js
// in fact it should be `boruvka`
const prims = (n, edges, _) => {
    edges = edges.map(([a, b, w]) => [a - 1, b - 1, w])

    const roots = []
    const ranks = []
    for (let i = 0; i < n; i += 1) {
        roots.push(i)
        ranks.push(1)
    }
    const find_root = a => {
        let curr = a
        while (curr != roots[curr]) {
            const next = roots[curr]
            roots[curr] = roots[next]
            curr = next
        }
        return curr
    }
    const is_connected = (a, b) => find_root(a) == find_root(b)
    const connect = (a, b) => {
        if (is_connected(a, b)) {
            return
        }
        const ra = find_root(a)
        const rb = find_root(b)

        if (ranks[ra] >= ranks[rb]) {
            roots[rb] = ra
            ranks[ra] += ranks[rb]
        } else {
            connect(b, a)
        }
    }

    let r = 0
    let components_c = n
    while (components_c > 1) {
        const nearest = new Array(n)
        for (const [a, b, w] of edges) {
            const ra = find_root(a)
            const rb = find_root(b)
            if (ra == rb) {
                continue
            }

            if (nearest[ra] == null || nearest[ra][1] > w) {
                nearest[ra] = [rb, w]
            }
            if (nearest[rb] == null || nearest[rb][1] > w) {
                nearest[rb] = [ra, w]
            }
        }

        for (let a = 0; a < n; a += 1) {
            if (nearest[a] == null) {
                continue
            }
            const [b, w] = nearest[a]
            if (is_connected(a, b)) {
                continue
            }
            connect(a, b)
            components_c -= 1
            r += w
        }
    }
    return r
}
```