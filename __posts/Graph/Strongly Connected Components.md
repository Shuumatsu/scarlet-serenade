A directed graph is strongly connected if there is a path between all pairs of vertices. A strongly connected component (SCC) of a directed graph is a maximal strongly connected subgraph.

## Kosaraju’s algorithm

We can find all strongly connected components in O(V+E) time using Kosaraju’s algorithm.

the idea is from topological sort

```
for vertex in 0..n: 
    DFS & keep a stack, push vertex into the stack like postorder
transpose the adjacent matrix
while let Some(vertex) = stack.pop():
    DFS(those vertices visited in this round of DFS composes a SCC
```

### Practice 

> https://practice.geeksforgeeks.org/problems/strongly-connected-components-kosarajus-algo/1

```c++
vector<int> fake_topo(int V, vector<int> adj[]) {
    vector<bool> visited(V);
    vector<int> fake_topo_sorted(V);
    function<void(int)> f = [&](int vertex) {
        if (visited[vertex]) { return; }
        visited[vertex] = true;

        for (auto neighbor : adj[vertex]) { f(neighbor); }
        fake_topo_sorted.push_back(vertex);
    };

    for (auto vertex = 0; vertex < V; vertex += 1) { f(vertex); }
    return fake_topo_sorted;
}

int kosaraju(int V, vector<int> adj[]) {
    auto fake_topo_sorted = fake_topo(V, adj);

    vector<vector<int>> rev_adj(V);
    for (auto vertex = 0; vertex < V; vertex += 1) {
        for (auto neighbor : adj[vertex]) {
            rev_adj[neighbor].push_back(vertex);
        }
    }

    vector<bool> visited(V);
    function<void(int)> f = [&](int vertex) {
        if (visited[vertex]) { return; }
        visited[vertex] = true;
        for (auto nei    ghbor : rev_adj[vertex]) { f(neighbor); }
    };

    auto c = 0;
    while (fake_topo_sorted.size() > 0) {  
        auto vertex = fake_topo_sorted.back();
        fake_topo_sorted.pop_back();

        if (visited[vertex]) { continue; }

        c += 1;
        f(vertex);
    }
    return c;
}
```