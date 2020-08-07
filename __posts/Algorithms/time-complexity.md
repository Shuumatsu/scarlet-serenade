$O(g(n)) = \{f(n) : \exist c > 0, n_0 > 0, s.t. \forall n \geq n_0, 0 \leq f(n) \leq cg(n) \}$
$\Omega(g(n)) = \{f(n) : \exist c > 0, n_0 > 0, s.t. \forall n \geq n_0, 0 \leq cg(n) \leq f(n) \}$
$\theta(g(n)) = \{f(n) : \exist c_1 > 0, c_2 > 0, n_0 > 0, s.t. \forall n \geq n_0, 0 \leq c_1g(n) \leq f(n) \leq c_2g(n) \}$

这里我们用等号表示集合的成员关系， $f(n) = O(n)$ means $f(n) \in O(n)$


e.g. heapify 的时间复杂度为 O(log(n))