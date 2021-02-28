---
title: Algorithms & Data Structures | Binary Search
---


查找和目标值完全相等的数


```c++
function<int(int, int, int)> f = [&](int i, int j, int target) {
    if (i > j) { return -1; }
    if (i == j) { return xs[i] == target ? i : -1; }

    int m = i + (j - i) / 2;
    if (xs[m] == target) { return m; }
    if (xs[m] < target) { return f(m + 1, j, target); }
    return f(i, m - 1, target);
};
```

---