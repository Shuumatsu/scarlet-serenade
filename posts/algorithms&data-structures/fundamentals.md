---
title: Algorithms & Data Structures | Fundamentals
---

二分的本质在于，对于某种性质，我们可以将区间分为两部分一部分是满足该性质的，一部分是不满足该性质的。

有序数组中的目标元素查找是最基本的一个应用，根据中点可以将数组分为两部分，一部分是满足其中所有元素都小于（或等于）目标元素的，一部分是不满足的，而目标元素一定在其中一部分。

> :a[https://www.acwing.com/activity/content/problem/content/823/1/]{href=https://www.acwing.com/activity/content/problem/content/823/1/ .nav}

```c++
int main() {
    int n = read<int>();
    int q = read<int>();

    vector<int> xs;
    for (int i = 0; i < n; i += 1) { xs.push_back(read<int>()); }

    function<int(int, int, int)> query_left = [&](int i, int j, int target) {
        if (i > j) { return -1; }
        if (i == j) { return xs[i] == target ? i : -1; }

        int m = (i + j) / 2;
        if (xs[m] >= target) { return query_left(i, m, target); }
        return query_left(m + 1, j, target);
    };
    function<int(int, int, int)> query_right = [&](int i, int j, int target) {
        if (i > j) { return -1; }
        if (i == j) { return xs[i] == target ? i : -1; }

        // 我们可以根据是否 + 1 来控制当仅剩两个元素的时候，中点是 i 还是 j
        // 若不 + 1，这里则会是 i，在我们选中右侧区间 m..= j 的情况下
        //     - 要么进入死循环
        //     - 要么在 base case 中处理 i + 1 == j 的情况
        int m = (i + j + 1) / 2;
        if (xs[m] <= target) { return query_right(m, j, target); }
        return query_right(i, m - 1, target);
    };

    for (int i = 0; i < q; i += 1) {
        int target = read<int>();

        int left = query_left(0, xs.size() - 1, target);
        int right = query_right(0, xs.size() - 1, target);

        cout << left << " " << right << endl;
    }

    return 0;
}
```