---
title: Algorithms & Data Structures | Permutations
---

:a[generate all permutations]{href=https://leetcode.com/problems/permutations/ .nav}

```c++
vector<vector<int>> ret;
// 表示从左往右填到第 pos 位
function<void(int)> backtrack = [&](int pos) {
    if (pos == nums.size()) {
        ret.push_back(vector<int>(nums));
        return;
    }

    // 我们要考虑 pos 位置我们要填哪个数。根据题目要求我们肯定不能填已经填过的数，因此
    // 很容易想到的一个处理手段是我们定义一个标记数组 vis[] 来标记已经填过的数
    // 但其实可以用 pos 之前的位置表示已填过的数
    for (int curr = pos; curr < nums.size(); curr += 1) {
        swap(nums[pos], nums[curr]);
        backtrack(pos + 1);
        swap(nums[pos], nums[curr]);
    }
};
backtrack(0);
```

---

:a[generate next permutation]{href= .nav}  