---
title: Algorithms & Data Structures | Knapsack Problems
---

## 01 Knapsack Problems

有 `n` 件物品和一个容量为 `cap` 的背包。放入第 `i` 件物品的消耗是 `cost[i]`, 收益是 `price[i]`。求解装入哪些物品得到的收益最高

### Practice  

:a[https://www.acwing.com/problem/content/2/]{href=https://www.acwing.com/problem/content/2/ .nav}

首先很容易想到递归解

```cpp
// 考虑 0..=i 个物品在给定 capacity 下的最大收益
function<int(int, int)> f = [&](int i, int capacity) {
    if (i < 0) { return 0; }
    int ret = f(i - 1, capacity);
    if (costs[i] <= capacity) {
        ret = max(ret, prices[i] + f(i - 1, capacity - costs[i]));
    }
    return ret;
};
```

翻译成动态规划，
- 因为 `f(i, *)` 的结果依赖于 `f(i - 1, *)` 的结果，所以我们升序遍历 `i`;
- 因为 `f(*, capacity)` 的结果依赖于 `f(*, <= capacity)` 的结果，所以我们升序遍历 `capacity`;

```cpp
int knapsack(int items_cnt,
             int capacity,
             vector<int> costs,
             vector<int> prices) {
    if (items_cnt == 0) { return 0; }

    vector<vector<int>> dp;
    for (int i = 0; i < items_cnt; i += 1) {
        dp.push_back(vector<int>(capacity + 1, 0));
    }
    for (int cap = costs[0]; cap <= capacity; cap += 1) {
        dp[0][cap] = prices[0];
    }
    for (int i = 1; i < items_cnt; i += 1) {
        for (int cap = 0; cap <= capacity; cap += 1) {
            dp[i][cap] = dp[i - 1][cap];
            if (cap >= costs[i]) {
                dp[i][cap] =
                    max(dp[i][cap], dp[i - 1][cap - costs[i]] + prices[i]);
            }
        }
    }

    return dp[items_cnt - 1][capacity];
}
```

我们可以进行状态压缩，注意这里需要逆序遍历 `cap`

```cpp
int knapsack(int items_cnt,
             int capacity,
             vector<int> costs,
             vector<int> prices) {
    if (items_cnt == 0) { return 0; }

    vector<int> dp(capacity + 1, 0);
    for (int i = 0; i < items_cnt; i += 1) {
        for (int cap = capacity; cap >= costs[i]; cap -= 1) {
            dp[cap] = max(dp[cap], dp[cap - costs[i]] + prices[i]);
        }
    }

    return dp[capacity];
}
```