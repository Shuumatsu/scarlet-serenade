---
title: Hamming Distance
---

在信息论中，两个等长字符串之间的汉明距离（英語：Hamming distance）是两个字符串对应位置的不同字符的个数。换句话说，它就是将一个字符串变换成另外一个字符串所需要替换的字符个数。
- e.g., `1011101` 与 `1001001` 之间的汉明距离是 `2`。

## 计算两个数字的 Hamming Distance

思路非常简单，将两个数异或之后计算其中 1 的数量即可

### Practice

:a[461. Hamming Distance]{href=https://leetcode.com/problems/hamming-distance/ .nav}

```c++
class Solution {
public:
    int hammingDistance(int x, int y) {
        int diff = x ^ y;
        int count = 0;
        while (diff > 0) {
            count += diff & 1;
            diff >>= 1;
        }
        return count;
    }
};
```

## 计算多个数字的 Hamming Distance 之和

假如给定了 `n` 个 `u64` 的数字的话，如果两两比较之后再将他们之间的距离加起来的话，时间复杂度将会达到 $O(n^2)$. 所以我们需要一个更好的算法


我们可以单独考虑所有数的每一位，再将每一位的的距离加起来即可。遍历所有数的第 `i` 位，可以得到总共有 `a` 个 `0`, `b` 个 `1` \ 
那么这一位的 Hamming Distance 之和就为 `a * b`

### Practice

:a[477. Total Hamming Distance]{href=https://leetcode.com/problems/total-hamming-distance/ .nav}

```rust
impl Solution {
    pub fn total_hamming_distance(nums: Vec<i32>) -> i32 {
        let mut count = 0;
        for i in 0..32 {
            let mut ones = 0;
            let mut zeros = 0;
            for &num in nums.iter() {
                let mask = 1 << i;
                if mask & num == mask {
                    ones += 1;
                } else {  
        count
    }
}
```