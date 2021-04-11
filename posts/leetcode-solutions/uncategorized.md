---
title: 'LeetCode Solutions | Uncategorized'
---


#### 1353. Maximum Number of Events That Can Be Attended

贪心的想法，每次我们取最快要过期的那个 event1，称为 Schedule 1

proof：（我觉得这个证明哪里感觉起来有点卡啊。。不过无所谓了反正这题是用来练线段树的

-   At some day, suppose both events E1 and E2 are available to choose to attend. For contradictory purpose, suppose the event E1 that is going to end sooner is not the best choice for now. Instead, E2 that ends later is the best choice for now. By choosing E2 now, you come up with a schedule S1.
-   I claim that I can always construct another schedule S2 in which we choose E1 instead of E2 for now, and S2 is not worse than S1.

    -   In S1, from now on, if E1 is picked some time after, then I can always swap E1 and E2 in S1, so I construct a S2 which is not worse than S1.
    -   In S1, from now on, if E1 is not picked some time after, then I can aways replace E2 in S1 with E1, so I construct a S2 which is not worse than S1.

-   So it is always better (at least not worse) to always choose the event that ends sooner.

-   1. 按开始时间排序。curr_t 表示当前时间，把所有已开始的事件加入到按结束时间的最小堆
-   2. 用线段树，线段树存储每个段内最小的空闲时间。搜索每一个事件，如果返回的时间事件还没结束就加一，并且把那个时间设为忙碌

#### 1365. How Many Numbers Are Smaller Than the Current Number

注意到每个数都在 0，100 的范围内，考虑桶排序。
那么对于每个桶内的元素来说，小于他的元素个数就等于前面所有桶内的元素数之和，这个可以用只依赖前一个桶的 DP 算出来

#### :a[1466. Reorder Routes to Make All Paths Lead to the City Zero]{href=https://leetcode.com/problems/reorder-routes-to-make-all-paths-lead-to-the-city-zero/}

regard all connections as bidirectional, do a tree traversal from 0. if some connection should be reversed, then `ret += 1`


#### :a[1448. Count Good Nodes in Binary Tree]{href=https://leetcode.com/problems/count-good-nodes-in-binary-tree/}

dfs

#### :a[1573. Number of Ways to Split a String]{href=https://leetcode.com/problems/number-of-ways-to-split-a-string/}

- 当输入全为 0 时，通过隔板法得 $C_n^2$
- 当输入中 1 的个数不为 3 的倍数时，返回 0 
- otherwise, 用 `accu` 表示当前一遇到多少个 1，通过 accu 判断当前位置可属于哪一段

```python
for c in arr:
    if c == '1':
        accu += 1
    if accu == target:
        ways_of_first_cut += 1
    elif accu == target * 2:
        ways_of_second_cut += 1

return (ways_of_first_cut * ways_of_second_cut) % (10**9 + 7)
```

#### :a[1576. Replace All ?'s to Avoid Consecutive Repeating Characters]{href=https://leetcode.com/problems/replace-all-s-to-avoid-consecutive-repeating-characters/}

trivial

#### :a[1578. Minimum Deletion Cost to Avoid Repeating Letters]{jhref=https://leetcode.com/problems/minimum-deletion-cost-to-avoid-repeating-letters/}

类似于压缩字符串，记录每一段的总消耗和最大单个消耗，相减即为需要的每段最小消耗

#### :a[1615. Maximal Network Rank]{href=https://leetcode.com/problems/maximal-network-rank/}

count degrees of every city

for every pair, there can be at most 1 edge in-common

记 `first` 表示所有节点中度数的最大值，`second` 表示所有节点中度数的次大值。显然，在挑选城市对时，我们只需要考虑拥有最大或者次大度数的城市就行了，这是因为

求出数组 `indeg` 并得到最大 / 次大度数以及对应的城市集合


#### :a[1647. Minimum Deletions to Make Character Frequencies Unique]{href=https://leetcode.com/problems/minimum-deletions-to-make-character-frequencies-unique/}

```typescript
const occurrences = new Set()
for (let i = 0; i < freqs.length; i += 1) {
    while (freqs[i] > 0 && occurrences.has(freqs[i])) {
        freqs[i] -= 1
        ret += 1
    }

    occurrences.add(freqs[i])
}
```

#### :a[1682. Longest Palindromic Subsequence II]{href=https://leetcode.com/problems/longest-palindromic-subsequence-ii/}

memoized recursion / dp 

因为条件 "No two consecutive characters are equal, except the two middle ones." 我们有两个选择: 

- `fn find(arr: &[char], i: usize, j: usize) -> (usize, Vec<char>)`
    - `find(arr, i + 1, j - 1)` 中可能有多个长度相同但是边缘字母不同的回文子串（例如 `abab`），这使得我们在 `find` 中需要复杂的判断来合并回文子串
- `fn find(arr: &[char], i: usize, j: usize, prev: char) -> usize`

我们选择后者

```rust
let mut ret = 0;
if arr[i] == arr[j] && prev != arr[i] {
    ret = ret.max(2 + find(arr, i + 1, j - 1, arr[i], cache));
} else {
    ret = ret.max(find(arr, i + 1, j, prev, cache));
    ret = ret.max(find(arr, i, j - 1, prev, cache));
}
```

:a[压缩至二维]{href=https://leetcode.com/problems/longest-palindromic-subsequence-ii/discuss/981739/Java-greater-Recursive-(TLE)-greater-Memoization-greater-3D-Bottom-Up-greater-2D-Bottom-Up-(O(n)-Space) .nav}