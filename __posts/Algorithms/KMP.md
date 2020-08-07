# KMP algorithm

e.g. `text: abababababca` 
   & `pattern: abababca`

算法的核心思想是，与其每次发生不匹配的时候，与其将 pattern 向右移动一位，并从 pattern 的头部开始重新比较

```
abababababca
abababca
=> 
abababababca
 abababca
```

不如利用当前已匹配成功的部分的最长公共前后缀来移动更多的距离，

- 因为如果 text 的已匹配的部分的后缀和 pattern 的前缀不同的话，一定不会匹配成功

- 取最长是因为这样向右移动的距离最小，不会漏掉某个匹配
- 当我们讨论最长公共前后缀的时候，要求前后缀并不是原串，因为这种情况下是恒等的，没有意义

比如我们这个例子，在 pattern 的 c 处匹配失败的时候，c 的前面 `ababab` 已匹配的部分有最长公共前后缀 `abab`，则我们直接把这个前缀移动到本来后缀的位置。

注意到 text 的已匹配的部分的后缀也是当前 pattern 的已匹配的后缀，则每次匹配失败时 pattern 的移动到哪其实是与 text 无关的，所以我们可以在匹配前对 pattern 进行一个预处理，计算出每次匹配失败的时候，应该将其如何移动。


```
abababababca
abababca
=> 
abababababca
  abababca
```

---

在实际编码的过程中，通常是用两个指针 `text_idx` & `pattern_idx` 分别指向 `text` & `pattern` 中正在匹配的位置。所以我们上面提到的移动操作，在这里是将 `j` 回退。

我们用一个 `next` 数组来记录，应该回退到哪个位置。`next[pattern_idx]` 表示当 `pattern_idx` 匹配失败时，`pattern_idx` 应回退到的位置

1. next[0] = -1；-1 在这里是一个 magic number 表示，我们不改变 pattern_idx 而是让 text_idx += 1
2. next[1] = 0；因为要求前后缀并不是原串，所以这个情况下匹配失败我们只能从头开始比较

`j` 回退多少取决于目前为止的公共前后缀的长度，如上例，在 `j = 6` 时匹配失败，此时的最长公共前后缀的长度为 4，则 j 应变为 4，即第5个位置，即 `next[i] = len_common`

每次去计算当前的最长公共前后缀长度是非常费时间的，而当前 `j` 对应的最长公共前后缀与 `j - 1` 时对应的是有一定的关系的，所以我们可以用 DP 的思想来线性时间内求得 `next` 数组。

当我们已有 `next[i] = k` 时，这意味着前缀 `pattern[0, k - 1]` 与后缀 `pattern[i - k, i - 1]` 都是匹配的。现计算 next[i + 1]，


1. 若 `pattern[i] == pattern[k]` 则我们继承 `next[i] = k` 对应的前后缀并将他们都扩展一位，有 `next[i + 1] = k + 1`

```
abcdabcdf (f 处失败，已有前一个 d 处失败对应的的公共前后缀长度为 3
    abcdabcdf
```

2. 若 `pattern[i] ！= pattern[k]`，则我们不能通过扩张上次的公共前后缀来得到当前的。
```
abcdabcef (f 处失败，已有前一个 e 处失败对应的的公共前后缀长度为 3
    abcdabcef
```
观察可以发现，我们尝试扩展的一次比较，相当于是另一个 KMP 在该处的一次失败，则这里 `next[i + 1] = k + 1` 里的 `k` 应取 `next[k]`
- we repeatedly doing this until `k == -1` or it matches


以 `pattern: abababca` 为例，套用上面的过程有 `next = [-1, 0, 0, 1, 2, 3, 4, 0]`


## Practice 

https://leetcode.com/problems/implement-strstr/submissions/

```javascript
const preprocess = pattern => {
    const next = new Array(pattern.length).fill(-1)
    next[1] = 0
    for (let i = 2; i < pattern.length; i += 1) {
        let k = next[i - 1]
        while (k != -1 && pattern[k] != pattern[i - 1]) k = next[k]
        next[i] = k + 1
    }
    return next
}

/**
 * @param {string} text
 * @param {string} pattern
 * @return {number}
 */
var strStr = function (text, pattern) {
    const next = preprocess(pattern)

    let text_idx = 0
    let pattern_idx = 0
    while (pattern_idx < pattern.length) {
        if (text_idx >= text.length) return -1
        if (text[text_idx] == pattern[pattern_idx]) {
            text_idx += 1
            pattern_idx += 1
        } else {
            if (pattern_idx == 0) text_idx += 1
            else pattern_idx = next[pattern_idx]
        }
    }

    return text_idx - pattern.length
}
```