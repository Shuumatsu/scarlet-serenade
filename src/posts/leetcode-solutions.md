---
title: 'LeetCode Solutions'
---

#### :a[1. Two Sum]{href=https://leetcode.com/problems/two-sum/ .nav}

use a hashtable, key is the needing num, and the value is the position

#### :a[2. Add Two Numbers]{href=https://leetcode.com/problems/add-two-numbers/ .nav}

use a variable to indicate if carrying

#### :a[3. Longest Substring Without Repeating Characters]{href=https://leetcode.com/problems/longest-substring-without-repeating-characters/ .nav}

sliding window, instead of manually moving `i` to right each time, we can use a table to store the index the i will move to

#### :a[17. Letter Combinations of a Phone Number]{href=https://leetcode.com/problems/letter-combinations-of-a-phone-number/ .nav}

cartesian product

#### :a[26. Remove Duplicates from Sorted Array]{href=https://leetcode.com/problems/remove-duplicates-from-sorted-array/ .nav}

two pointers, where to store, whats the current value, compare current value with last stored

#### :a[36. Valid Sudoku]{href=https://leetcode.com/problems/valid-sudoku/ .nav}

the same with 348. Design Tic-Tac-Toe

#### :a[38. count-and-say]{href=https://leetcode.com/problems/count-and-say/ .nav}

just iterate

#### 41. First Missing Positive

:a[https://github.com/Shuumatsu/Ryokou/issues/12]{href=https://github.com/Shuumatsu/Ryokou/issues/12 .nav}

---

make an array, iterate through xs, mark array[x] as true, find the first index at which the value is false

---

O(1) space: move all elements within range to its right position, find the first index where the value is not right

---

:a[https://github.com/Shuumatsu/Ryokou/issues/12]{href=https://github.com/Shuumatsu/Ryokou/issues/12 .nav}

#### 45. Jump Game II

1. calc min steps from tail to head
2. bfs, use a curr and prev `furthest` value to indicating current level

#### 49. Group Anagrams

其实就是按照字符串中每个元素出现的个数分类，可以用一个长度位 26 的数组作为 key，遍历字符串的时候 arr[c] += 1，相同 key 对应的字符串就应该被归类在一起

如果个数只有 0，或者一，26 位的数组就可以用 26bit 的二进制数优化

如果空间有限的话，可以把每个 char 对应到一个素数，key = \prod primes

#### 53. Maximum Subarray

一眼看上去就很像标准的 sliding window 问题，设立 i，j 两个指针表示范围。当范围内的和为正的时候表示这段距离还有价值，保留，增加 j。当范围内的和为负的时候，不如舍弃这一段让 i=j，sum 从零开始

---

仔细看本题的 sliding window 解法，其实 i 没有实际意义，可以只用 j，四舍五入就是直接遍历数组就行了，sum = max(0, sum)，每次 sum 取 0 就代表舍弃了前面的那段

#### 55. Jump Game

memorized recursion or dp

---

check from the back, if the final lastpos that can reach the end(or the prev lastpos) equals to zero, then return true

#### 57. Insert Interval

binary search then insert

#### 71. Simplify Path

use an array, `.` makes no change, `..` decrease length by one

#### 73. Set Matrix Zeroes

use two arrays to indicate elements on which rows and cols should be set 0

---

instead of using two arrays, use the first element on the row and col

#### 78. Subsets

backtracking a tree-like structure

:span[external link]{href=https://leetcode.com/problems/subsets/discuss/27281/A-general-approach-to-backtracking-questions-in-Java-Subsets-Permutations-Combination-Sum-Palindrome-Partitioning .nav}

#### 79. Word Search

check neighbors with a visited hash or making a mark on the board

#### 80. Remove Duplicates from Sorted Array II

two pointers, next position to check and next position to store, compare next value with last 2 stored value

#### 82. Remove Duplicates from Sorted List II

continuing check to following values to determine if its acceptable

#### 83. Remove Duplicates from Sorted List

inplace, check curr with last

---

another list to store, check last stored value with curr value

#### 86. Partition List

two list

#### 89. Gray Code

trash problem, dont do it
remember how to get/set the bit number at specified position is enough

#### 90. Subsets II

use a hash set to get the count of every number, recursion on unique numbers, decide how many are selected

---

instead of recursion, iter, get result from prev decided expr

#### 93. Restore IP Addresses

just do it

#### 99. Recover Binary Search Tree

#### 110. Balanced Binary Tree

just do it

#### 131. Palindrome Partitioning

brute force recursion

#### 134. Gas Station

find the travel length `l` of starting point `i`, the travel length of starting point in `[i .. i + j]` are no bigger than `l`

---

if the total gas is greater than costs, there must be a starting point. start `i` and start point from 0, each time `curr` reaches a negative value, set starting point to `i + 1`

#### 139. Word Break

recursion+memorization or dp

#### 146. LRU Cache

hashmap & dll

#### 150. Evaluate Reverse Polish Notation

#### 162. Find Peak Element

binary search, whether `m` is at a descending slope or not

#### 163. Missing Ranges

let `lo = lower - 1` initially, then compare each value with lo then update lo, finally check lo with upper

#### 166. Fraction to Recurring Decimal

using long division(the way we do division by hand) and a hash map to check if the current numerator has once appeared, if so, we found the repeating range. the first time the current numerator is smaller than the denomenater, place the comma

#### 172. Factorial Trailing Zeroes

every zero comes from factors 5 \* 2, there are plenty of 2s, so the key is to find how many 5-factors.

---

:a[community solution]{href=https://leetcode.com/problems/factorial-trailing-zeroes/discuss/52371/My-one-line-solutions-in-3-languages/112755 .nav}

#### 190. Reverse Bits

get every pos using bit op then construct a new one

---

every time you right shift the original number, left shift the resulting number then add 1(or 0)

#### 199. Binary Tree Right Side View

find right most of each level

#### 202. Happy Number

Detect Cycles with a HashSet

---

Floyd's Cycle-Finding Algorithm

#### 215. Kth Largest Element in an Array

small heap of size `k`

---

quicksort

#### 220. Contains Duplicate III

fixed-size balacend binary search tree to store recent k values

---

bucket sort, check nearby and self bucket

#### 243. Shortest Word Distance

hash

#### 250. Count Univalue Subtrees

just iter

#### 251. Flatten 2D Vector

the same with 341. Flatten Nested List Iterator

#### 257. Binary Tree Paths

iter

#### 263. Ugly Number

mod 2 or 3 or 5, then check the quotient

#### 277. Find the Celebrity

O(n^2): indeg/outdeg of every one

---

O(n): from 0 to `n-1` if the candidate knows the next people, then the the next people becomes the candidate. then check the deg of the final candidate

#### 283. Move Zeroes

两个指针，一个 j 指向当前查看的元素，一个 i 指向当前非 0 元素应该插入的位置。
每当检查到一个 0， j++，如果非零元素，插入 i，i++，j++。最后把 i 和她之后的位置全部置零

#### 285. Inorder Successor in BST

do an inorder traversal

---

if p is guaranteed to exist: if go left, then mark current node as next, if go right, then do nothing, if equal, then go right then go left until null.

#### 295. Find Median from Data Stream

two heaps, the median will always be one of the top or half of the sum of the top of the two heaps

---

balanced binary search tree

#### 300. Longest Increasing Subsequence

memorized recursion or dp

---

use an array to store the current length of each length, then binary search on it

#### 322. Coin Change

memorized recursion or dp

#### 326. power-of-three

binary search

---

let `a` be the biggest number which is the power of 3 within int, then `a % n == 0`

---

change base, only the most significant position is not 0.

#### 341. Flatten Nested List Iterator

flatten it in the constructor (I guess we can use call/cc to pause the recursion so we can lazy flatten it? I will check this when I fully master cps)

---

manually maintain a stack, in this way, each time we call `next()`, we execute one

#### 342. Power of Four

bit mask, every two position - if num is a power of two: x > 0 and x & (x - 1) == 0 - num & (101010...10)\_2 == 0

#### 347. Top K Frequent Elements

heap

---

count occurrence using a hash map, and then store them in an array using occurrence as the index, [element] as value, take first k element from the right side of the array

#### 348. Design Tic-Tac-Toe

maintain tow arrays and two variables for rows, cols, diagonal and anti-diagonal indicating they belong to whom or in mixed-status

#### 393. UTF-8 Validation

do it using bitmask

#### 394. Decode String

use stack to extract strings and counts

---

parsec

#### 426. Convert Binary Search Tree to Sorted Doubly Linked List

inorder traversal

#### [445. Add Two Numbers II](https://leetcode.com/problems/add-two-numbers-ii/)

Similar with [2. Add Two Numbers](https://leetcode.com/problems/add-two-numbers/); use a stack

#### 510. Inorder Successor in BST II

if curr has right child

---

if curr is left child, if curr is right child

#### 897. Increasing Order Search Tree

inorder traversal

#### 951. Flip Equivalent Binary Trees

just compare

#### 973. K Closest Points to Origin

quicksort

--- 

heap

#### 1086. High Five

pq of fixed size

---

partial sort

#### 1110. Delete Nodes And Return Forest

use a param to indicate if it will be a root

#### 1122. Relative Sort Array

sort

---

use a sort len of 1001 to store counts in arr1, loop arr2 to place to target arr, then result of the numbers in arr to target arr

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

#### 48. Rotate Image
