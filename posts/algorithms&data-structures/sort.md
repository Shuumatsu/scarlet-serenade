---
title: Algorithms & Data Structures | Sort
---

## Selection Sort

```ts
function sortArray(nums: number[]): number[] {
    for (let i = 0; i < nums.length; i += 1) {
        let min_idx = i
        let min_val = nums[i]
        for (let j = i + 1; j < nums.length; j += 1) {
            if (nums[j] < min_val) {
                min_val = nums[j]
                min_idx = j
            }
        }
        [nums[i], nums[min_idx]] = [nums[min_idx], nums[i]]
    }

    return nums
}
```

## Bubble Sort 

```ts
function sortArray(nums: number[]): number[] {
    for (let i = 0; i < nums.length; i += 1) {
        for (let j = i + 1; j < nums.length; j += 1) {
            if (nums[j] < nums[i]) {
                [nums[i], nums[j]] = [nums[j], nums[i]]
            }
        }
    }

    return nums
}
```

## Insertion Sort

```ts
function sortArray(nums: number[]): number[] {
    for (let i = 1; i < nums.length; i += 1) {
        // currently placed position
        let curr = i
        while (curr >= 1 && nums[curr - 1] > nums[curr]) {
            [nums[curr], nums[curr - 1]] = [nums[curr - 1], nums[curr]]
            curr -= 1
        }
    }

    return nums
}
```

## Quick Sort

```ts
function sortArray(nums: number[]): number[] {
    const partition = (i: number, j: number): number => {
        let p = i
        for (let k = i; k < j; k += 1) {
            if (nums[k] < nums[j]) {
                ;[nums[k], nums[p]] = [nums[p], nums[k]]
                p += 1
            }
        }
        ;[nums[p], nums[j]] = [nums[j], nums[p]]

        return p
    }
    const quicksort = (i: number, j: number) => {
        if (i >= j) return

        const p = partition(i, j)
        quicksort(i, p - 1)
        quicksort(p + 1, j)
    }

    quicksort(0, nums.length - 1)
    return nums
}
```

### Three-way Version

```ts
function sortArray(nums: number[]): number[] {
    const partition = (i: number, j: number): [number, number] => {
        const pivot = nums[i]

        let mut less = i;
        let mut greater = j;
        let mut curr = i;
        // `nums[0..less]`: all less than pivot
        // `nums[less..curr]`: all equal to pivot
        // `nums[curr..=greater]`: unknown
        // `nums[greater..]`: greater than pivot
        while curr <= greater {
            match nums[curr].cmp(&pivot) {
                Ordering::Equal => curr += 1,
                Ordering::Less => {
                    nums.swap(less, curr);
                    less += 1;
                    // `nums[less]` is originally within `nums[less..curr]`
                    // so `num[curr]` now is equal to pivot, so we advance `curr`
                    curr += 1;
                }
                Ordering::Greater => {
                    nums.swap(greater, curr);
                    greater -= 1;
                    // `nums[curr]` is unknown now, so we don't advance `curr`
                }
            }
        }

        return [less - 1, greater + 1]
    }
    const quicksort = (i: number, j: number) => {
        if (i >= j) return

        const [a, b] = partition(i, j)
        quicksort(i, a)
        quicksort(b, j)
    }

    quicksort(0, nums.length - 1)
    return nums
}

```

## Heap Sort

```ts
const down = (nums: number[], bound: number, i: number) => {
    const left_child = 2 * i + 1
    const right_child = Math.min(bound, 2 * i + 2)
    if (left_child > bound) return

    const next = nums[left_child] > nums[right_child] ? left_child : right_child
    if (nums[i] < nums[next]) {
        ;[nums[i], nums[next]] = [nums[next], nums[i]]
        down(nums, bound, next)
    }
}

const heapify = (nums: number[]): void => {
    for (let i = Math.floor(nums.length / 2); i >= 0; i -= 1) {
        down(nums, nums.length - 1, i)
    }
}

function sortArray(nums: number[]): number[] {
    heapify(nums)
    for (let p = nums.length - 1; p > 0; p -= 1) {
        ;[nums[0], nums[p]] = [nums[p], nums[0]]
        down(nums, p - 1, 0)
    }
    return nums
}
```

## Merge Sort

### Top-Bottom Approach

```ts
function sortArray(nums: number[]): number[] {
    const merge_sort = (i: number, j: number) => {
        if (i >= j) return

        const m = Math.floor((i + j) / 2)
        merge_sort(i, m)
        merge_sort(m + 1, j)

        const temp: number[] = []
        let a = i,
            b = m + 1
        while (a <= m && b <= j) {
            if (nums[a] <= nums[b]) {
                temp.push(nums[a])
                a += 1
            } else {
                temp.push(nums[b])
                b += 1
            }
        }
        while (a <= m) {
            temp.push(nums[a])
            a += 1
        }
        while (b <= j) {
            temp.push(nums[b])
            b += 1
        }

        for (let curr = 0; curr < temp.length; curr += 1) {
            nums[i + curr] = temp[curr]
        }
    }
    merge_sort(0, nums.length - 1)

    return nums
}
```

### Bottom-Top Approach

```ts
function sortArray(nums: number[]): number[] {
    const merge = (start: number, radius: number) => {
        const temp: number[] = []

        const second_start = start + radius,
            end = Math.min(nums.length - 1, start + 2 * radius - 1)
        if (second_start >= nums.length) return

        let i = start,
            j = second_start

        while (i < second_start && j <= end) {
            if (nums[i] <= nums[j]) {
                temp.push(nums[i])
                i += 1
            } else {
                temp.push(nums[j])
                j += 1
            }
        }
        while (i < second_start) {
            temp.push(nums[i])
            i += 1
        }
        while (j <= end) {
            temp.push(nums[j])
            j += 1
        }

        for (let curr = 0; curr < temp.length; curr += 1) {
            nums[start + curr] = temp[curr]
        }
    }

    for (let radius = 1; radius <= nums.length; radius *= 2) {
        for (let from = 0; from < nums.length; from += 2 * radius) {
            merge(from, radius)
        }
    }

    return nums
}
```