## using existing instructions - Lamport Bakery’s algorithm

面包店一次只能接待一位顾客的采购。已知有 n 位顾客要进入面包店采购，按照次序安排他们在前台登记一个签到号码。该签到号码逐次增加 1。顾客根据签到号码的由小到大的顺序依次入店购货。完成购买的顾客在前台把其签到号码归 0。 如果完成购买的顾客要再次进店购买，就必须重新排队。
这就类似于线程排队进入临界区，因为是并发的情况，可能两个线程获取到了相同的号码，这时 pid 小的进程有更高的优先级

```c++
// declaration and initial values of global variables
Entering : array[1..NUM_THREADS] of bool = {};
Number : array[1..NUM_THREADS] of integer = {};
lock(integer i) {
    Entering[i] = true;
    Number[i] = +max(Number[], ..., Number[NUM_THREADS]);
    Entering[i] = false;
    for (j = ; j <= NUM_THREADS; j++) {
        // Wait until thread j receives its number:
        while (Entering[j]) { /* nothing */
        }
        // Wait until all threads with smaller numbers or with the same
        // number, but with higher priority, finish their work:
        while ((Number[j] != 0) &&
               ((Number[j], j) < (Number[i], i))) { /* nothing */
        }
    }
}
unlock(integer i) { Number[i] = ; }

Thread(integer i) {
    while (true) {
        lock(i);
        // The critical section goes here...
        unlock(i);
        // non-critical section...
    }
}
```

每个线程只写它自己的 `Entering[i]`、`Number[i]`，只读取其它线程的这两个数据项。所以这两个数组是安全的。

`Entering` 是必须的，假如不使用的话，假设进程 `i < j` 二者都将被分配到相同的序号（可能进程 `j` 在 `Number[i] = +max(Number[], ..., Number[NUM_THREADS])`的途中插入），这时 `j` 读取到的 `Number[i]` 为 0，因此进入了临界区，随后 `i` 获得时间片，因为 `i < j` 同样进入临界区，违背了 mutual exclusion

## Using 3 main types of Atomic instructions

### Atomic Exchange

```assembly
EXCH R1, 78(R2)
```

It does both load and write at the same time, so that it swaps the content of R1 with the contents of the memory location.

We can use EXCH to implement our lock, it repeatedly tries to acquire the lock, when it succeeds, R1 will be 0

```c++
R1 = 1;
while (R1 == 1) { EXCH R1, lock_var; }
```

The problem is that, it writes all the time even while the lock is busy, thus making the shared bus busy.

### Test-And-Set

The test-and-set instruction atomically checks if a memory location is zero, and if so, sets the memory location to 1. If the memory location is 1, it does nothing. It returns the old value of the memory location.

The lock operation is implemented as:

```
while (test - and-set(l) == 1);
```



**Atomic Read/Write in same instruction**
The problem is that it is bad for pipelining, consider our classic 5-stages pipeline.

Fetch | Decode/Read | Compute Memory address | Memory access | Write

It is not realistic to do all it needs in one cycle without really complicating the memory stage a lot, because loads and stores do either a read or a write, but a read/write instruction needs to do both.


### Load-Link/Store-Conditional (LL/SC)

**Load Linked**: Load memory location into register and mark it as loaded by this processor. A memory location can be marked as loaded by more than one processor.
**Store Conditional**: Check if address same as in link register. If yes, store the new value and remove all marks from the memory location. Return whether or not the store succeeded.
The store conditional fails if another core has already done SC

Here is how to use LL/SC to implement the lock operation:

```c++
while (1) {
    LL r lock;
    if (r == 0) {
        if (SC 1 lock) { break; }
    }
}
```

If the code is simple, locks are not needed, the LL/SC can be used directly.  

```c++
while (1) {
    LL r1, lock;
    ADDI r1, 1, r1;
    if (SC r2, lock) break;
}
```

The implementation of these spin locks is unfair, they cannot guarantee that the longest waiting thread will get the lock first. Can make it even better by having a queue lock that queues up the waiting threads and gives the lock to the first thread in the queue

## Busy Waiting

Spin lock is busy waiting. So, on a uniprocessor if cannot get the thread the first time, should just suspend. So, lock acquisition looks like this:
```
while (test - and-set(l) == 1) { current_thread->yield(); }
```

On a multiprocessor. Because there is a cost to suspending and resuming the process. Maybe should spin just a little while, in hopes that other process will release lock.
There are three components of the cost: spinning, suspending and resuming.

The optimal algorithm is as follows:
    - If the lock will be free in less that the suspend and resume time, then spin until acquire the lock
    - If the lock will be free in more than the suspend and resume time, then suspend immediately
Obviously, cannot implement this algorithm - it requires knowledge of the future, which we do not in general have.

How do we evaluate practical algorithms? - algorithms that spin for a while, then suspend.
    We compare them with the optimal algorithm in the worst case for the practical algorithm

SR algorithm: spins for the suspend and resume time, then suspends.
	The worst case is when lock becomes free just after start the suspend. The optimal algorithm takes the suspend and resume time to acquire the lock. The SR algorithm costs twice the suspend and resume time.

---

What about other algorithms that spin for a different fixed amount of time then block? All are worse than the SR algorithm.
（这个比较我感觉就很怪，拿自己的最坏情况去和别人非最坏情况比，就以 LT-SR 为例，在 SR 的最坏的情况下，LT-SR 的成本不是更低吗 
    - LT-SR algorithm: spin for less than suspend and resume time. The worst case is when lock becomes free just after starting the suspend. In this case, the SR algorithm will just cost the spinning time.
    - GT-SR algorithm: spin for more than suspend and resume time. The worst case is when lock becomes free just after starting the suspend. In this case the SR algorithm will also suspend and resume, but it will spin for less time than the GT-SR algorithm


> https://people.csail.mit.edu/rinard/teaching/osnotes/h5.html
