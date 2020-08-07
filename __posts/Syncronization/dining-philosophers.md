> https://leetcode.com/problems/the-dining-philosophers/

1. 限制同时用餐的 philosophers 的个数，如果最多同时只有 4 位用餐，则不可能发生死锁 
```python
class DiningPhilosophers:
    def __init__(self):
        self.sema = Semaphore(4)
        self.forks = [Semaphore(1) for _ in range(0, 5)]

    # call the functions directly to execute, for example, eat()
    def wantsToEat(self, philosopher: int, pickLeftFork: 'Callable[[], None]',
                   pickRightFork: 'Callable[[], None]',
                   eat: 'Callable[[], None]',
                   putLeftFork: 'Callable[[], None]',
                   putRightFork: 'Callable[[], None]') -> None:
        left = philosopher
        right = (philosopher + 1) % 5
        with self.sema:
            self.forks[left].acquire()
            self.forks[right].acquire()
            pickLeftFork()
            pickRightFork()
            eat()
            putLeftFork()
            putRightFork()
            self.forks[left].release()
            self.forks[right].release()
```

2. 给每个资源编号，每个进程只能以固定的顺序获取资源，这样的话不可能有循环等待，也就不会死锁

如果将每个哲学家左边的叉编为和哲学家一样的编号，则对于 0、1、2、3 哲学家而言，就是依次取左边右边的叉，对 4 而言，则是先取右边再取左边

```python
class DiningPhilosophers:
    def __init__(self):
        self.forks = [Semaphore(1) for _ in range(0, 5)]

    # call the functions directly to execute, for example, eat()
    def wantsToEat(self, philosopher: int, pickLeftFork: 'Callable[[], None]',
                   pickRightFork: 'Callable[[], None]',
                   eat: 'Callable[[], None]',
                   putLeftFork: 'Callable[[], None]',
                   putRightFork: 'Callable[[], None]') -> None:
        left = philosopher
        right = (philosopher + 1) % 5
        if philosopher == 4:
            left, right = right, left

        self.forks[left].acquire()
        self.forks[right].acquire()
        pickLeftFork()
        pickRightFork()
        eat()
        putLeftFork()
        putRightFork()
        self.forks[left].release()
        self.forks[right].release()
```

