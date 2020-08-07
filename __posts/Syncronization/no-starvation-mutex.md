多个线程等待于一个 semaphore 可能造成饥饿，例如下面的情况

Thread A、B、C
```python
loop:
    semaphore.acquire()
    // do sth
    semaphore.acquire()
```

根据 schduler 的行为，线程 A、B 可能交互执行而 C 可能永远不会被执行

造成饥饿的原因是，每次 scheduler 从等待于 `semaphore` 的线程池中选取一个执行。而池中线程的数量可能永远是复数个的（执行了一个又新加了几个这样），所以可能存在某个线程永远不会被执行到的情况。

所以如果我们能够保证 scheduler 总是从有限数量（有限意味着，随着时间/每次选择，池中的数量总是减小的）的池中选择线程执行，则不会发生饥饿现象。

设立一个有限大小的缓冲区，每次 scheduler 从缓冲区中选择一个线程执行。当缓冲区内的线程全部执行完毕后，将所有缓冲区外的线程移入缓冲区。
    - 这样的机制保证了每个缓冲区内的进程都能在有限时间内被执行，进而每一个到达的线程都能在有限时间内被移入缓冲区

这里定义的缓冲区的概念是对操作系统的 scheduler 而言的，所以对于用户而言是透明的。所以我们的实现是将线程的执行分为三个阶段：waiting, in_buffer, running。

用代码实现如下

```python
waiting_count = 0
waiting_count_mutex = Semaphore(1) # protects waiting_count

in_buffer_count = 0
buffer_mutex = Semaphore(1) # protects buffer / in_buffer_count

cs_mutex = Semaphore(1) # protects critical section


with waiting_count_mutex:
    waiting_count += 1

buffer_mutex.acquire()
in_buffer_count += 1
waiting_count_mutex.acquire()
waiting_count -= 1
if waiting_count == 0:
    waiting_count_mutex.release()
    # all waiting threads have been moved into buffer
    cs_mutex.release()
else:
    waiting_count_mutex.release()
    # continue until all waiting threads have been moved into buffer
    buffer_mutex.release()
            
cs_mutex.acquire()
in_buffer_count -= 1

# do sth

if in_buffer_count == 0:
    # move waiting threads into buffer
    buffer_mutex.release()
else:
    # continue until all threads in buffer complete
    cs_mutex.release()
```

