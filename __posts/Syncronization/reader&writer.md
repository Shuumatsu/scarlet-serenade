**reader preference**: writer may starve 

```python
reader_switch = LightSwtich() # reader 的逻辑可用 `LightSwitch` 实现，第一个 `reader` 加锁，最后一个完成的 `reader` 解锁
cs = Semaphore(1) # protects critical section
```

writer
```python
cs.acquire()
# do sth
cs.release()
```

```python
reader_switch.acquire(cs)
# do sth 
reader_switch.release(cs)
```

---

**no starvation impl**：when a writer arrives(acquires the turnstile semaphore), the existing readers can finish, but no additional readers may enter.
    （经常用某个线程拿到第一个 semaphore 表示其到达，实现一个按到达顺序执行的读写锁的时候也是这样判定的

```python
reader_switch = LightSwtich() 
cs = Semaphore(1) 
turnstile = Semaphore(1) # 拿到 `turnstile.acquire()` 的线程即是下一个进入的线程
```

writer
```python
turnstile.acquire() # no additional readers may enter
cs.acquire()
# do sth
turnstile.release() # 当且仅当他完成后，才能有 `reader` 开始工作
cs.release()
```

reader
```python
turnstile.acquire() # turnstile 被 writer 进程拿到，则必须等到该 writer 完成任务
turnstile.release() # reader 拿到后立即释放，则其它 reader 可紧接着进入，或者被某个 writer 拿到

# below is the same with `reader preference` impl
reader_switch.acquire(cs)
# do sth 
reader_switch.release(cs)
```

---

**writer preference impl**：一旦有 writer 正在等待，就不能有 reader 开始执行。那么用一个锁控制是否 reader 可以执行，第一个到达的 writer 加锁，最后一个完成的 writer 解锁。这其实就是 `LightSwitch` 的逻辑

```python
allow_reader_mutex = Semaphore(1)
allow_reader_switch = LightSwtich() 

reader_switch = LightSwtich() 
cs = Semaphore(1) 
```

writer 
```python
allow_reader_switch.acquire(allow_reader_mutex)
cs.acquire()
# do sth
cs.release()
allow_reader_switch.release(allow_reader_mutex)
```

reader 
```python
# reader 仅在获取许可后尝试进入 critical section
allow_reader_mutex.acquire() 
# 如果获取后立即释放 allow_reader_mutex，考虑以下情形
# 在下一行竞争 critical section 失败，某个 writer 开始执行，并且执行期间来了新的 writer
# 在新的 writer 执行完毕之前，这个 reader 是不应执行的
# 但是他已经经过了 allow_reader_mutex，进而会去和新的 writer 竞争
reader_switch.acquire(cs)
allow_reader_mutex.release() # 因为 reader 并不阻止其它 reader，所以 critical section 内的 reader 不应持有 allow_reader_mutex
# do sth 
reader_switch.release(cs)
```