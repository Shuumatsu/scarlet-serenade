** simple producer&consumer**：

```python
mutex = Semaphore (1) // protect buffer
items = Semaphore (0) // if the buffer is empty
```

consumer:
```python
items.wait()
mutex.wait()
    event = buffer.get()
mutex.signal()
```

producer
```python
event = waitForEvent()

mutex.wait()
    buffer.add(event)
mutex.signal()
items.signal()
```

any time you wait for a semaphore while holding a mutex, there is a danger of deadlock. 所以下面这个是错的，当 buffer 为空的时候，第一个到达的线程是 consumer 线程就会死锁

```python
mutex.wait()
items.wait()
    event = buffer.get()
mutex.signal()
```

**bounded producer&consumer**：

```
mutex = Semaphore(1) // protect buffer
items = Semaphore(0) // if the buffer is empty
cap = Semaphore(buffer.size()) // 额外使用一个 semaphore 用来记录当前 `buffer` 是否还有剩余容量
```

consumer:
```python
items.wait()
mutex.wait()
    event = buffer.get()
mutex.signal()
cap.signal()
```

producer:
```python
event = waitForEvent()
cap.wait()

mutex.wait()
    buffer.add(event)
mutex.signal()
items.signal()
```