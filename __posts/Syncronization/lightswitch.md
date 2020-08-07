第一个进教室的人（进入时教室为空）打开灯，最后一个离开教室的人（离开时教室为空）关灯

```python
class LightSwitch:
    def __init__(self):
        self.count = 0
        self.mutex = Semaphore(1)

    def lock(self, sema):
        self.mutex.acquire()
        self.count += 1
        if self.count == 1:
            sema.acquire()
        self.mutex.release()

    def release(self, sema):
        self.mutex.acquire()
        self.count -= 1
        if self.count == 0:
            sema.release()
        self.mutex.release()
```

仅第一个调用 `instance.lock` 的线程需要等待/获取锁。
仅最后一个调用 `instance.release` 的线程需要释放锁。

