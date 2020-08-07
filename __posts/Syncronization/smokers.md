制作一根香烟需要三种材料：tobacco、paper、match。
```python
tobacco = Semaphore(0)
paper = Semaphore(0)
match = Semaphore(0)
```

有三个供应商分别源源不断的供应 (tobacco, paper)、(tobacco, match)、(paper, match)

```python
# A
tobacco.release()
paper.release()
# B
tobacco.release()
match.release()
# C
paper.release()
match.release()
```

有三个吸烟者有无限的需求，他们分别持有 match, paper, tobacco。当他们拿到缺的材料之后就开始吸烟。要求不修改供应商的代码，设计三个吸烟者线程的代码。

每个供应商刚好提供某一个吸烟者缺失的部分，可是却无法精确推送到目标吸烟者。所以设计一个中间层将提供的材料分配给吸烟者。
为 match, paper, tobacco 分别分配变量记录提供的数量
```python
tobacco_count = 0
paper_count = 0
match_count = 0

mutex = Semaphore(1) # protects counters
```

中间层使用三个线程实现，分别监听某一种材料。每当中间层检测到当前已有的材料满足某位吸烟者的时候，则唤醒该吸烟者。

proxy tobacco (另外两个同理
```python
tobacco.acquire()
with mutex:
    if paper_count > 1:
        paper_count -= 1
        smokers[match].release()
    else if match_count > 1:
        match_count -= 1
        smokers[paper].release()
    else:
        tobacco_count += 1
```
