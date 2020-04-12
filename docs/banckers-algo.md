# Bankers' Algorithm 

银行家算法的核心思想是检查安全状态，假设某个进程 p 请求了某些资源，计算分配之后是否依然是安全状态，如果依然是，就允许分配，不然拒绝。


```
MAX is a n * m matrix, MAX_Matrix[i, j] 表示进程 i 总共需要多少资源 j 

Allocated is a n * m matrix, Allocated[i, j] 表示已经分配给了进程 i 多少资源 j

Resources is a vector of length m, Resources[i] 表示系统中还有有多少资源 i
```

通过这些数据可以计算出 `NEED = MAX - Allocated`

```
NEED is a n * m matrix, NEED[i, j] 表示进程 i 还需要多少资源 j
``` 

## Safe State 

如果当前的进程们能以某种顺序完成而不发生死锁，那么这个状态就是安全的。

从第一个未完成的进程开始到最后一个，重复：如果剩下的资源能满足满足当前的进程，那么则标记该进程为已完成，then free all the resources allocated to it. 

如果本轮循环后，所有进程都完成了，那么可以断定这是安全状态。
如果只有部分进程被满足了，则进行下一轮循环。
如果本轮循环中没有任何一个进程能被满足，那么该状态不安全。


```
let found = true
while found && unfinished 
    found = false
    for proc in unfinished 
        if avalable > need[proc] 
            found = true
            delete proc from unfinished
        
if unfinished == 0 
    return safe
else 
    return unsafe
    
<!-- 稍加修改此代码我们也能得出一个安全序列 -->
```

