---
title: Memory Consistency and Cache Coherence
---




---

Cache coherence 
- deals with ordering of writes to a single memory location
- only needed for systems with caches 

Memory consistency 
- deals with ordering of reads/writes to all memory locations
- needed in systems with or without caches

---

:::div{style="display: flex; justify-content: space-around"}
```
P0:
S1: datum = 5;
S2: datumIsReady = 1;
```

```
P1:
S3: while (!datumIsReady);
S4: … = datum 
```
:::

Program-order expectation: Programmers expect memory accesses in a thread to be executed in
the same order in which they occur in the source code. 
Atomicity expectation: A read or write happens instantaneously with respect to all processors.