---
title: Computer Architecture | RISC V | Concurrency
---

RISC V 有宽松的内存一致性模型（relaxed memory consistency model）因此其它线程看到的内存访问可以是乱序的
所有的 RV32A 指令都有一个请求位 aq 一个释放位 rl

RV32A 有 2 种类型的原子操作
- atomic memory operation
    - AMO 对内存执行一个原子操作，并将寄存器设置为操作前的内存值；原子表示内存读写之间的过程不会被打断，内存值不会被其它处理器修改
- load reserved / store conditional