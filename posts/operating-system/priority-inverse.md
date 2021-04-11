---
title: Operating System | Priority Inverse
---

在操作系统中，一般情况下，进程分优先级，高优先级进程需要执行时可打断现正在执行的低优先级进程；普通的临界资源使用方法，如果一个临界资源被获取了，则其它想要获取此资源的程序被阻塞，直到此资源被释放；

有三个进程（其优先级从高到低分别为 T1、T2、T3），有一个临界资源 CS（T1 与 T3 会用到）。这时，T3 先执行，获取了临界资源 CS。然后 T2 打断 T3。接着 T1 打断 T2，但由于 CS 已被 T3 获取，因此 T1 被阻塞，这样 T2 获得时间片。直到 T2 执行完毕后，T3 接着执行，其释放 CS 后，T1 才能获取 CS 并执行。这时，我们看 T1 与 T2，虽然 T1 优先级比 T2 高，但实际上 T2 优先于 T1 执行。这称之为优先级逆转。