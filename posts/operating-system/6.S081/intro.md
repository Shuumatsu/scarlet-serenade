---
title: Operating System | 6.S081 | Intro
---

```
sudo apt-get install git build-essential gdb-multiarch qemu-system-misc gcc-riscv64-linux-gnu binutils-riscv64-linux-gnu 

git clone git://g.csail.mit.edu/xv6-labs-2020
```

---

You can run `make grade` to test your solutions with the grading program. If you want to run the grade tests for one assignment, type `./grade-lab-util [lab name]`

---

在这门课程中，我们主要关注点在 Kernel、连接 Kernal 和用户空间程序的接口、Kernel 内软件的架构。所以，我们会关心 Kernel 中的服务，其中一个服务是文件系统，另一个就是进程管理系统。每一个用户空间程序都被称为一个进程，它们有自己的内存和共享的 CPU 时间。同时，Kernel 会管理内存的分配。不同的进程需要不同数量的内存，Kernel 会复用内存、划分内存，并为所有的进程分配内存。

---

each process has two stacks: a user stack and a kernel stack. when executing user instructions, user stack is in use and kernel stack is empty. when entered kernel, kernel code executes on kernel stack while user stack is saved but not used.