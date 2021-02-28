---
title: Computer Architecture | RISC V | Basics
---


全局变量：保存在数据段 .data 和 .bss 中，某些情况下 gp (x3) 寄存器保存两个数据段中间的一个位置，于是全局变量是基于 gp 加上一个偏移量来访问的。\
我们可以通过 链接脚本 (Linker Script) 调整链接器的行为，使得最终生成的可执行文件的内存布局符合我们的预期。

---

https://riscv.org/wp-content/uploads/2018/05/08.45-09.10-RISCV-20180509-FastInts.pdf
https://sifive.cdn.prismic.io/sifive/0d163928-2128-42be-a75a-464df65e04e0_sifive-interrupt-cookbook.pdf

---

- Software threads are threads of execution managed by the operating system.
- Hardware threads are a feature of some processors that allow better utilisation of the processor under some circumstances. \
They may be exposed to/by the operating system as appearing to be additional cores ("hyperthreading").

One hardware thread can run many software threads. （time-slicing

---

RV32I 有 31 个通用寄存器和一个恒为 0 的 X0 寄存器；在 RISC V 中，PC 不是通用寄存器。

6 种基本指令格式
- R 型：用于 register - register 
- I 型：用于短立即数和访存 Load 操作 （有 12 位用于 Imm
- U 型：用于长立即数 （有 20 位用于 Imm
    - (RISC V 指令中所有的立即数总是符号扩展
- S 型：用于访存 Store 操作
- B 型：用于条件跳转操作
- J 型：用于无条件跳转

所有位全是 1 或 全是 0 的指令是非法指令

---

加载与存储仅支持唯一的位置模式：位偏移寻址模式，即符号扩展 12 位立即数到基地址寄存器

lui 将 20 位常量加载到寄存器的高 20 位; auipc 将 20 位常量加载到 PC 的高 20 位
- e.g., 用 lui 加载立即数到寄存器的高 20 位，再用一般的立即数指令弄低 12 位可构造出构造大的常量
- e.g., 用 auipc 的 20 位与 jalr 的 12 位立即数的组合可以将执行流跳转到任何 32 位 PC 相对位置

RV32I 基于比较两个寄存器的结果进行分支跳转（有的 architecture 用控制位记录溢出等，并根据此进行跳转
由于 RISC V 的指令长度必须是 2 字节的倍数，所以分支指令的寻址方式是 12 位立即数乘 2 后符号扩展再加到 PC 上作为 target addr

---

RISC V 提供三种特权模式：user mode, supervisor mode 以及 machine mode。

machine mode 是硬件线程可以执行的最高特权模式，对内存， I/O 等底层功能有完全的控制权。machine mode 也是硬件 reset 后首先进入的模式。

---

试图访问一个不存在的 CSR 会引起一个非法指令异常。试图在没有适当权限级别的情况下访问一个 CSR 或写入一个只读寄存器也会产生非法指令异常。读/写寄存器也可能包含一些只读的位，在这种情况下，对只读位的写入将被忽略。

RISC V ISA 采用 12-bit encoding space for up to 4096 CSRs. 惯例上，最高两位用来表示在指定 privilege level 下的读写权限，次高两位表示该 CSR 要求的最低 privilege level

### CSR Field Specification

- Reserved Writes Preserve Values, Reads Ignore Values (WPRI
    - 有些 fields 被保留 for future use。软件应忽略从中读取到的值并且在写入寄存器时不改变其值
- Write/Read Only Legal Values (WLRL
    - 对某些 fields 来说，只有部分 bit encoding 是有效的。软件不应向这些 fields 中写入非法的值并且不应假定读出的值是合法的除非上次写入的是一个有效的值
- Write Any Values, Read Legal Values (WARL
    -  对某些 fields 来说，只有部分 bit encoding 是有效的。允许向其中写入任意值并且保证任意时候读出的值都是合法的
        - 若上次写入的值是不合法的，则根据实现，可能返回任意的一个合法的值
        - 若上次写入的值是合法的，则保证返回该合法值