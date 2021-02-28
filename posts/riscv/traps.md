---
title: 'Computer Architecture | RISC V | Traps: Exceptions & Interrupts'
---

machine mode 最重要的特性是控制和处理异常；RISC V 的异常 (exception) 分为两类
- 同步异常，再指令执行期间产生
- 中断 interrupt ，与指令异步的外部事件；有三种标准中断源：
    - 软件：通过像内存映射寄存器种存数并通常用一个 hart 来中断另一个 hart
    - 时钟：当实时计数器 mtime 大于 hart 的时间比较器（一个名为 mtimecmp 的内存映射寄存器）时触发时钟中断
    - 外部来源：由平台级中断控制器引发（大部分外部设备连接到这个中断控制器）

RISC V 的异常 are precise：所有异常前的指令已完全执行 && 所有异常后的指令还未开始执行

---

增加特权级别的陷阱被称为垂直陷阱，而保持在相同特权级别的陷阱被称为水平陷阱。Traps never transition from a more-privileged mode to a less-privileged mode.

委托中断会导致中断在委托人权限级别被屏蔽。例如，如果通过设置 `mideleg[5]` 将 supervisor timer interrupt (STI) 委托给 S 模式，那么在 M 模式下执行时将不会采取 STI。相反，如果 `mideleg[5]` is clear，STI可以在任何模式下进行，并且无论当前模式如何，都会将控制权转移到M模式下。

---

### Interrupts

CLINT 有一个固定的优先级方案，实现了软件、定时器和外部中断。\
当处于 Supervisor 模式时，Machine 模式的中断将立即获得优先权，并抢占 Supervisor 模式的操作。CLINT不支持在特权级别内进行抢占。

#### Local Interrupts

- Directly connected to one hart
- No arbitration between harts to service
- Determine source directly through xcause CSR
- Only two starndard local interrupts (software, timer)

中断ID代表每个中断的固定优先级值，不可配置。

##### Software interrupts

软件中断。是一条 CPU 指令，用以自陷一个中断。由于软中断指令通常要运行一个切换 CPU 至内核态（Kernel Mode/Ring 0）的子例程，它常被用作实现系统调用（System call）。\
软件中断通过写入 memory-mapped interrupt pending register msip for particular CPU 触发。在多CPU系统中，其他CPU能够写入 msip 来触发系统中任何其他 CPU 的软件中断。这样可以实现处理器间的高效通信。

#### Timer interrupts

当内存映射寄存器 mtime 大于或等于全局时间基寄存器 mtimecmp 时，就会触发定时器中断，and both registers are part of the CLINT and CLIC memory map. 在多CPU系统中，mtimecmp 可以被其他CPU写入来设置定时器中断。

#### Global (External) Interrupts

- Routed via Platform-Level Interrupt Controller (PLIC)
- PLIC arbitrates between multiple harts claiming interrupt
- Read of memory-mapped register returns source

The global interrupt controller is termed the Platform Local Interrupt Controller (PLIC). PLIC 用于管理所有的全局中断，并将它们路由到一个或多个 CPU 中。通过 PLIC 路由的全局中断通过一个带有专用中断 ID 的单一中断连接到达 CPU。

每个全局中断在 PLIC memory map 中都有一个可编程的优先级寄存器。\
最多有 1024 个可用的中断被路由到 PLIC，这些中断按 1 到 1024 的顺序编号。进入PLIC的每个中断都有一个可配置的优先级，从1-7，7为最高优先级。值为0表示不中断，有效地禁用该中断。\
PLIC内有一个全局阈值寄存器，允许配置低于一定水平的中断被阻断。例如，如果阈值寄存器包含的值为5，则所有配置优先级为1到5的PLIC中断将不允许传播到CPU。如果具有相同优先级的全局中断同时到达PLIC，则优先考虑两个中断ID中较低的一个。

进入PLIC处理程序后，a CPU reads the claim register to acquire the interrupt ID. \
申领成功后，将原子地清除PLIC中断待处理寄存器中的待处理位，向系统发出中断正在被服务的信号。即使在挂起位未被设置时，CPU试图申请中断是合法的。例如，当一个全局中断被路由到多个CPU时，一个CPU已经在另一个CPU尝试声称中断之前声称了该中断，这种情况可能会发生。\
在用MRET指令（或SRET/URET）退出PLIC处理程序之前，用处理程序进入时获得的 non zero claim/complete value 将 claim/complete register 写回

### Machine Trap Setup

- misa: MRW - ISA and extensions

- mstatus: MRW - Machine status register.
    - mstatus 寄存器跟踪并控制着 hart 的当前工作状态。mstatus 寄存器的限制视图分别以S级和U级ISA中的 sstatus 和 ustatus 寄存器的形式出现。
    - 每个特权模式提供了全局中断启用位，MIE、SIE 和 UIE。这些位主要用于保证当前特权模式下中断处理程序的原子性。
        - 当 hart 在特权模式 x 下执行时，当 xIE=1 时，中断全局启用，当 xIE=0 时，中断全局禁用。
            - 低特权模式 w < x 的中断，无论低特权模式全局 wIE 位的设置如何，中断总是全局禁用。
            - 高权限模式 y > x 的中断，无论高权限模式全局 yIE 位的设置如何，总是全局启用。
                - 高权限级别的代码可以使用单独的每个中断使能位，在将控制权让渡给低权限模式之前，禁用选定的高权限模式中断。
    - 为了支持 nested traps，每个 privilege mode x 都有 two-level stack of interrupt-enable bits and privilege modes:
        - xPIE 记录当前 trap 之前的 interrupt-enable bit
        - xPP 记录当前 trap 之前的 privilege mode; xPP 字段只能保存 x 以内的特权模式，所以 MPP 为 2 位宽，SPP 为 1 位宽，UPP 隐含为 0。
        - 当一个陷阱从特权模式 y 进入特权模式 x 时，xPIE 被设置为 xIE 的值；xIE 被设置为 0；xPP 被设置为 y。
    - MRET、SRET 或 URET 指令分别用于从 M 模式、S 模式或 U 模式的陷阱中返回。
        - 当执行 xRET 指令时，假设 xPP 持有值 y，则 xIE 设置为 xPIE；特权模式改为y；xPIE 设置为1；xPP 设置为 U（如果不支持用户模式，则为M）。
    - 对于较低特权模式，任何 trap （同步或异步）通常在较高特权模式下处理，进入时禁用中断。

- mtvec: MRW - Machine trap handler base address.
    - mtvec 寄存器是一个 MXLEN 位的读/写寄存器，存放陷阱向量配置，由向量基地址（BASE）和向量模式（MODE）组成。
        - BASE 字段中的值必须始终在 4 个字节的边界上对齐，MODE 的设置可以对 BASE 字段中的值施加额外的对齐限制。
        - 当 MODE=Direct 时，所有进入机器模式的陷阱都会导致 PC 被设置为 BASE 字段中的地址。
        - 当 MODE=Vectored 时，所有进入机器模式的同步异常都会导致 PC 被设置为 BASE 字段中的地址，而中断则会导致 PC 被设置为 BASE 字段中的地址加上四倍的中断原因号。

- medeleg: MRW - Machine exception delegation register.
- mideleg: MRW - Machine interrupt delegation register.
    - 默认情况下，任何特权级别的所有陷阱都是在机器模式下处理的，机器模式处理程序可以通过MRET指令将陷阱重定向回适当的级别。
    - medeleg 和 mideleg 中提供的读/写位指示某些异常和中断应该由较低的特权级别直接处理。
    - 在具有三种特权模式 (M/S/U) 的系统中，设置 medeleg 或 mideleg 中的一个位将把 S 模式或 U 模式下的相应陷阱委托给S模式陷阱处理程序。
        - 如果支持U模式陷阱，S 模式可以过来设置 sedeleg 和 sideleg 寄存器中的相应位，将 U 模式下发生的陷阱委托给 U 模式陷阱处理程序。
    
    - 当一个陷阱委托给一个权限较小的模式x时，
        - xcause 寄存器写入陷阱原因
        - xepc 寄存器写入采取陷阱的指令的虚拟地址
        - xtval 寄存器写入异常特定的数据元
        - mstatus 的 xPP 字段写入陷阱时的活动权限模式，xPIE 字段写入陷阱时的 xIE 字段的值

    - 在权限较低的模式下，有些异常情况不能发生，相应的位应硬接为零。尤其是 medeleg[11] 和 sedeleg[11:9] 都要硬接线为零。

- mip: 
- mie: MRW - Machine interrupt-enable register.
    - mip 寄存器是一个 MXLEN 位的读/写寄存器，包含了待处理的中断信息，而 mie 则是相应的 MXLEN 位的读/写寄存器，包含了中断使能位。
    - 只有mip中的低权限软件中断（USIP、SSIP）、定时器中断（UTIP、STIP）和外部中断（UEIP、SEIP）所对应的位可以通过这个CSR地址写入，其余的位都是只读的。
    - mip 和 mie 寄存器的限制视图在 S 模式和 U 模式下分别以 sip/sie、uip/uie 寄存器的形式出现。如果通过设置 mideleg 寄存器中的位将中断委托给特权模式 x，则该中断在 xip 寄存器中变得可见，并可使用 xie 寄存器进行屏蔽。否则，xip 和 xie 中的相应位似乎被硬接为零。

    - MTIP、STIP、UTIP 位分别为对应模式的分别定时器中断的定时器中断待处理位。
        - MTIP 位是只读的，通过写入内存映射的机器模式定时器比较寄存器来清除。
        - UTIP 和 STIP 位可由 M 模式软件写入，以将定时器中断传递给较低的权限级别。
        - User and supervisor software may clear the UTIP and STIP bits with calls to the AEE and SEE respectively.
    - 有一个单独的定时器中断使能位，分别命名为MTIE、STIE和UTIE，用于M模式、S模式和U模式定时器中断。

    - mip 中的 MEIP 字段是一个只读位由特定平台的中断控制器设置和清除。SEIP 可以由 M 模式软件写入，向 S 模式指示外部中断正在等待。(P31

    - 不同特权模式的多个并发中断按所定特权模式的递减顺序处理。同一特权模式下的多个并发中断按以下优先级递减的顺序处理：MEI、MSI、MTI、SEI、SSI、STI、UEI、USI、USI、STI。MEI、MSI、MTI、SEI、SSI、STI、UEI、USI、UTI。同步异常的优先级低于所有的中断。

## Machine Trap Handling

- mscratch: SRW - Scratch register for machine trap handlers.
- mepc: SRW - Machine exception program counter.
- mcause: SRW - Machine trap cause.
- mtval: SRW - Machine bad address or instruction
- mip: SRW - Machine interrupt pending.