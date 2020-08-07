**pipeline**: 



<img src="D:\learning-notes\Computer-Architecture&amp;Organization\basic.assets\image-20200618001346387.png"  style="zoom: 50%;" />


MEM stage: we could have a load instruction in which case what the ALU computed is really the address that we use to access the data memory and what comes out of data memory is what we end up writing to our registers.

对每个指令来说，所需要的时间是不变的，但是大大提高了吞吐率
例如西医需要三个步骤，分别需要一个小时，现在需要洗 10 件衣服，

    - 如果不使用 pipeline，则需要 30 小时
        - 如果使用 pipeline，最后一件一副需要 9 小时开始执行，再洗 3 小时，所以总共需要 12 小时

CPI： cycle per instruction
    使用 pipeline 的理想情况下，可看作 `CPI = 1`

**Stall**：因为某些原因，指令可能在某 stage 停留，其后的所有指令也得等待

|| Fetch | Decode / Read | ALU  | MEM  | WRITE |
| ----- | ----- | ------------- | ---- | ---- | ----- |
|C1| ... | ADD R2, R1, R1 | LW R1, 0 |      |       |
|C2| ... | ADD R2, R1, R1 |  | LW R1, 0 |       |
|C3| ... | ADD R2, R1, R1 |  |      | LW R1, 0 |

`ADD R2, R1, R1` 指令需要等待 `LW` 指令将内存中的值存入寄存器后才能计算



**FLUSH**: 
|| Fetch | Decode / Read | ALU  | MEM  | WRITE |
| ---- | ---- | ---- | ---- | ---- |
| C1   | ...  | JUMP |      |      |
| C2   | ...  | ...  | JUMP |      |
| C3   | ...  | X    | X    | JUMP |

在 C1 阶段还没有完成 `Decode` 我们并不知道这是 JMP 命令，所以直接 `fetch` 了下一条指令
在 C2 阶段还没有完成 `ALU` 我们并不知道程序应该跳转到哪里，所以直接 `fetch` 了下一条指令
在 C2 阶段完成后我们发现 `fetch` 了错误的指令，所以将错误的指令 flush 掉，在 C3 阶段根据跳转的结果 `fetch` 新的指令

**Control Dependence**: once we have a branch instructions, all following instructions have control dependence on it



**True Dependence**: RAW
**False Dependence**: WAW / WAR

```assembly
ADD R1, R2, R3 ; 1
ADD R1, R2, R3 ; 2
ADD R1, R2, R3 ; 3
```



判断的时候需要注意， 1 与 3 之间是没有 WAW 关系的，因为 1 的结果被 2 覆盖，所以对 3 来说 1 其实随便怎么样都无所谓

**Hazard**: dependence results incorrect execution

Handle Harzard:
    - Control Dependence: Flush
    - Data Dependence: 
      - Stall
      - Forward

Forward 会将 ALU  得到的结果推送到后面的指令 （我猜这是用一个 Async resetter 实现的，本来触发器内存储的是上次读取得到的值，现在被 forward 过来的值覆盖掉了



添加更多的 stage：
    - 更多 harzard
    - 每个 cycle 花费更少的时间


**Brach Prediction** 
    - if this instruction is a branch insitruction 
    - if this branch instruction will be taken 
    - whats the target pc 

wrong prediction penalty:
    1 + 指令预测错误的期望 * 每次预测错误的penalty
以 no-taken prediction 为例，假设是一个标准的5级流水线，在ALU阶段结束的时候搞清楚是否跳转，程序中有百分之20的分支语句，其中每个有百分之50的概率跳转
则 指令预测错误的期望  = 0.5 * 0.2
   每次预测错误的penalty = 2
则 wrong prediction penalty = 1 + 0.5 * 0.2 * 2


**refuse prediction**：当且仅当完全确定应该 fetch 哪条指令了才去fetch


**no-taken prediction**：前面提到的无论何时直接默认fetch pc+4 的指令的做法

**prediction benifitss**：
    如果采用 refuse prediction，
        非分支语句 需要到 decode 阶段结束才能去fetch吓一跳；成本为2
        分支语句 需要到 ALU 结束；成本为3
    如果采用 no-taken prediction：
        每个非分支语句，成本为 1
        分支语句 需要到 ALU 结束；
            - 发现错误：成本为 3
                        - 预测正确：成本为 1

History based prediction
使用一个 Branch Target Buffer 
    - key： program counter
        - value：next program counter
if finnaly the dedicated pc is not the same with what we get from thje table then it's treated as mipredicted, and we need to update the table 
如果是一个有 1024 个 entries 的表，则需要 10 位的二进制地址来索引，
每个 pc 我们取 LSB 10. 如果有地址都是 word aligned（4bytes），则丢弃最低两位后再取10位

register renaming
使用一个 regiter allocation table
这个 table 存储每个 R_i 对应的重命名后的寄存器名字
这样可以消除 false dependencies
    WAW：他们都写入到了不同的寄存器名字上，则没有影响
    WAR：写入的寄存器名字与先前读入的名字不同，则没有影响

<img src="D:\learning-notes\Computer-Architecture&amp;Organization\basic.assets\image-20200618005400208.png" alt="image-20200618005400208" style="zoom: 50%;" />


**ILP**: IPC when 
    - processor does entire instruction in one cycle
        - processor can do any number of instructions in one cycle
        - still need to follow True Dependencies

| INSTRUCTIONS   | Cycle - 1 | Cycle - 2 | Cycle - 3 | Cycle - 4 |
| -------------- | --------- | --------- | --------- | --------- |
| ADD R1, R1, R1 | X         |           |           |           |
| ADD R2, R2, R1 |           | X         |           |           |
| ADD R3, R2, R1 |           |           | X         |           |
| ADD R6, R7, R8 | X         |           |           |           |
| ADD R8, R3, R7 |           |           |           | X         |
| ADD R1, R1, R1 |           | X         |           |           |
| ADD R1, R7, R7 |           | X         |           |           |
| `ILP = 7 / 4`  |           |           |           |           |

计算 ILP：找到最长的 true dependence 依赖链



**ILP with Control Dependencies**
    - no structural dependencies (physical limitations, such as only limited number of ALUs available
    - perfect same-cycle branch prediction
```assembly
    ADD R1, R2, R3
    MUL R1, R1, R1
    BNE R5, R1, Label ; taken branch
    ADD R5 R1, R2

Label:
    MUL R5, R7, R8
```
甚至在开始执行分支语句之前我们就已经知道 `Label` 处的语句会执行



**comparing ILP & IPC**
given processor:
    - 2-issue: means handle 2 insts per cycle
      - 1 MUL
      - 1 ADD/SUB/XOR
    - out of order

```assembly
ADD R1, R2, R3 ; 1
SUB R4, R1, R5 ; 2
XOR R6, R7, R8 ; 3
MUL R5, R8, R9 ; 4
ADD R4, R8, R9 ; 5
```

| INSTRUCTIONS   | Cycle - 1 | Cycle - 2 | Cycle - 3 | Cycle - 4 |
| -------------- | --------- | --------- | --------- | --------- |
| ADD R1, R2, R3 | X         |           |           |           |
| SUB R4, R1, R5 |           | X         |           |           |
| XOR R6, R7, R8 |           |           | x         |           |
| MUL R5, R8, R9 | X         |           |           |           |
| ADD R4, R8, R9 |           |           |           | x         |
`IPC = 7 / 4`

calc ILP: 只需要关注 True Dependence (RAW)
    - `1 -> 2`
则 `ILC = 5 / 2`