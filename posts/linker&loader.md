---
title: Linker & Loader
---

在最以前的时候，当我们想要跳转到，例如第五条指令，我们用跳转指令 `jump 5`。
这样做的弊端是，程序并不是写好后就不变的，流过我们只之后在目标指令前插入了别的指令，我们的跳转指令就指向了错误的地址。程序员需要手工重新计算跳转的目标地址并重写跳转指令。这个重新计算地址的过程被称为 **Relocation**。
在后面我们可以看到，在 Linker 被发明后，**Relocation** 由 Linker 负责。

---

后来我们用 **Symbol** 表示跳转的目标，我们将第五条指令开始的子程序明明为 `foo`

```
jump foo
```

这样不管以后我们在 `foo` 之前或之后插入或减少了多少条指令我们都不需要手动修改跳转指令。汇编器会为我们计算 `foo` 符号的地址吗，然后把所有引用到 `foo` 的指令修正到正确的地址。

---

再之后，现代软件的开发的规模往往都很大，我们将源码放在不同的模块中，这些模块相互依赖又相互独立。
我们需要解决不同模块间的通信问题，即模块间的函数调用和模块间的变量访问。函数访问需要知道目标函数的地址，变量访问需要知道目标变量的地址，所以通信问题可以归结为模块间的符号引用问题。
模块拼接的过程被称为 **Linking**。

链接的主要过程包括了 **Address and Storage Allocation**, **Symbol Resolution** 和 **Relocation**.

每个模块都是单独编译的，在编译器编译 `main.c` 的时候并不知道 `printf` 函数的地址，所以它暂时把其地址搁置，等待最后链接的时候由连接器将这些目标指令的地址修正。连接器在连接的时候根据引用的符号 `printf` 自动的去对应模块查找 `printf` 的地址，然后将 `main.c` 中的所有引用到 `printf` 的指令修正，让它们的目标地址为真正的 `printf` 函数的地址。

这个地址修正的过程也被叫做 **Relocation**，每个修正到地方叫 **Relocation Entry**。

---

编译器编译源码后生成的文件叫做 **Object File**。object file 就是源码编译后但还未进行链接的那些中间文件。

```sh
 ± gcc -c main.c && file main.o
main.o: ELF 64-bit LSB relocatable, x86-64, version 1 (SYSV), not stripped
```

执行 `file main.o` 命令后我们可以看出，object file 是 **Executable Linkable Format** 文件的一种。

ELF 文件标准里把 ELF 格式的文件归为一下四类：

-   **Shared Object File**，例如 `/lib/glibc-2.5.so` 和 Windows 的 DLL: 这种文件包含了代码和数据，可以在一下两种情况下使用
    -   连接器可以使用这种文件跟其他可重定位文件和目标文件链接产生新的目标文件
    -   第二种是动态连接器可以将几个这种共享目标文件与可执行文件结合，作为进程映像的一部分来运行。
-   **Executable File**，这类文件包含了可以直接执行的程序
-   **Relocatable File**, 例如 Linux 的 `.o` 这类文件包含了代码和数据，可以被用来链接成可执行文件或 shared object file。
-   **Core Dump File**, 当进程意外终止时，系统可以将进程的地址空间的内容及终止时的一些其他信息转储到 core dump file

---

首先 object file 当然包含编译后的机器指令和数据。在这之外，还包含连接时所需要的一切信息，比如符号表、调试信息、字符串等。

ELF header 描述了整个文件的文件属性，包括文件是否可执行、是静态链接还是动态链接以及入口地址（如果是可执行文件）、目标硬件、目标操作系统等信息。

```c
int printf(const char* format, ...);

int global_init_var = 84;
int global_uninit_var;

void func1(int i) { printf("%d\n", i); }

int main(void) {
    static int static_var = 85;
    static int static_var2;

    int a = 1;
    int b;

    func1(static_var + static_var2 + a + b);

    return 0;
}
```

```sh
 ± objdump main.o -h

main.o:     file format elf64-x86-64

Sections:
Idx Name          Size      VMA               LMA               File off  Algn
  0 .text         0000005c  0000000000000000  0000000000000000  00000040  2**0
                  CONTENTS, ALLOC, LOAD, RELOC, READONLY, CODE
  1 .data         00000008  0000000000000000  0000000000000000  0000009c  2**2
                  CONTENTS, ALLOC, LOAD, DATA
  2 .bss          00000008  0000000000000000  0000000000000000  000000a4  2**2
                  ALLOC
  3 .rodata       00000004  0000000000000000  0000000000000000  000000a4  2**0
                  CONTENTS, ALLOC, LOAD, READONLY, DATA
  4 .comment      00000013  0000000000000000  0000000000000000  000000a8  2**0
                  CONTENTS, READONLY
  5 .note.GNU-stack 00000000  0000000000000000  0000000000000000  000000bb  2**0
                  CONTENTS, READONLY
  6 .note.gnu.property 00000030  0000000000000000  0000000000000000  000000c0  2**3
                  CONTENTS, ALLOC, LOAD, READONLY, DATA
  7 .eh_frame     00000058  0000000000000000  0000000000000000  000000f0  2**3
                  CONTENTS, ALLOC, LOAD, RELOC, READONLY, DATA
```

-   源码编译后的机器指令经常存放在 code section 里（.text）；
-   rodata section 保存了 "%d\n" 这样的只读数据，一般是程序里面的只读变量（例如 const 修饰的）
-   全局变量和局部静态变量数据经常放在 data section 里 (.data)；
-   未初始化的全局变量和未初始化的局部静态变量存储在 bss section 里（.bss）。bss section 并没有内容，所以它在文件中也不占据空间。未初始化的变量默认为 0，所以为它们在 data section 中分配空间且存储 0 是没有必要的。bss section 只是为这些变量预留位置而已。

我们看到 .bss 虽然 size 不为 0 但是它是没有 CONTENTS 属性的。它的下一个 section .rodata 的 offset 和 它是相同的，说明它在文件中确实没有占据空间。

```
ELF Header
.text
.data
.bss
... other sections
Section Header Table
String Tables
Symbol Tables
...
```

我们可以用 `readelf` 命令来详细查看 ELF 文件

```txt
 ± readelf -h main.o
ELF Header:
  Magic:   7f 45 4c 46 02 01 01 00 00 00 00 00 00 00 00 00
  Class:                             ELF64
  Data:                              2's complement, little endian
  Version:                           1 (current)
  OS/ABI:                            UNIX - System V
  ABI Version:                       0
  Type:                              REL (Relocatable file)
  Machine:                           Advanced Micro Devices X86-64
  Version:                           0x1
  Entry point address:               0x0
  Start of program headers:          0 (bytes into file)
  Start of section headers:          1064 (bytes into file)
  Flags:                             0x0
  Size of this header:               64 (bytes)
  Size of program headers:           0 (bytes)
  Number of program headers:         0
  Size of section headers:           64 (bytes)
  Number of section headers:         14
  Section header string table index: 13
```

我们可以看到结构体的每一个字段都和 `readelf` 的输出一一对应。值得一提的是魔数。

魔数的前 4 个字节是所有 ELF 文件都必须相同的标识码 7f 45 4c 46。第 5 个字节是用来标识 ELF 文件类的，01 表示是 32 位的，02 表示是 64 位的。第 6 个字节用来表示字节序。第 7 个字节规定 ELF 文件的版本号。后面的 9 个字节 ELF 标准没有定义，通常为 0。

```c
#define EI_NIDENT	16

typedef struct elf64_hdr {
    unsigned char e_ident[EI_NIDENT]; /* ELF "magic number" */
    Elf64_Half e_type;
    Elf64_Half e_machine;
    Elf64_Word e_version;
    Elf64_Addr e_entry; /* Entry point virtual address */
    Elf64_Off e_phoff; /* Program header table file offset */
    Elf64_Off e_shoff; /* Section header table file offset */
    Elf64_Word e_flags;
    Elf64_Half e_ehsize;
    Elf64_Half e_phentsize;
    Elf64_Half e_phnum;
    Elf64_Half e_shentsize;
    Elf64_Half e_shnum;
    Elf64_Half e_shstrndx;
} Elf64_Ehdr;
```
