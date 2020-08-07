**Operating Systems are fundamentally event-driven systems**. E.g. The disk controller finishes reading in the disk block and generates and interrupt. The OS moves the read data into the user program and restarts the user program.

## Process 

Process is a key OS abstraction that users see. OS implement the process abstraction by using a context switch to switch from running one process to running another process

Processes are separated: no process can directly affect the state of another process.

A processor has a limited amount of physical resources. For example, it has only one register set. But every process running on the machine behaves like it has its own set of registers. **How does machine implement context switch?** 
    Solution is saving and restoring hardware state on a context switch. Save the sate in **Process Control Block (PCB)**. Depends on the hardware, the PCB can have
        - Registers – almost all machines save registers in PCB
        - Processor Status Word 
        - Stack pointer
        - PID
        - Program Counter
        - Memory (Page Tables, Segment Tables
        - Open files

## Thread

A thread is again an execution stream in the context of a thread state. Key difference between processes and threads is that **multiple threads share parts of their state.** (Recall that no processes could directly access memory of another process) But each thread still has its own registers. Also has its own stack, but other threads can read and write the stack memory.
**Typically, a Thread Control Block (TCB) contains only registers. Do not need to do anything to the MMU when switch threads.**

OS will have a separate thread for each process and that thread will perform OS activities on behalf the process. In this case we say that each user process is backed by a kernel thread.
    - When process starts up a remote TCP connection, its thread handles the low-level details of sending out network packets.

### Thread Creation

```c++
class Thread {
   public:
    Thread(char* debugName);
    ~Thread();
    void Fork(void (*func)(int), int arg);
    void Yield();
    void Finish();
}
```

The constructor creates a new thread: it allocates a data structure with space for the TCB
To start the thread running, must tell it what function to start running when it runs.
