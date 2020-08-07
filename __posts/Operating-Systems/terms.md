Concurrency: allow several different tasks to be underway at the same time, as if each had a private machine. To keep track of everything, processes and threads were invented.

Thread: a sequential execution stream
    - Executes a series of instructions in order (only one thing happens at a time)

Process: one or more threads, along with their execution state.
    - Execution state: everything that can affect, or be affected by, a thread:
    Code, data, registers, call stack, open files, network connections, time of day, etc.
    - Part of the process state is private to a thread
    - Part is shared among all threads in the process

hyperthreading: each physical core behaves as if it is actually two cores, so it can run two threads simultaneously (e.g. execute one thread while the other is waiting on a cache miss).

process control block to keep track of each process
    - Execution state for each thread (saved registers, etc.)
    - Scheduling information
    - Information about memory used by this process
    - Information about open files
    - Accounting and other miscellaneous information

3 states of process:
    - Running
    - Blocked: waiting for an event (disk I/O, incoming network packet, etc.)
    - Ready: waiting for CPU time

