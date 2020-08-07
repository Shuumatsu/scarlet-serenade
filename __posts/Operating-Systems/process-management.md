<img src="D:\learning-notes\Operating-Systems\process-management.assets\image-20200607235935318.png" alt="image-20200607235935318"  />

- New(create): yet not created, it’s the program in secondary memory that will be picked up by OS to create process
- Suspended ready: swapped out to external storage by scheduler
- Suspended blocked: 

Dispatcher: loading the selected process
    1. Switching context
    2. Switching to user mode
    3. Jump to proper location in the newly loaded program

Schedulers
    - Long term (job
        - Decides how many processes should be made to stay in ready state(degree of multi programming
    - Medium term (part of swapping
        - Sends that process from running queue to blocked queue. When a process completes its I/O operation then it should again be shifted to ready queue
    - Short term (CPU 
        - Decides which process to run next and then call the dispatcher

![image-20200608000803427](D:\learning-notes\Operating-Systems\process-management.assets\image-20200608000803427.png)


## CPU scheduling

- arrival_t
- completion_t
- burst_t: time required by a process for CPU execution
- turnaround_t: completion_t – arrival_t
- waiting_t: ternaround_t – burst_t

### Algorithms

- FCFS (non-preemptive

- Shortest job first (non-preemptive
- Shortest remaining time first (preemptive
    - When a new job comes in, if it has shorter remaining time than current executing one, then it will preempt

- Longest job first (non-preemptive
- Longest remaining time first (preemptive

- Highest response ratio next (non-preemptive
    - Each time choose the process with highest response ratio

- Round-robin scheduling (non-preemptive
    - Based on FCFS, after a fixed time interval or the current process is finished, push current process to the tail of the list (if not finished), switch to the next process
        - depends on implementation, may push back the new arrivals before put current unfinished task back to ready_queue

- Multilevel queue scheduling
    - We have queues of different priorities, Each queue has its own scheduling algorithm. E.g. queue 1 and queue 2 uses Round Robin while queue 3 uses FCFS
    Scheduling among the queues: Each queue has absolute priority over lower priority queues, which means that unless queue 1 is empty, no process inside queue 2 can run
        - An alternative approach is: Each queue gets certain portion of CPU time to schedule process inside it. E.g. queue 1 get 50% CPU time, queue 2 get 20% CPU time and queue 3 get 30% CPU time

- Multilevel feedback queue scheduling
    Based on Multilevel queue scheduling, but process can move between the queues.
    - Rule 1: If priority(A) > Priority(B), then A runs
    - Rule 2: if priority(A) = Priority(B), then A & B run in Round Robin
    - Rule 3: when a job enters the system, it is placed at the queue highest priority 
    - Rule 4: 
        - If a job uses up an entire time slice while running, its priority is reduced (i.e. gets moved down one queue). 
        - If a job gives up the CPU before the time slice is up, it stays at the same queue
    - Rule 5: After some time period S, move all the jobs in the system to the queue with highest priority

### Practice 

It is helpful to explicitly maintain a queue when manually calculating the processes running at each moment. For example there are 5 processes A, B, C, D, E, the queue written A3B2 indicates that the next one to run will be process A, and process A takes 3 more units of time slice.

e.g. given following process 

| Process ID | Arrival time | CPU burst time | Priority |
| ---------- | ------------ | -------------- | -------- |
| A          | 0            | 3              | 4        |
| B          | 1            | 1              | 2        |
| C          | 3            | 5              | 5        |
| D          | 6            | 2              | 6        |
| E          | 8            | 2              | 1        |

For each of the three scheduling algorithms, fill in the the table below with the processes that are
running on the CPU at each time tick. Assume the following:
    - The time quantum for RR is 1 clock tick and when RR quantum expires, the currently running thread is added to the end of the ready list before any newly arriving threads.
    - Assume that context switch overhead is 0, and that new processes are available for scheduling as soon as they arrive.

| Time | RR   | RTF  | Priority |
| ---- | ---- | ---- | -------- |
|      |      |      |          |

Use RR as an example, we use 3 columns: Time, Queue, Running

| Time | Queue | Running |
| ---- | ----- | ------- |
| 0    | A3    | A       |
| 1    | A2B   | A       |
| 2    | BA    | B       |
| 3    | AC5   | A       |
| 4    | C5    | C       |
| 5    | C4    | C       |
| 6    | C3D2  | C       |

Continue doing this will give us the example.

The solutions is:

![image-20200608001922179](D:\learning-notes\Operating-Systems\process-management.assets\image-20200608001922179.png)