# Deadlock

Deadlock is a situation where a set of processes are blocked because each process is holding a resource and waiting for another resource acquired by some other process.
 
Deadlock can arise if the following four conditions hold simultaneously (Necessary Conditions)

- **Mutual Exclusion**: One or more than one resource is non-sharable (Only one process can use at a time)
- **Hold and Wait**: A process is holding at least one resource and waiting for resources.
- **No Preemption**: A resource cannot be taken from a process unless the process releases the resource.
- **Circular Wait**: A set of processes are waiting for each other in circular form.

quiz: 有 m 份不可共享的资源，有 n 个进程相互竞争，假设每个进程需求 \( S_i \) 那么他们满足什么样的数学关系可以保证没有死锁

An: 考虑极端情况，所有的进程都拿到了 \( S_i - 1 \) 份资源，那么再需要一份即可打破，那么数学关系式为 \( \sum (S_i - 1) + 1 <= m \)，也就是 \( \sum S_i < m + n \)


## Methods for handling deadlock

There are three ways to handle deadlock

### Deadlock prevention or avoidance

We need to know all information about resources which the process WILL need beforehand. 

#### Deadlock prevention

which means we are to break one (or more) of the 4 Necessary Conditions

- Eliminate Hold and Wait: 
  - Allocate all required resources to the process before the start of its execution. 
    - 会降低资源的利用度，比如明明进程要很久之后才用打印机的情况
  - The process will make a new request for resources after releasing the current set of resources. This solution may lead to starvation. （为啥
- Eliminate No preemption: Each resource will be assigned with a numerical number. A process can request the resources increasing/decreasing the order of numbering. 没懂

#### Deadlock Avoidance

The famous Deadlock avoidance algorithm is Bankers Algorithm


### Deadlock detection and recovery

Let deadlock occur, then do preemption to handle it once occurred.

### Ignore the problem altogether

If the deadlock is very rare, then let it happen and reboot the system. This is the approach that both Windows and UNIX take.
