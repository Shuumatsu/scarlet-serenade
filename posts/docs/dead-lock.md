# Deadlock

Deadlock is a situation where a set of processes are blocked because each process is holding a resource and waiting for another resource acquired by some other process.
 
Deadlock can arise if the following four conditions hold simultaneously (Necessary Conditions)

- **Mutual Exclusion**: One or more than one resource is non-sharable (Only one process can use at a time)
- **Hold and Wait**: A process is holding at least one resource and waiting for resources.
- **No Preemption**: A resource cannot be taken from a process unless the process releases the resource.
- **Circular Wait**: A set of processes are waiting for each other in circular form.

quiz: 有 m 份不可共享的资源，有 n 个进程相互竞争，假设每个进程需求 \( S_i \) 那么他们满足什么样的数学关系可以保证没有死锁

An: 考虑极端情况，所有的进程都拿到了 \( S_i - 1 \) 份资源，那么再需要一份即可打破，那么数学关系式为 \( \sum (S_i - 1) + 1 <= m \)，也就是 \( \sum S_i < m + n \)

**Resource Allocation Graph** Each resource is represented with a box, each process with a circle, and the individual instances of the resources with dots in the boxes. Arrows go from processes to resource boxes if the process is waiting for the resource. Arrows go from dots in resource box to processes if the process holds that instance of the resource. 

If graph contains no cycles, is no deadlock. If has a cycle, may or may not have deadlock. 


**Concept: Safe Sequence**. Is an ordering of processes such that all processes can execute to completion in that order even if all request maximum resources. 
**Concept: Safe State** - a state in which there exists a safe sequence. Deadlock avoidance algorithms always ensure that system stays in a safe state.

How can you figure out if a system is in a safe state? 
- Bankers' Algorithm

Here is deadlock detection algorithm. Is very similar to safe state detection algorithm.

1: Work = Avail;
   Finish[i] = False for all i;
2: Find i such that Finish[i] = False and Request[i] <= Work
   If no such i exists, goto 4
3: Work = Work + Alloc[i]; Finish[i] = True; goto 2
4: If Finish[i] = False for some i, system is deadlocked. 
   Moreover, Finish[i] = False implies that process i is deadlocked.

基本上和找安全序列的区别就是最后一步，Finish[i] 对应着


## Methods for handling deadlock

There are three ways to handle deadlock

### Deadlock prevention or avoidance

We need to know all information about resources which the process WILL need beforehand. 

#### Deadlock prevention

which means we are to break one (or more) of the 4 Necessary Conditions

**Eliminate Circular Wait**: Order the locks, and always acquire the locks in that order.

Occasionally you may need to write code that needs to acquire locks in different orders. Here is what to do in this situation.

- First, most locking abstractions offer an operation that tries to acquire the lock but returns if it cannot. We will call this operation TryAcquire. Use this operation to try to acquire the lock that you need to acquire out of order.
- If the operation succeeds, fine. Once you've got the lock, there is no problem.
- If the operation fails, release all of the locks that come after the lock you are trying to acquire, then reacquire all of the locks in the right order.

```
int d1, d2;
// The standard acquisition order for these two locks
// is l1, l2.
Lock *l1, // protects d1 
     *l2; // protects d2
// Decrements d2, and if the result is 0, increments d1
void increment() {
  l2->Acquire();
  int t = d2;
  t--;
  if (t == 0) { 
    if (l1->TryAcquire()) {
      d1++;
    } else { 
      // Any modifications to d2 go here - in this case none
      l2->Release();
      l1->Acquire();
      l2->Acquire();
      t = d2;
      t--;
      // some other thread may have changed d2 - must recheck it
      if (t == 0) { 
        d1++;
      }
    }
    l1->Release();
  }
  d2 = t;
  l2->Release();
}
```

**Eliminate Hold and Wait**: 
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


 Must free up some resources so that some processes can run. So, preempt resources - take them away from processes. Several different preemption cases:
 
Can preempt some resources without killing job - for example, main memory. Can just swap out to disk and resume job later.

If job provides rollback points, can roll job back to point before acquired resources. At a later time, restart job from rollback point. Default rollback point - start of job.

For some resources must just kill job. All resources are then free. Can either kill processes one by one until your system is no longer deadlocked. Or, just go ahead and kill all deadlocked processes.

