**Deadlock** is a situation where a set of processes are blocked because each process is holding a resource and waiting for another resource acquired by some other process.

Deadlock can arise if the following four conditions hold simultaneously (Necessary Conditions)
    - **Mutual Exclusion**: One or more than one resource is non-sharable (Only one process can use at a time)
    - **Hold and Wait**: A process is holding at least one resource and waiting for resources.
    - **No Preemption**: A resource cannot be taken from a process unless the process releases the resource.
    - **Circular Wait**: A set of processes are waiting for each other in circular form.


quiz: There are m shares of non-shared resources and n processes competing, and assuming each process needs $S_i$ units’ resources then what mathematical relationship do they satisfy that guarantees no deadlock
An: Consider the extreme case where all the processes have $S_i-1$ share of the resource, then only one more resource is needed, then the mathematical relation is $\sum\left(S_i-1\right)+1\le\ m$, i.e. $\sum S_i<m+n$

## Resource Allocation Graph
Each resource is represented with a box, each process with a circle, and the individual instances of the resources with dots in the boxes. Arrows go from processes to resource boxes if the process is waiting for the resource. Arrows go from dots in resource box to processes if the process holds that instance of the resource. 
If graph contains no cycles, is no deadlock. If has a cycle, may or may not have deadlock. 

## Safe State 

**Concept: Safe Sequence** is an ordering of processes such that all processes can execute to completion in that order even if all request maximum resources. 
**Concept: Safe State** is a state in which there exists a safe sequence. Deadlock avoidance algorithms always ensure that system stays in a safe state.

**Bankers' Algorithm** can be used to figuring out if a system is in a safe state. 

1. Work = Available.
   Finish[i] = False for all i;
2. Find i such that Finish[i] = False and Request[i] <= Work
   If no such i exists, goto 4
3. Work[i] = Work[i] + Alloc[i]; Finish[i] = True; goto 2
4. If Finish[i] = False for some i, system is deadlocked. 
   Moreover, Finish[i] = False implies that process i is deadlocked.

顺便可以知道哪些进程死锁了，第四步中 Finish[i] 对应着死锁的进程

## Deadlock prevention 

which means we are to break one (or more) of the 4 Necessary Conditions

**Eliminate Mutual Exclusion**: almost impossible

**Eliminate Circular Wait/Hold and Wait**: 
1. Allocate all required resources to the process before the start of its execution. 
    - 比如一个打印机，进程可能非常久之后才用，但是却一直占据
2. Order the locks and always acquire the locks in that order. In this case, there will be no situation where two processes wait for each other 
Occasionally you may need to write code that needs to acquire locks in different orders. Here is what to do in this situation
    - First, most locking abstractions offer an operation that tries to acquire the lock but returns if it cannot. We will call this operation TryAcquire. Use this operation to try to acquire the lock that you need to acquire out of order.
    - If the operation succeeds, fine. Once you have got the lock, there is no problem.
    - If the operation fails, release all the locks that come after the lock you are trying to acquire, then reacquire all of the locks in the right order.

```c++
int d1, d2;
// The standard acquisition order for these two locks
// is l1, l2.
Lock *l1,  // protects d1
    *l2;   // protects d2
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
            if (t == 0) { d1++; }
        }
        l1->Release();
    }
    d2 = t;
    l2->Release();
}
```

**Eliminate Preemption**: 
Must free up some resources so that some processes can run. So, preempt resources - take them away from processes. Several different preemption cases:
Can preempt some resources without killing job 
    - for example, main memory. Can just swap out to disk and resume job later.
    - If job provides rollback points, can roll job back to point before acquired resources. Later, restart job from rollback point. Default rollback point - start of job.
    - For some resources must just kill job. All resources are then free. Can either kill processes one by one until your system is no longer deadlocked. Or, just go ahead and kill all deadlocked processes.
