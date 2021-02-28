---
title: Conditional Variables
---

A condition variable is an explicit queue that threads can put themselves on when some state of execution (i.e. some condition) is not as desired (by waiting on the condition); \
some other thread, when it changes said state, can then wake one (or more) of those waiting threads and thus allow them to continue (by signaling on the condition)

```c++
class Condition {
  public:
    Condition(char* debugName);  
    ~Condition(); 
    void Wait(Lock *conditionLock); 
    void Signal(Lock *conditionLock);
    void Broadcast(Lock *conditionLock);
}
// All locks must be the same one.
```

- `Wait(Lock *l)`: atomically releases the lock and waits. When `Wait` returns, the lock will have been reacquired 
- `Signal(Lock *l)`: Enables one of the waiting threads to run. When `Signal` returns the lock is still acquired
`Broadcast(Lock *l)`: Enables all of the waiting threads to run. When `Broadcast` returns the lock is still acquired

---

There are two kinds of condition variables:
- Mesa semantics, there are no guarantees when the signaled thread will run. Other threads that acquire the lock can execute between the signaler and the waiter.
- Hoare semantics, is harder to build but provides a stronger guarantee that the woken thread will run immediately upon being woken

--- 

Let’s consider a bounded buffer problem. Suppose we have two consumer threads and one producer thread.

Below is a buggy implementation:

```c++
int loops;  // must initialize somewhere...
cond_t cond;
mutex_t mutex;
void* producer(void* arg) {
    int i;
    for (i = 0; i < loops; i++) {
        Pthread_mutex_lock(&mutex);            // p1
        if (count == 1)                        // p2
            Pthread_cond_wait(&cond, &mutex);  // p3
        put(i);                                // p4
        Pthread_cond_signal(&cond);            // p5
        Pthread_mutex_unlock(&mutex);          // p6
    }
}
void* consumer(void* arg) {
    int i;
    for (i = 0; i < loops; i++) {
        Pthread_mutex_lock(&mutex);            // c1
        if (count == 0)                        // c2
            Pthread_cond_wait(&cond, &mutex);  // c3
        int tmp = get();                       // c4
        Pthread_cond_signal(&cond);            // c5
        Pthread_mutex_unlock(&mutex);          // c6
        printf("%d\n", tmp);
    }
}
```

Things may happen in following order:
- At t1, consumer thread 1 run, it finds the buffer empty, then it goes to sleep
- At t2, producer thread 1 run, it finds the buffer empty, then it add one item to the buffer then signal one sleeping thread (in this case consumer 1
- At t3, consumer thread 2 sneaks in, and it find the buffer ready, and it consumes the buffer
- At t4, consumer thread 1 run, it tries to consume the buffer, but the buffer is empty now. Here’s where the error occurs

After we change the `if` to `while`, things get better but **still broken**. 

There are two race conditions, but we have only one lock: 
1. the buffer 
2. who's the next one to be awaken 

Our lock only protects the buffer. Things may happen in following order and in this case a deadlock will occur:
- At t1, consumer thread 1 run, it finds the buffer empty, then it goes to sleep
- At t2, consumer thread 2 run, it finds the buffer empty, then it goes to sleep
- At t3, producer thread 1 run, it finds the buffer empty, then it add one item to the buffer then signal one sleeping thread (in this case either consumer 1 or consumer 2, let’s suppose consumer 1 is awaken
- At t4, consumer thread 1 run, it finds the buffer ready, then it consumes it and signals one sleeping thread, let’s suppose consumer 2 is awaken
- At t5, consumer thread 2 run, it finds the buffer empty, then it goes to sleep
At t5 we reach a state that all threads are sleeping. At t4, some producer thread is expected to be wake up, but the consumer thread 2 is wake up.

Below is a correct solution, 
    - producer threads wait on the condition empty and signal fill
    - consumer threads wait on the condition fill and signal empty

```c++
cond_t empty, fill;
mutex_t mutex;
void* producer(void* arg) {
    int i;
    for (i = 0; i < loops; i++) {
        Pthread_mutex_lock(&mutex);             // p1
        while (count == MAX)                    // p2
            Pthread_cond_wait(&empty, &mutex);  // p3
        put(i);                                 // p4
        Pthread_cond_signal(&fill);             // p5
        Pthread_mutex_unlock(&mutex);           // p6
    }
}
void* consumer(void* arg) {
    int i;
    for (i = 0; i < loops; i++) {
        Pthread_mutex_lock(&mutex);            // c1
        while (count == 0)                     // c2
            Pthread_cond_wait(&fill, &mutex);  // c3
        int tmp = get();                       // c4
        Pthread_cond_signal(&empty);           // c5
        Pthread_mutex_unlock(&mutex);          // c6
        printf("%d\n", tmp);
    }
}
```


#### Covering Conditions
Let us consider a multi-thread memory allocation library. Suppose we have 100 units of memory.
- At t1, thread 1 required 100 units of memory, and there are enough memory, so 100 units of memory are allocated to thread 1
- At t2, thread 2 required 50 units of memory, but there are not enough memory, so thread 2 goes to sleep
- At t3, thread 3 required 10 units of memory, but there are not enough memory, so thread 3 goes to sleep
- At t5, thread 1 releases 20 units of memory, and it signals one thread. But who are to be signaled?

In this case, all threads are both consumer and producer, so it is not like the bounded buffer problem, we cannot use `cond_t empty, fill` to direct

Instead we use `broadcast` to wakeup all waiting threads, this will introduce some performance penalty, but it is the simplest & straight forward method 

