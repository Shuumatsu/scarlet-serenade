---
title: lambda_{ad}
---

## Core Calculus

Our core calculus $\lambda_{ad}$ is defined as follows. And for demonstration purpose, we split $\lambda_{ad}$ into two pars: a **sequential part** and a **parallel part**

```ebnf
func = fun (x: X) -> t 
     | async_fun (x: X) -> t

event = E {term}

value = n (* integer literal *)
      | C {term}
      | func
      | event
      | Anno event func
      | ()

pattern = 

term = value    
     | t1 t2 (* application *)
     | await t | promise_ref n 

     | ref t | !t | t := t | l (* store location *)

     | t1 + t2 | t1 - t2
     | let x = t1 in t2
     | match t1 with {p1 -> t1}
```

```
(fun (x: X) -> expr) value => expr[value/x]
let x = expr1 in expr2 => let x = value in expr2 => expr2[value/x]

if expr -> value then C expr => C value
```


## Dynamic Semantics

Since we are doing concurrent programming, so our process will contain possibly multiple thread.
    - A process is an instance of an executing program
    - A thread is an instance of an executing computation


In our definition, a program is just a $\lambda_{ad}$ term. And we define computations as sequences of small-step evaluations.

We model the whole world by composing individual computations into **parallel threads**.

World: set of computations(threads)
Configuration: (world, store, registry)

Every thread has a unique id for identifying purpose. When we apply an async function, we construct a new thread with a new associated unique id.

- In the previous thread, the async function application returns a promise reference containing the unique id.

- When the new thread finishes, the value of the computation will be used to fullfill the promise.


```
data Thread = Thread Int Computation

type World = [] Thread
```

```
if p, store, registry -> p', store', registry then
    run (Configuration (p:ps) store registry) = 
        (Configuration (p':ps) store' registry)
```



If the current step is async function application

```
(Configuration (((async fn (x: X) -> body) val)   :ps) store registry) -> (Configuration (p':[val/x]body:ps) store registry')
```

If a thread finally reduces to a value(the thread ends),

```
(Configuration (v:ps) store registry) -> (Configuration ps store registry')
```

Promise = Pending Int | Fulfilled Int Value

`map<unique_id, promise>`

thread_1 finishes => registry[1] (= Pending 1) => (FulFilled 1 v) 

registry' = (1, FulFilled 1 v)::registry

before
await promise // await pending

after

for await promise, we have evaluation rules only for fulfilled promise

```

application => promise_location

(pending | ful) <= registry[promise_location]
```


---

involve (raising events) in our dynamic semantics





1. eval: Config -> Config * Event

2. Conf of (thread, events)[] * registry * store 