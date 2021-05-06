---
title: Validate Temporal Relations
---

Asynchronous means that actions can happen independent of the current "main" execution stream.

You may say that, technically speaking, an asynchronous operation automatically assumes eventing -- at least "completed", "faulted" or "aborted/cancelled" events (one or more of these) are sent to the instigator of the operation (or the underlying O/S itself) to signal that the operation has ceased. Thus, async is always event-driven, but not the other way round.

Especially in applications that have many interactions from outside:
- user actions (e.g., touch the screen, type input)
- signals
- interrupts (e.g., network, timer)

we call these source events here

Every task is associated with an unordered set representing the async operations that should be done before it.

```ocaml
val fn: (int -> int) async_op
```

```ocaml
let receive_bounded_fn (fn: Task) = (* ... *)
```



```ocaml
let () = register event fn
```

the temporal relationship is determined by the locations where they are called/defined



We can make no destructive changes to the existing type system.

we split types into value types and task types  