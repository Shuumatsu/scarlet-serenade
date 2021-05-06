---
title: Validate Temporal Relations of Asynchronous Functions
---

Asynchronous means that actions can happen independent of the current "main" execution stream.

---

Asynchronous functions have a wide range of usages
- user actions (e.g., touch the screen, type input)
- signals
- interrupts (e.g., network, timer)

Things can get messy in large projects especially where first class functions are supported.

---

e.g., Some common bugs

Different function callbacks have same type signature, so wrong callback may be registered.

```javascript
document.addEventListener('click', cb_for_blur)
```

It's easy to forget to await on an async function. In a web scraper, you may fail to limit the rate of requests.

```rust
pub async fn run() -> Result<()> {
    let mut client = client::connect(host).await?;

    // do something

    // sleep(Duration::from_millis(100)).await?;
    sleep(Duration::from_millis(100));
    Ok(())
}
```

---

Technically speaking, an asynchronous task automatically assumes some event. 

- directly register the function on an event
- register the continuation/function on the completion of previous task (event)
    - used as an callback function 
    - await on an async function (usually promise-based

Thus, async is always event-driven. So we start with events to build the model.

---

We start from simple, we construct a new type system based on simply-typed lambda calculus. We introduce a new kind of type called **event type**. 

Inspired by Refinement Types, every function of interests can be annotated with 
- an ordered list representing the async operations that should be done (events that should be triggered) before it.
- an event type to be triggered when it's done

```ocaml
type annotated_type (a: function_type) = Anno of a * [event_type] * event_type option
```

In this way, we can make no destructive changes to the existing type system. For languages like TypeScript, that support Phantom Types, we could even make no changes in the syntax level.

---

We have two key rules:
- a function implemented for the callback of an event cannot be called in other places
- annotated functions can only be executed after some events

And the key factors are 
- where and how the function is defined
&nbsp;&nbsp;&nbsp;&nbsp;we get enough information by writing type signatures 
- where the function is called
&nbsp;&nbsp;&nbsp;&nbsp;we need a context-sensitive analysis 

---

We can restric the function maybe called by add restriction on the params.

Only functions with the same or fewer required predecessor events can be passed in
```ocaml
let bounded (fn: event_a + event_b) = (* do something *)
```

```ocaml
val cb_for_click_event: (() -> (), [click_event])
let cb_for_click_event () = (* do something *)

let register_click_event (cb: click_event) = (* do something *)
let _ = register_blur_event cb_for_click_event (* error *)
```

If possible, we could have some improvements, like dependent type support. Instead of having `register_click_event` and `register_blur_event`, we could have a generic function `register`, that automatically infer the callback type from the event param.

we start from the main function of the program, we do context-sensitive pointer analysis on every possible execution path.

```ocaml
let main = 
    task_a ();
    await task_after_event_a (); (* error *)
    await task_a ();
    (* += event_a *)
    await task_after_event_a (); 
```



