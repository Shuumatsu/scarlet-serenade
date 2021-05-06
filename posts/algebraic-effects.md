---
title: Algebraic Effects
---

Side effects: Observable effects of a computation other than its return value
Algebraic effects: Represent side effects as sets of operations that interact with the context of a computation

we can pause execution and ask the context to execute a get() for use and return the resulting value

By thinking of effects this way we separate the specification of how to execute pure computations from the specification of how side-effects behave 


implementation:
- entering an effect handler creates a fresh stack
- these stacks are heap allocated and dynamically resized 
- performing an effect wraps the current stack in a continuation and executes the handler on the previous stack 
- continuing a continuation reinstances the paused stack and resumes its execution