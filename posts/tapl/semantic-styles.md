---
title: Types and Programming Languages | Semantic Styles
---

严格的给出了语言的语法以后，下面需要给出对项如何求值的精确定义，即语言的语义。

**Definition**: the **semantics** of the language: precise definition of how terms are evaluated

There are three basic approaches to formalizing semantics:
1. **Operational semantics** specifies the behavior of a programming language by defining a simple **abstract machine** for it. (Strictly speaking, what we are describing here is the so-called **small-step style** of operational semantics
    - This machine is "abstract" in the sense that it uses the terms of the language as its machine code, rather than some low-level microprocessor instruction set. 
    - A **state** of the machine is just a term, and the machine's behavior is defined by a **translation function** that, for each state, either gives the next state by performing a step of simplification on the term or declares that the machine has halted
    - The **meaning** of a term `t` can be taken to be the final state that the machine reaches when started with `t` as its initial state


2. **Denotational semantics** takes 
    - The **meaning** of a term is taken to be some mathematical object, such as a number or a function. 
    - Giving denotational semantics for a language consists of 
        1. finding a collection of **semantic domains** (btw. **domain theory**: the search for appropriate semantic domains for modeling various language features
        2. defining an **interpretation** function mapping terms into elements of these domains

3. **Axiomatic semantics** instead of first defining the behaviors of programs (by giving some operational or denotational semantics) and then deriving laws from this definition, axiomatic methods take the laws themselves as the definition of the language. 
    - The **meaning** of a term is what can be proved about it.