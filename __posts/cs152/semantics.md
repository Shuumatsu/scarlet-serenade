advantages of giving a formal, mathematical definition of the language semantics.
    - Less ambiguous.
    - More concise: Mathematical concepts and notation can clearly and concise describe a language, and state restrictions on legal programs in the language.
    - Formal arguments: Most importantly, a formal semantics allows us to state, and prove, program properties that we’re interested in.

There are three main approaches to formally specify the semantics of programming languages: (Each of these approaches has different advantages and disadvantages in terms of how mathematically sophisticated they are, how easy it is to use them in proofs, or how easy it is to implement an interpreter or compiler based on them.)
    - operational semantics: describes how a program would execute on an abstract machine;
    - denotational semantics: models programs as mathematical functions;
    - axiomatic semantics: defines program behavior in terms of the logical formulae that are satisfied before and after a program;

we separate the “concrete syntax” of the language (which specifies how to unambiguously parse a string into program phrases) from the “abstract syntax” of the language (which describes, possibly ambiguously, the structure of program phrases).
    when writing expressions, we will occasionally use parenthesis to indicate the structure of the abstract syntax tree, but the parentheses are not part of the language itself.

**Small-step operational semantics** describe how such an execution in terms of successive reductions of an expression, until we reach a number, which represents the result of the computation

The state of the abstract machine is usually referred to as a configuration
    - a store (aka environment or state), which assigns integer values to variables.
    - the expression left to evaluate.

```
Config = Exp × Store
Store = Var → Int
```
For example, we have ⟨(4 + 2) × y, σ⟩ −→ ⟨6 × y, σ⟩. 

We can compactly describe the transition function −→ using inference rules:

The fact(s) above the line are called premises; the fact below the line is called the conclusion. The rules without premises are axioms; and the rules with premises are inductive rules.

If $f(y) = \sigma[x \mapsto n]$ then 
$$f(y)=\left\{\begin{array}{ll}
n & \text { if } y=x \\
\sigma(y) & \text { otherwise }
\end{array}\right.$$

Suppose we want to evaluate expression (foo + 2) × (bar + 1) in a store σ where σ(foo) = 4 and σ(bar) = 3.

We write −→∗for the reflexive transitive closure of the relation −→. That is, if ⟨e, σ⟩ −→∗ ⟨e′, σ′⟩, then using zero or more steps, we can evaluate the configuration ⟨e, σ⟩ to the configuration ⟨e′, σ′⟩. Thus, we can write:
⟨(foo + 2) × (bar + 1), σ⟩ −→∗ ⟨24, σ⟩.

The machine configuration that contains the final result is the point where the evaluation stops; they are called final configurations.

**Expressing Program Properties**
Progress:  For each store σ and expression e that is not an integer, there exists a possible transition for ⟨e, σ⟩:
$$
\forall e \in \text { Exp. } \forall \sigma \in \text { Store. either } e \in \operatorname{Int} \text { or } \exists e^{\prime}, \sigma^{\prime} .\langle e, \sigma\rangle \longrightarrow\left\langle e^{\prime}, \sigma^{\prime}\right\rangle
$$

Termination: The evaluation of each expression terminates:
$$
\forall e \in \text { Exp. } \forall \sigma_{0} \in \text { Store. } \exists \sigma \in \text { Store. } \exists n \in \text { Int. }\left\langle e, \sigma_{0}\right\rangle \longrightarrow^{*}\langle n, \sigma\rangle
$$

Deterministic Result: The evaluation result for any expression is deterministic:
$$
\begin{aligned}
&\forall e \in E x p . \forall \sigma_{0}, \sigma, \sigma^{\prime} \in \text { Store. } \forall n, n^{\prime} \in \text { Int. }\\
&\text { if }\left\langle e, \sigma_{0}\right\rangle \longrightarrow^{*}\langle n, \sigma\rangle \text { and }\left\langle e, \sigma_{0}\right\rangle \longrightarrow^{*}\left\langle n^{\prime}, \sigma^{\prime}\right\rangle \text { then } n=n^{\prime} \text { and } \sigma=\sigma^{\prime}
\end{aligned}
$$

**Inductive sets** is a set that is built using a set of axioms and inductive (inference) rules.

axioms of the form:
$$
\frac{}{a \in A}
$$

inductive rules:
$$
\frac{a_{1} \in A \quad \ldots \quad a_{n} \in A}{a \in A}
$$

The set A is the set of all elements that can be inferred to belong to A using a (finite) number of applications of these rules, starting only from axioms.



![image-20200715004752371](D:\learning-notes\cs152\semantics.assets\image-20200715004752371.png)