---
title: Essentials of Programming Languages
---

`raco pkg install eopl`

https://efanzh.org/2017/08/06/essentials-of-programming-languages-exercises.html


# Data Abstraction

When the client manipulates the values of the data type only through the procedures in the interface, we say that the client code is **representation independent**, because then the code does not rely on the representation of the values in the data type.

If in this way we implement our natural numbers, it doesn't matter if we are using a church number or in a binary form.

In the exercies part, "Exercise 2.3" gives an example, which is much like exporting a type in Haskell without exporting its data constructor.


# Representation Strategies for Data Types

## exercises 

> Exercise 2.1 [★] Implement the four required operations for bigits. Then use your implementation to calculate the factorial of 10. How does the execution time vary as this argument changes? How does the execution time vary as the base changes? Explain why.

```lisp
; when base becomes smaller, the time used execution becomes longer
; since more allocations are needed
(define base 2)

; I will use pairs to represent the natural numbers except zero
; since the `predecessor` should not perform on zero.
(define zero empty)

(define is-zero? null?)

(define (successor n)
        (if (is-zero? n)
            (cons 1 zero)
            (let ([lowest-digit (car n)]
                  [rest-digits (cdr n)])
                 (if (= (- base 1) lowest-digit)
                     (cons 0 (successor rest-digits))
                     (cons (+ lowest-digit 1)
                           rest-digits)))))

(define (predecessor n)
        (let ([lowest-digit (car n)]
              [rest-digits (cdr n)])
             (if (= 0 lowest-digit)
                 (cons (- base 1)
                       (predecessor rest-digits))
                 (if (and (= 1 lowest-digit)
                          (is-zero? rest-digits))
                     zero
                     (cons (- lowest-digit 1)
                           rest-digits)))))

(define (plus x y)
        (if (is-zero? x)
            y
            (plus (predecessor x) (successor y))))

(define (multiply x y)
        (define (h accu x y)
                (if (is-zero? x)
                    accu
                    (h (plus accu y) (predecessor x) y)))
        (h zero x y))

(define (factorial n)
        (define (h accu curr)
                (if (is-zero? curr)
                    accu
                    (h (multiply accu curr)
                       (predecessor curr))))
        (h (successor zero) n))
```

> Exercise 2.3 [★★] Define a representation of all the integers (negative and nonnegative) as diff-trees, where a diff-tree is a list defined by the grammar
> Diff-tree ::= (one) | (diff Diff-tree Diff-tree)
> The list (one) represents 1. If t1 represents n1 and t2 represents n2, then (diff t1 t2) is a representation of n1 − n2.
> So both (one) and (diff (one) (diff (one) (one))) are representations of 1; (diff (diff (one) (one)) (one)) is a representation of -1.
> Show that every number has infinitely many representations in this system.
> Turn this representation of the integers into an implementation by writing zero, is-zero?, successor, and predecessor, as specified on page 32, except that now the negative integers are also represented. Your procedures should take as input any of the multiple legal representations of an integer in this scheme. For example, if your successor procedure is given any of the infinitely many legal representations of 1, it should produce one of the legal representations of 2. It is permissible for different legal representations of 1 to yield different legal representations of 2.
> Write a procedure diff-tree-plus that does addition in this representation. Your procedure should be optimized for the diff-tree representation, and should do its work in a constant amount of time (independent of the size of its inputs). In particular, it should not be recursive.

This first problem is easy to answer using Contradiction. And the last two problem I will use ocaml to implement, since ocaml has better adt support.

```ocaml

```
