A closed term is one in which all identifiers are bound.

**α-equivalence** we can change the name of bound variables without changing the meaning of functions. Thus λx.x is the same function as λy.y. Expressions e1 and e2 that differ only in the name of bound variables are called α-equivalent ("alpha equivalent"), sometimes written e1 =α e2.


**β-equivalence** We write e1{e2/x} to mean expression e1 with all free occurrences of x replaced with e2.
We call this equivalence, between (λx. e1) e2 and e1{e2/x}, is called β-equivalence.

In the pure lambda calculus, any abstraction is a value. Remember, an abstraction λx. e is a function; in the pure lambda calculus, the only values are functions. 
In an applied lambda calculus with integers and arithmetic operations, values also include integers. Intuitively, a value is an expression that can not be reduced/executed/simplified any further.

Evaluation strategies

**Call-by-value**: it only allows an application to reduce
after its argument has been reduced to a value and does not allow evaluation under a λ.
    - given an application e1 e2, we first evaluate e1 until it is a value, then we evaluate e2 until it is a value, and then we apply the function to the value—a β-reduction.

**Call-by-name**:  applies the function as soon as possible.


Fixed point combinators

**Y combinator**

Y = λf.(λp.p p)(λf.λx.f f x)

fact = \n -> if n = 1 then 1 else n * fact (n - 1)

fact = (\f n -> if n = 1 then 1 else n * f (n - 1)) fact

fact = (\f n -> if n = 1 then 1 else n * f (n - 1)) 
       (\f n -> if n = 1 then 1 else n * f (n - 1))

fact = (\f n -> if n = 1 then 1 else n * f f (n - 1)) 
       (\f n -> if n = 1 then 1 else n * f f (n - 1))

fact = (\f -> (\p n -> if n = 1 then 1 else n * p (n - 1)) (f f)) 
       (\f -> (\p n -> if n = 1 then 1 else n * p (n - 1)) (f f)) 

fact' = \p n -> if n = 1 then 1 else n * p (n - 1)
Y = \F -> (\f -> F (f f)) (\f -> F (f f))

fact = Y fact' 
     = (\f -> fact' (f f)) (\f -> fact' (f f))
     = fact' ((\f -> fact' (f f)) (\f -> fact' (f f)))
     = fact' fact

then fact = Y fact' is the fixed-point of fact'

Y = \F -> (\f -> F (f f)) (\f -> F (f f)) works under call-by-name semantics but not call-by-value
    because (f f) will always be called even though n = 1
we could delay this calculation by using eta-conversion: 
    Y = \F -> (\f -> F (\x -> f f)) (\f x -> F (\x -> f f))

```ocaml
let fix f input =
  (fun proc -> f (fun x -> proc proc x))
    (fun proc -> f (fun x -> proc proc x))
    input

(* let fix f input = (fun proc -> f (proc proc)) (fun proc -> f (proc proc)) input *)

let fact' proc n = match n with 0 -> 1 | n' -> n * proc (n' - 1)

let fact = fix fact'

;;
print_int (fact 10)
```

**Turning's fixed-point combinator Θ**

from definition of fixed-point: Θ f = f (Θ f)
Θ = \f -> f (Θ f)
Θ = (\t f -> f (t t f)) (\t f -> f (t t f))

fact = Θ fact'
     = (\t f -> f (t t f)) (\t f -> f (t t f)) fact'
     = \f -> f ((\t f -> f (t t f)) (\t f -> f (t t f)) f) fact'
     = fact' ((\t f -> f (t t f)) (\t f -> f (t t f)) fact')
     = fact' (Θ fact')
     = fact' fact