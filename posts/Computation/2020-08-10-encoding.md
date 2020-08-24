### Encoding Boolean

`let cond p a b = if p then a else b` 是常见的 boolean 操作，我们从这个方法下手。可以看出如果将 `a b` 看作一个 tuple 那么 `p` 就像一个一个从 tuple 中取值的函数。`true` 为取第一个元素，`false` 为第二个。
容易写出函数表达的 boolean:

```
let true' a b = a

let false' a b = b

let and' a b = a b false'

let or' a b = a true' b
```

```
COND == λp.λa.λb.p a b
TRUE == λx.λy.x
FALSE == λx.λy.y

AND == λa.λb.a b FALSE
OR == λa.λb.a TRUE b
```

### Encoding LISP-style lists

从 `car` 与 `cdr` 入手，The way we'll define a non-nil list is as a function that "stores" the head and tail of the list in its body. Its argument will be a selector function (head or tail). 
```
<!-- type list = λs.s h t -->
car = λl.l (λh.λt.h) = λl.l true
cdr = λl.l (λh.λt.t) = λl.l false

is_empty = λl.l (λh.λt.false)
nil = λs.true

CONS = λh.λt.(λs.s h t)
```

### Encoding Natural Numbers

#### 可以用 list 来实现

```
ISZERO == ISEMPTY
PRED == TAIL
SUCC is a function that adds one more element onto a given list, so: SUCC == λL.CONS x L
```

#### 根据函数 apply 的次数来实现 

```
n = λx.λy.x^n y

0	==	λx.λy.y
1	==	λx.λy.xy
2	==	λx.λy.x(xy)
3	==	λx.λy.x(x(xy))
etc.
```

```
ISZERO = λf.f(λx.FALSE)TRUE
n != 0 即 x 一旦 apply 就返回 falses，那么 let `x` be `λx.FALSE`
n == 0 即 y 直接返回就返回 true，那么 let `y` be `True`
```

$$\left(\operatorname{SUCC}\left(\lambda x . \lambda y . x^{n} y\right)\right) \rightarrow_\beta \left(\lambda x . \lambda y . x^{n+1} y\right)$$

那么需要接受一个数返回另一个数，即接受一个接受两个参数的函数返回一个接受两个参数的函数。区别在于第二个函数的第一个参数多 apply 一次，但是我们无法改变参数函数的 function body，不妨在 apply 到函数之前，先让 x apply y 一次。
`SUCC == λa.(λx.λy.a x (xy))`


```ocaml
module type ChurchNumber = sig
  (* in untyped lambda calculus, we can use a generic type instead of bool *)
  (* we use bool here because of is_zero *)
  type t = (bool -> bool) -> bool -> bool

  val zero : t

  val one : t

  val two : t

  val is_zero : t -> bool

  val add : t -> t -> t

  val mult : t -> t -> t
end

module ChurchNumber : ChurchNumber = struct
  type t = (bool -> bool) -> bool -> bool

  let zero f b = b

  let one f b = f (f b)

  let two f b = f (f (f b))

  let is_zero n = n (fun _ -> false) true

  let add x y f b = x f (y f b)

  let mult x y f b = x (y f) b
end
```


