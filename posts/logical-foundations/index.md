---
title: Logical Foundations
---

在 Coq 中，检验的方式一共有三种：

第一，我们可以用 `Compute` 指令来计算包含 next_weekday 的复合表达式：
```coq
Compute (next_weekday friday).
(* ==> monday : day *)
```

第二，我们可以将期望的结果写成 Coq 的示例：
```coq
Example test_next_weekday:
  (next_weekday (next_weekday saturday)) = tuesday.
Proof. simpl. reflexivity. Qed.
```

第三，我们可以让 Coq 从 `Definition` 中 '提取（Extract）' 出用其它更加常规的编程语言编写的程序（如 OCaml、Scheme、Haskell 等）

---

Coq 中的每个表达式都有类型，它描述了该表达式所计算的东西的类别。 `Check` 指令会让 Coq 显示一个表达式的类型。
```coq
Check true.
(* ===> true : bool *)
```

## Fixpoints and Structural Recursion

Coq 要求每个 `Fixpoint` 定义中的某些参数必须是 "递减的"。\
这项要求是 Coq 的基本特性之一，它保证了 Coq 中定义的所有函数对于所有输入都会终止。 然而，由于 Coq 的 "递减分析" 不是非常精致， 因此有时必须用一点不同寻常的方式来编写函数。

you can write a sensible Fixpoint definition (of a simple function on numbers, say) that does terminate on all inputs, but that Coq will reject because of this restriction. 

## Working with Structured Data

执行 Search foo 会让 Coq 显示所有涉及到 foo 的定理。例如，去掉下面的注释后， 你会看到一个我们证明过的所有关于 rev 的定理的列表：
```coq
(*  Search rev. *)
```


## Polymorphism and Higher-Order Functions

Coq 支持定义 '多态' 归纳类型。 例如，以下就是 '多态列表' 数据类型。
```coq
Inductive list (X:Type) : Type :=
  | nil
  | cons (x : X) (l : list X).
```

`list` 是个从 `Type` 到 `Type` 的函数。
```coq
Check list : Type -> Type.
Check nil : forall X : Type, list X.
Check cons : forall X : Type, X -> list X -> list X.
```

要使用多态函数，我们需要为其参数再额外传入一个或更多类型。 例如，前面 repeat 函数体中的递归调用必须传递类型 `X`。幸运的是，Coq 允许我们避免这种冗余。在任何我们可以写类型参数的地方，我们都可 以将类型参数写为 "洞" _，可以看做是说 "请 Coq 自行找出这里应该填什么。更确切地说，当 Coq 遇到 `_` 时，它会尝试 **统一 (unify)** 所有的局部变量信息，包括函数应当应用到的类型，其它参数的类型，以及应用函数的上下文中期望的类型， 以此来确定 `_` 处应当填入的具体类型。

这听起来很像类型标注推断。实际上，这两种个过程依赖于同样的底层机制。我们可以这样写 `repeat' (X : _) (x : _) (count : _) : list X :=`

我们甚至可以通过告诉 Coq '总是' 推断给定函数的类型参数来在大多数情况下 直接避免写 `_`。\
`Arguments` 用于指令指定函数或构造子的名字并列出其参数名， 花括号中的任何参数都会被视作隐式参数。（如果定义中的某个参数没有名字， 那么它可以用通配模式 _ 来标记。这种情况常见于构造子中。）
```coq
Arguments nil {X}.
Arguments cons {X} _ _.
Arguments repeat {X} x count.
```
现在我们完全不用提供类型参数了: `Definition list123'' := cons 1 (cons 2 (cons 3 nil)).`

此外，我们还可以在定义函数时就声明隐式参数， 只需要将某个参数两边的圆括号换成花括号。例如：
```coq
Fixpoint repeat''' {X : Type} (x : X) (count : nat) : list X :=
  match count with
  | 0 ⇒ nil
  | S count' ⇒ cons x (repeat''' x count')
  end.
```
（注意我们现在甚至不必在 `repeat'''` 的递归调用中提供类型参数了， 实际上提供了反而是无效的，因为 Coq 并不想要它。）

此外，我们还可以在函数名前加上前缀 @ 来强制将隐式参数变成显式的：
```coq
Check @nil : forall X : Type, list X.
Definition mynil' := @nil nat.
```


## More Basic Tactics

所有构造子都是单射的， 而不同构造子构造出的值绝不可能相等

在 Coq 中进行归纳证明时，有时控制归纳假设的确切形式是十分重要的。 特别是，在调用 induction 策略前，我们有时需要用 intros 将假设从目标移到上下文中时要十分小心。

在 induction 之前做一些 intros 来获得更一般归纳假设并不总是奏效。 有时需要对量化的变量做一下 '重排'。例如，假设我们想要通过对 `m` 而非 `n` 进行归纳来证明 `double_injective`。我们可以先引入所有量化的变量，然后 '重新一般化（re-generalize）' 其中的一个或几个，选择性地从上下文中挑出几个变量并将它们放回证明目标的开始处。 用 generalize dependent 策略就能做到。

```coq
Proof.
  intros n m.
  (* n and m are both in the context *)
  generalize dependent n.
  ...
```

## Logic in Coq

Coq 是一门拥有 '类型' 的语言，也就是说，一切有意义的 表达式都具有一个相应的类型。逻辑表达也不例外，我们试图在 Coq 中证明的一切语句都有名为 `Prop` 的类型，即 '命题类型'。

除了拥有类型之外，命题还是 '一等的（First-Class）' 实体， 即在 Coq 的世界中，我们可以像操作其它实体那样操作命题。例如，我们可以用 `Definition` 为命题取名，就像为其它表达式取名一样。
```coq
Definition is_three (n : nat) : Prop :=
  n = 3.
```

相等关系运算符 `=` 也是一个返回 `Prop` 的函数。\
表达式 `n = m` 只是 `eq n m` 的语法糖（它使用 `Notation` 机制定义在 Coq 标准库中）。由于 `eq` 可被用于任何类型的元素，因此它也是多态的：
```coq
Check @eq : forall A : Type, A -> A -> Prop.
(* 注意我们写的是 @eq 而非 eq：eq 的类型参数 A 是隐式声明的，因此我们需要关掉隐式参数的类型推断以便看到 eq 的完整类型。*)
```

Coq 将 `¬ P` 定义为 `P -> False`，而 `False` 是在标准库中特别定义的矛盾性命题。\
除 `False` 外，Coq 的标准库中还定义了 `True`，一个明显真的命题。 为了证明它，我们使用了预定义的常量 I : True：
```coq
Lemma True_is_true : True.
Proof. apply I. Qed.
```

不等性是十分常见的否定句的例子，，它有一个特别的记法 x ≠ y：
```coq
Notation "x <> y" := (~(x = y)).
```

## Existential Quantification

为了证明形如 `∃ x, P` 的语句，我们必须证明 `P` 对于某些特定的 `x` 成立，这些特定的 `x` 被称作存在性的 '例证'。反之，如果我们的的上下文中有形如 `∃ x, P` 的存在前提， 可以将其解构得到一个例证 `x` 和一个陈述 `P` 对于 `x` 成立的前提。
```coq
Definition even x := ∃ n : nat, x = double n.
Lemma four_is_even : even 4.
Proof.
  unfold even. ∃ 2. reflexivity.
Qed.
```

## Applying Theorems to Arguments

Coq 的一个特性是， 它将' 证明' 本身也作为一等对象。\
对于几乎所有将定理名作为参数的策略而言，你都可以 "将定理作为函数" 来使用。 注意，定理应用与函数应用使用了同样的类型推导机制，所以你可以将通配符作为定理的参数， 或者为定理声明默认的隐式前提。

```coq
Lemma in_not_nil_42_take4 :
  forall l : list nat, In 42 l -> l ≠ [].
Proof.
  intros l H.
  apply (in_not_nil nat 42).
  apply H.
Qed.

Lemma in_not_nil_42_take5 :
  forall l : list nat, In 42 l -> l ≠ [].
Proof.
  intros l H.
  apply (in_not_nil _ _ _ H).
Qed.
```

## Coq vs. Set Theory

Coq 的逻辑核心，即 '归纳构造演算（Calculus of Inductive Constructions）' 系统， 在很多重要的方面不同于数学家用来写下精确而严谨的定义和证明的形式化系统。 例如，在主流的纸笔数学家中使用最普遍的 '策梅洛 - 弗兰克尔集合论（ZFC）' 中， 一个数学对象可同时属于不同的集合；而在 Coq 的逻辑中，一个项最多只属于一个类型。

在一般的数学研究中，对于任意两个函数 f 和 g， 只要它们对相同的输入产生相等的结果，那么它们就被认为相等：
```
(forall x, f x = g x) -> f = g
```
This is known as the principle of functional extensionality. Thus, functional extensionality simply means that a function's identity is completely determined by what we can observe from it 

我们可以用 Axiom 指令将函数的外延性添加到 Coq 的核心逻辑系统中。
```coq
Axiom functional_extensionality : forall {X Y: Type}
                                    {f g : X -> Y},
  (forall (x:X), f x = g x) -> f = g.
```

## Classical vs. Constructive Logic

以下推理原则即便符合直觉， 不过在 Coq 中它是不可证明的：

```coq
Definition excluded_middle := forall P : Prop,
  P ∨ ~ P.
```

一般的排中律在 Coq 中默认并不可用，像 Coq 一样不假设排中律成立的逻辑系统被称作 '构造逻辑'。

以下四条陈述的任一条，以及 excluded_middle 可以认为刻画了经典逻辑。
```coq
Definition peirce := forall P Q: Prop,
  ((P -> Q) -> P) -> P.

Definition double_negation_elimination := forall P: Prop,
  ~~P -> P.

Definition de_morgan_not_and_not := forall P Q: Prop,
  ~(~P /\ ~Q) -> P \/ Q.

Definition implies_to_or := forall P Q: Prop,
  (P -> Q) -> (~P \/ Q).
```

证明如下（证明一条将它们连接起来的单向蕴含环链。
```
(* excluded_middle -> peirce: 直接展开即可
peirce -> excluded_middle: 排中律可看作 perice 的 Q 为 ~(P \/ ~P) 时的情况 *)
Theorem exc_mid_peirce:
  excluded_middle <-> peirce.
Proof. intros. unfold excluded_middle, peirce. split.
  - intros. destruct (H P).
    + assumption.
    + apply H0. intros. unfold not in H1. apply H1 in H2. contradiction.
  - intros. apply (H _ (~(P \/ ~P))). unfold not. intro. right. intro. 
    apply H0; left; assumption.
Qed.
```

## Inductively Defined Propositions

可将偶数性质的定义翻译为在 Coq 中使用 `Inductive` 声明的定义， 声明中每一个构造子对应一个推断规则：
```coq
Inductive ev : nat -> Prop :=
| ev_0 : ev 0
| ev_SS (n : nat) (H : ev n) : ev (S (S n)).
```

在 Coq 中，我们可以认为 `ev` 定义了一个性质 `ev : nat -> Prop`，其包括 "证据构造子" `ev_0 : ev 0` 和 `ev_SS : forall n, ev n -> ev (S (S n))`。这些 "证据构造子" 等同于已经证明过的定理。 具体来说，我们可以使用 Coq 中的 `apply` 策略和规则名称来证明某个数的 `ev` 性质
```
Theorem ev_4 : ev 4.
Proof. apply ev_SS. apply ev_SS. apply ev_0. Qed.
```


在 `Inductive` 定义中，类型构造子冒号左侧的参数叫做形参（Parameter）， 而右侧的叫做索引（Index）或注解（Annotation）\
`ev` 中的 `nat` 参数出现在冒号'右侧'，这允许在不同的构造子类型中使用不同的值：例如 `ev_0` 类型中的 `0` 以及 `ev_SS` 类型中的 `S (S n)`。与此相应， 每个构造子的类型必须在冒号后显式指定

对比 `list` 的定义
```coq
Inductive list (X:Type) : Type :=
    | nil
    | cons (x : X) (l : list X).
```
它以 '全局的方式' 在冒号 '左侧' 引入了参数 `X`， 强迫 `nil` 和 `cons` 的结果为同一个类型`list X`。如果在定义 `ev` 时将 `nat` 置于冒号左侧，就会得到如下错误：
```coq
Fail Inductive wrong_ev (n : nat) : Prop :=
| wrong_ev_0 : wrong_ev 0
| wrong_ev_SS (H: wrong_ev n) : wrong_ev (S (S n)).
(* ===> Error: Last occurrence of "wrong_ev" must have "n"
        as 1st argument in "wrong_ev 0". *)
```
`wrong_ev_0` 与 `wrong_ev_SS` 的类型不一致，前者是 wrong_ev 0 而后者不是。

## Inversion on Evidence

对于这类证据使用 '归纳（induction）' 和' 分类讨论（case analysis）' 来进行论证是可行的。

用 destruct 解构证据即可证明下述定理（这就是对证明进行反演）：
```coq
Theorem ev_minus2 : forall n,
  ev n -> ev (pred (pred n)).
Proof.
  intros n E.
  destruct E as [| n' E'].
  - (* E = ev_0 *) simpl. apply ev_0.
  - (* E = ev_SS n' E' *) simpl. apply E'.
Qed.
```

考虑另一种情况
```coq
Theorem evSS_ev : forall n,
  ev (S (S n)) → even n.
Proof.
  intros n E.
  destruct E as [| n' E'] eqn:EE.
  - (* E = ev_0. *)
    (* 我们须证明 n 是偶数，但没有任何有用的假设信息可以使用！ *)
Abort.
```

这里 `destruct E` 之后，`ev (S (S n))` 中的 `S (S n)` 在假设中被"忘记"了，并为 `ev_0` 的情况生成了子目标，如果我们记住 `S (S n)` 的话就可以利用爆炸原理 `0 = S (S n)` 证得该子目标：
```coq
Theorem evSS_ev_remember : ∀ n,
  ev (S (S n)) → ev n.
Proof.
  intros n H. remember (S (S n)) as k. destruct H as [|n' E'].
  - (* E = ev_0 *)
    (* Now we do have an assumption, in which k = S (S n) has been
       rewritten as 0 = S (S n) by destruct. That assumption
       gives us a contradiction. *)
    discriminate Heqk.
  - (* E = ev_S n' E' *)
    (* This time k = S (S n) has been rewritten as S (S n') = S (S n). *)
    injection Heqk as Heq. rewrite Heq in E'. apply E'.
Qed.
```

Alternatively, the proof is straightforward using our inversion lemma.

```
Theorem evSS_ev : ∀ n, ev (S (S n)) → ev n.
Proof. intros n H. apply ev_inversion in H. destruct H.
 - discriminate H.
 - destruct H as [n' [Hnm Hev]]. injection Hnm as Heq.
   rewrite Heq. apply Hev.
Qed.
```

`inversion` 的工作原理大致如下：
- 接受一个前提 `H`，该前提的类型 `P` 是通过归纳定义的，以及
- 对于 `P` 的定义里的每一个构造子 `C`，
    - 产生一个新的子目标，在该子目标中我们假设 `H` 是通过 `C` 构造的，
    - 作为额外的假设，在子目标的上下文中增加 `C` 的论据（前提），
    - 将 `C` 的结论（结果类型）与当前的目标相匹配，计算出为了能够应用 `C` 而必须成立的一些相等关系，将这些相等关系加入上下文中（以及，为了方便，在目标中替换它们），以及
    - 如果这些相等关系无法满足（例如，它们涉及到 S n = O），那么立即解决这个子目标。

## Induction on Evidence

我们可以用 `destruct` 进行分类讨论，同样我们也可以用 `induction` 进行归纳

## Inductive Relations

我们可以认为被一个数所参数化的命题（比如 ev）是一个 '性质'，也即， 它定义了 nat　的一个子集，其中的数可以被证明满足此命题。 以同样的方式，我们可认为有两个参数的命题是一个 '关系'，也即，它定义了一个 可满足此命题的序对集合。

## remember tactics

在 Coq 中调用 `remember e as x` 策略会\
(1) 替换所有表达式 `e` 为变量 `x` 
(2) 在当前上下文中添加一个等式 `x = e`。

// TODO

## Improving Reflection

// TODO 

## The Curry-Howard Correspondence

该同构启发很多看问题的新方法。例如考虑偶数的 `inductive` 定义：
```
Inductive ev : nat -> Prop :=
  | ev_0 : ev 0
  | ev_SS : forall n, ev n -> ev (S (S n)).
```

`ev` 定义中第二行的 `ev_0 : ev 0` 可以读作 "ev_0 是 ev 0 的证明" 也可以读作 "ev_0 的类型为 ev 0"。\
`Check ev_SS : forall n, ev n -> ev (S (S n)).` 可以将其读作 "ev_SS 构造子接受两个参数 —— 数字 n 以及命题 ev n 的证明 —— 并产生 ev (S (S n)) 的证明。"

既在类型层面表达 "具有 ... 类型"，又在命题层面表示 "是 ... 的证明"。 这种双关称为 '柯里 - 霍华德同构（Curry-Howard correspondence）'。 

---

有关 `ev` 的一个证明, `Theorem ev_4 : ev 4. Proof. apply ev_SS. apply ev_SS. apply ev_0. Qed.` 产生了一个 **'证据对象' (proof object)**\
实际上，我们也可以不借助脚本 '直接' 写出表达式作为证明。`Check (ev_SS 2 (ev_SS 0 ev_0)) : ev 4.`

---

在 Coq 的计算世界里（即所有的数据结构和程序存在的地方），有两种值的 类型中拥有箭头：一种是 '构造子 (Constructor)'，它通过归纳地定义数据类型 引入，另一种是 '函数 (Function)'。

类似地，在 Coq 的逻辑世界里（即我们运用证明的地方），有两种方式来给与蕴含式需要的证据：构造子，通过归纳地定义命题引入，和函数
- 考虑下列陈述：`Theorem ev_plus4 : forall n, ev n → ev (4 + n).` 我们在寻找一个 '类型 (Type)' 是 `forall n, ev n -> ev (4 + n)` 的表达式 —— 也就是说，一个接受两个参数（一个数字和一个证据）并返回一个证据的 '函数 (Function)'

---

到蕴含式（->）和量化（forall）都表示证据上的函数。事实上，他们是同一个东西：当我们使用 `∀` 时没有依赖，就可以简写为当 `->`。即，我们没有必要给与箭头左边的类型一个名字：`forall (x:nat), nat = forall (_: nat), nat = nat -> nat`\
总的来说，"P -> Q"只是"forall (_:P), Q" 的语法糖。

```coq
Definition conj_fact : forall P Q R, P /\ Q -> Q /\ R -> P /\ R := 
  fun P Q R (H1: P /\ Q) (H2: Q /\ R) => match H1 with 
    | conj P _ => match H2 with 
      | conj _ R => conj P R
    end
  end.
```

---

归纳定义足够用于表达我们目前为止遇到的大多数的联结词。事实上， 只有全称量化（以及作为特殊情况的蕴含式）是 Coq 内置的，所有其他的都是被归纳 定义的。

```coq
Inductive and (A B:Prop) : Prop :=
  conj : A -> B -> A /\ B

Inductive or (A B:Prop) : Prop :=
  | or_introl : A -> A \/ B
  | or_intror : B -> A \/ B
where "A \/ B" := (or A B) : type_scope.

Inductive ex {A : Type} (P : A → Prop) : Prop :=
| ex_intro : forall x : A, P x -> ex P.

Inductive True : Prop :=
  I : True.

(* False 是一个 '没有' 构造子的归纳类型 -- 即，没有任何方式能够构造一个它的证明。 *)
Inductive False : Prop :=.

Definition not (A:Prop) := A -> False.

Inductive ex (A:Type) (P:A -> Prop) : Prop :=
  ex_intro : forall x:A, P x -> ex (A:=A) P.
```

在 Coq 里，甚至连相等关系都不是内置的。它拥有如下的归纳定义。
```coq
Inductive eq (A: Type) (x: A) : A -> Prop :=
    eq_refl : x = x :>A
```

事实上，相等关系的归纳定义和 Leibniz 相等关系是 '等价的 (equivalent)'。
```coq
Lemma leibniz_equality_equality : forall (X : Type) (x y: X),
  (forall P: X -> Prop, P x -> P y) -> x = y.
Proof.
  intros. apply (H (fun z => x = z)). apply eq_refl.
Qed.
```

## IndPrinciples

每当我们使用 Inductive 来声明数据类型时，Coq 就会自动为该类型生成 '归纳法则'。通常，为归纳类型 t 生成的归纳法则形式如下：每个构造子 c 都会生成归纳法则中的一种情况
- 若 c 不接受参数，该情况为: 
    - P 对 c 成立
- 若 c 接受参数 `x1:a1 ... xn:an`，该情况为：
    - 对于所有的 `x1:a1 ... xn:an`，若 `P` 对每个类型为 `t` 的参数都成立，则 `P` 对于 `c x1 ... xn` 成立

例如对归纳定义的自然数和多态列表
```coq
Check nat_ind.
(* ===>
   nat_ind : forall P : nat -> Prop,
      P 0 ->
      (forall n : nat, P n -> P (S n)) ->
      forall n : nat, P n  *)

Inductive list (X:Type) : Type :=
    | nil : list X
    | cons : X -> list X -> list X.

Check list_ind.
(* list_ind: forall (X : Type) (P : list X -> Prop),
       P (nil X) ->
       (forall (x : X) (l : list X), P l -> P (cons X x l)) ->
       forall l : list X, P  *)
```

There's nothing magic about this induction lemma: it's just another Coq lemma that requires a proof. Coq generates the proof automatically too...

```coq
Definition nat_id := 
    fun (P : nat -> Prop)
        (* Suppose we have evidence f that P holds on 0 *)
        (f : P 0)
        (* and evidence f0 that ∀ n:nat, P n -> P (S n) *)
        (f0 : forall n : nat, P n -> P (S n)) => 
        (* Then we can prove that P holds of an arbitrary nat n via a recursive function F  *)
        (* (here defined using the expression form Fix rather than by a top-level Fixpoint declaration) *)
            fix F (n : nat) : P n := match n with
                | 0 => f
                | S n0 => f0 n0 (F n0)
            end.
```

We can adapt this approach to proving nat_ind to help prove non-standard induction principles too. 例如证明 `Lemma evenb_ev : ∀ n: nat, evenb n = true -> ev n.` 时，偶数是两个一跳的，对 `n` 进行归纳对于 `evenb` 并没有多大作用。Attempts to prove this by standard induction on n fail in the case for S (S n), because the induction hypothesis only tells us something about S n, which is useless.

But we can make a much better proof by defining and proving a non-standard induction principle that goes "by twos":
```coq
Definition nat_ind2 : forall (P : nat -> Prop),
    P 0 ->
    P 1 ->
    (forall n : nat, P n -> P (S(S n))) ->
    forall n : nat , P n := fun P P0 P1 PSS =>
        fix f (n:nat) := match n with
            | 0 => P0
            | 1 => P1
            | S (S n') => PSS n' (f n')
        end.

Fixpoint evenb (n:nat) : bool := match n with
    | O        => true
    | S O      => false
    | S (S n') => evenb n'
end.

Inductive ev : nat -> Prop :=
    | ev_0 : ev 0
    | ev_SS (n : nat) (H : ev n) : ev (S (S n)).


Lemma evenb_ev : forall n, evenb n = true -> ev n.
Proof.
    intros. induction n as [ | |n'] using nat_ind2.
        - apply ev_0.
        - simpl in H. inversion H.
        - simpl in H. apply ev_SS. apply IHn'. apply H.
Qed.
```

Coq 也为归纳定义的 '命题' 生成归纳法则，例如我们要对具有性质 `even` 的自然数归纳证明证明 `P` 
```coq
Inductive even : nat -> Prop :=
    | ev_0 : even 0
    | ev_SS (n : nat) : even n -> even (S (S n)).

Check even_ind.
(* even_ind: forall P : nat -> Prop,
       P 0 ->
       (forall n : nat, even n -> P n -> P (S (S n))) ->
       forall n : nat, even n -> P  *)
```  