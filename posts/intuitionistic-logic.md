---
title: Intuitionist Logic
---

基于经典逻辑，我们可以 "非构造地" 证明一个命题。例如：

Theorem: 存在两个无理数 $x, y$ ，使得 $x ^ y$ 为有理数\
Proof: 如果 $\sqrt{2}^{\sqrt{2}}$ 是有理数，那么我们可以取 $x=y=\sqrt{2}$ ，否则可以取 $x=\sqrt{2}^{\sqrt{2}}, y=\sqrt{2}$

上面的证明虽然在经典逻辑里没有问题，但我们仍无法确定究竟哪一种情况是正确的。除此之外，我们还可以做出一个构造性证明 (constructive proof): \
对于 $x = \sqrt{2}, y = 2 \log _{2} 3$ ，我们有 $x^{y}=3 \in Q$.

这种 "构造式" 的推理方式对应着 "直觉主义逻辑"（Intuitionist logic）。

### BHK 释义（The BHK interpretation）

直觉主义命题逻辑，或称直觉主义命题演算（Intuitionist propositional calculus, IPC），的语言和经典命题逻辑的语言是一样的。

**Definition:** 假设一个命题变量 (propositional variables) 无限集合 $PV$，我们定义逻辑式（formulas）的集合 $\Phi$ 为满足下列条件的最小集合
- 所有谓词变量和常量 $\perp$ （谬）都是 $\Phi$ 的元素；变量和常量被称为原子式（atomic formulas）。
- 如果 $\phi, \psi \in \Phi$ ，那么 $(\phi \rightarrow \psi),(\phi \vee \psi),(\phi \wedge \psi) \in \Phi$

---

**Definition:** 否定、等价和真（truth）定义如下：
- $\neg \phi \equiv_{d f} \phi \rightarrow \perp$
- $\phi \leftrightarrow \psi \equiv_{d f}(\phi \rightarrow \psi) \wedge(\psi \rightarrow \phi)$
- $\top \equiv{ }_{d f} \perp \rightarrow \perp$

直觉主义逻辑里的语义不是通过真值表来判断的，而是通过构建模式来解释的。这就是著名的 BHK 释义（Brouwer-Heyting-Kolmogorov interpretation）
- $\phi_{1} \wedge \phi_{2}$ 的构造（或证明）包含 $\phi_{1}$ 的构造和 $\phi_{2}$ 的构造；
- $\phi_{1} \vee \phi_{2}$ 的构造包含一个指数（indicator） $i \in\{1,2\}$ 和一个 $\phi_{i}$ 的构造；
- $\phi_{1} \rightarrow \phi_{2}$ 的构造是一个把每一个 $\phi_{1}$ 的构造都转换为 $\phi_{2}$ 的构造的函数（方法）；
- 不存在 $\perp$ 的构造。

```
Section BHK_interpretable.

Hypotheses P Q R:Prop.

Theorem bot_to_p: False -> P.
Proof. intros. contradiction. Qed.

Theorem neglect_Q: P -> Q -> P.
Proof. intros. assumption. Qed.

Theorem pqr: (P -> Q -> R) -> (P -> Q) -> P -> R.
Proof. intros. auto. Qed.

Theorem doub_neg: P -> ~~P.
Proof. unfold not. intros. apply H0 in H. assumption. Qed.

Theorem doub_neg_elim: ~~~P -> ~P.
Proof. unfold not. intros. apply H. intros. apply H1 in H0. assumption. Qed.

Theorem contra_imp:(P -> Q) -> (~Q -> ~P).
Proof. unfold not. intros. apply H0. apply H in H1. assumption. Qed.

Theorem vee_distr: ~(P \/ Q) <->  (~P /\ ~Q).
Proof. split; unfold not.
    - intros. split. 
        + intros. destruct H. left. assumption.
        + intros. destruct H. right. assumption.
    - intros. destruct H. destruct H0.
        + apply H. assumption.
        + apply H1. assumption.
Qed.

Theorem sum_arrow:((P /\ Q) -> R) <-> (P -> (Q -> R)).
Proof. split; intros; tauto. Qed.

Theorem ex_mid_doub_neg: ~~(P \/ ~P).
Proof. unfold not. intro. apply H. right. intro. apply H. left. assumption. Qed.

Theorem ex_mid_impl: (P \/ ~P) -> ~~P -> P.
Proof. unfold not. intros.  destruct H.
    - assumption.
    - apply H0 in H. contradiction.
Qed.

End BHK_interpretable.
```

### 自然演绎（natural deduction）

直觉主义逻辑的一个证明系统是自然演绎系统 $\mathrm{NJ}(\rightarrow, \perp, \wedge, \vee)$ 。其中 J 代表直觉主义逻辑。NK 是经典逻辑的自然演绎系统。

**Definition:** 自然演绎里的 **判断（judgement）** 是一个对子，写作 $\Gamma \vdash \phi$ ，读作 “Gamma 证明 phi”，包括一个有限逻辑式集合 $\Gamma$ 和一个逻辑式 $\phi$（这里的 $\phi$ 不是空集）.

$\Gamma \vdash \phi$ 的形式证明（proof）或推导（derivation）是一个满足下列条件的有限判断树：
- 根标记为 $\Gamma \vdash \phi$ ；
- 所有的叶子都是公理（公设，axioms），即形如 $\Gamma, \phi \vdash \phi$ 的判断；
- 其他节点的符号都可以从子节点借助定义的法则得到

:h3 经典逻辑的代数语义学

**Definition:** 布尔代数（Boolean algebra）是有顶端和底端元素的 lattice $B$ ，$B$ 中的每一个元素 $a$ 都有一个补（记为 $-a$ ）。
布尔代数通常记为 $B =\langle B, \sqcup, \sqcap,-0,1\rangle$ ，其中 $a \leq b \Longleftrightarrow a \sqcap b=a$.

:a[TODO]{href=https://zhuanlan.zhihu.com/p/33228589 .nav}
