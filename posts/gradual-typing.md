---
title: Gradual Typing
---

the gradual type checker 通过一个用 `?` 表示的 unknow type (also called the dynamic type) 来处理 unannotated variables。


the gradual type checker 在两个类型间允许 implicit conversion 如果他们 **consistent** with each other (用 `S ~ T` 来表示 S is consistent with T，用 `S !~ T` S is not consistent with T
1. 对任意类型 `T` 有 `? ~ T` and `T ~ ?`. 即允许 `?` 与任何类型的相互 conversions
2. 对任意 basic type `B`, such as int, 有 `B ~ B`.
3. tuple type `T1 * T2` is consistent with `S1 * S2` if `T1 ~ S1` and `T2 ~ S2`. 这个规则被推广到任意大小的 tuple 
4. 函数类型 `fun (T1, ..., Tn) => R)` is consistent with `fun (S1, ..., Sn) => U)` if `T1 ~ S1, ..., Tn ~ Sn` and `R ~ U`.

