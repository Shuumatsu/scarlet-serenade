### 消除歧义

没有通用的方法消除歧义，甚至 CFG 的歧义性判定都是 undecidable 的。

但是有些有些常见情况是需要掌握的

#### Dangling Else

如此定义的语法，在两个 if 接 一个 else 的情况下无法确定 else 是与哪一个 if 配对

```
statement 
    -> simple statement
    -> if statement
    -> "loop", statement

if statement
    -> "if", statement
    -> "if", statement, "else", statement 
```

在绝大多数的编程语言中，我们采用的规则是让 `else` 与 最近的未匹配的 `if` 配对
通过核心语句 `"if", closed statement, "else",`，其中 `closed statement` 表示完全配对的语句，来保证 else 一定与最近的未匹配的 `if` 配对

```
statement 
    -> open statement
    -> closed statement 

open statement
    -> "if", statement
    -> "if", closed statement, "else", open statement
    -> "loop", open statement

closed statement 
    -> "if", closed statement, "else", closed statement 
    -> "loop", closed statement
    -> simple statement
```

#### Arithmetic Expressions

例如带括号的加法乘法运算，通过设定优先级与结合性处理
让加法乘法运算都是左结合的：`a + b + c = (a + b) + c`
让乘法运算有更高的优先级：`a + b * c = a + (b * c)`

```
I -> a | b | Ia | I b | I0 | I1 // 不可拆分的原子项
F -> I | (E)                    // 括号可使任何东西变成原子项，所以包裹一个最低级的 E
T -> F | T * F                  // 表示 F 的项左结合
E -> T | E + T                  // 表示 E 的项左结合，对 + 来说 * 的项是不可拆分的
```







