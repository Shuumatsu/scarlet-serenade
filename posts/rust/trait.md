---
title: Rust | Trait
---

## trait 在 rust 中可被用来实现 static dispatch

考虑下面的例子，这是一个有 trait 限制的泛型。

```rust
fn print_hash<T: Hash>(t: &T) {
    println!("The hash is {}", t.hash())
}

print_hash(&true);      // instantiates T = bool
print_hash(&12_i64);    // instantiates T = i64
```

当我们用不同的类型去调用这个函数的时候，编译器会生成不同的函数实例。Generics are compiled away, resulting in static dispatch. 

---

## trait 在 rust 中可被用来实现 dynamic dispatch

考虑这样的例子，如果只有 static dispatch，那么我们的 `listeners` 必须都是统一类型。

```rust
trait ClickCallback {
    fn on_click(&self, x: i64, y: i64);
}

struct Button<T: ClickCallback> {
    listeners: Vec<T>
}
```

但其实我们想要的是 a set of **heterogeneous** listeners。 我们可以用 trait object 做到这一点。
```rust
struct Button {
    listeners: Vec<Box<ClickCallback>>
}
```

我们要求它在指针背后是因为他们属于不同的类型，所有有着不同的大小，而我们的栈要求变量有一个编译期确定的大小。

> https://doc.rust-lang.org/std/raw/struct.TraitObject.html

```rust
pub struct TraitObject {
    pub data: *mut (), // 指向实际的类型 T 的实例
    pub vtable: *mut (), // pointing to the concrete piece of machine code for each method in the implementation
}
```