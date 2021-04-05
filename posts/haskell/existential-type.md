---
title: Haskell | Existential Type
---

Normally, any type variable appearing on the right must also appear on the left:

```haskell
-- data Worker x y = Worker {buffer :: b, input :: x, output :: y}
--     error: Not in scope: type variable 'b'
data Worker b x y = Worker {buffer :: b, input :: x, output :: y}

-- suppose that a Worker can use any type 'b' so long as it belongs to some particular class. 
-- Then every function that uses a Worker will have a type like
foo :: (Buffer b) => Worker b Int Int
```

Using existential types, we can avoid this:

```haskell
data Worker x y = forall b. Buffer b => Worker {buffer :: b, input :: x, output :: y}

foo :: Worker Int Int
```

This has a number of consequences. 
- First of all, it is now impossible for a function to demand a Worker having a specific type of buffer.
- Second, the type of foo can now be derived automatically without needing an explicit type signature.
- Thirdly, since code now has no idea what type the buffer function returns, you are more limited in what you can do to it.

