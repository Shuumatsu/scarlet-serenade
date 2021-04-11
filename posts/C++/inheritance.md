---
title: C++ | Inheritance 
---

## Inheritance Mode 

```c++
class Parent {
    int private_field = 0;

   public:
    int public_field = 0;

   protected:
    int protected_field = 0;
};

// public member of the base class will become public in the derived class
// protected members of the base class will become protected in derived class
class PublicInherit : public Parent {};

// both public member and protected members of the base class will become
// Private in derived class
class PrivateInherit : private Parent {};

// both public member and protected members of the base class will become
// protected in derived class
class ProtectedInherit : protected Parent {};
```

## Constructor & Destructor

- it will first call the constructor of the `Base` class and then the `Derived` class. (like placing a `super` call at the beginning of the constructor
- it will first call the destructor of the `Derived` class and then the `Base` class.