[< back to index](../doc_index.md)

# Reentrancy

A function is called reentrant, 
when its execution can be interrupted and the function can be then safely called again.

When programming in Millfork, you need to distinguish conceptually three kinds of reentrant functions:

* nesting-safe

* recursion-safe

* interrupt-safe

As Millfork is a middle-level language, it leaves taking care of those issues to the programmer. 

## Nesting safety

Nesting occurs when a function is called when calculating parameters for another call of the same function:

    f(f(4))
    f(0, f(1,1))
    f(g(f(5))
    f(g()) // where g calls f, directly or indirectly

Since parameters are passed via global variables, 
calling a function while preparing parameters for another call to the same function may cause undefined behaviour.

For that reason, a function is considered nesting-safe if it has maximum one parameter.

It is possible to make a safe nested call to a non-nesting safe function, provided two conditions are met:

* the function cannot modify its parameters

* the non-nested parameters have to have the same values in all co-occurring calls: `f(5, f(5, 6, 7), 7)`

In all other cases, the nested call may cause undefined behaviour.
In such cases, it's recommended to introduce a temporary variable:

    tmp_f11 = f(1,1)
    f(0, tmp_f11)

## Recursion safety

A function is recursive if it calls itself, either directly or indirectly.

Since most automatic variables will be overwritten by the inner call, the function is recursive-safe if:

* parameters are no longer read after the recursive call is made

* an automatic variable is not read from without reinitialization after each recursive call

* all the other variables are stack variables

In all other cases, the recursive call may cause undefined behaviour.

The easiest, but suboptimal way to make a function recursion-safe is to make all local variables stack-allocated
and assign all parameters to variables as soon as possible. This is slow though, so don't do it unless really necessary.

Example:

    word fibonacci(byte i) {
       if i < 2 { return 1 }
       stack byte j
       j = i
       return fibonacci(j-1) + fibonacci(j-2)
    }

## Interrupt safety

A function is interrupt-safe if it can be safely called, either directly or indirectly,
simultaneously by the main code and by an interrupt routine.

The only way to make a function interrupt-safe is:

* have either no parameters, or just one parameter passed via registers that is immediately assigned to a local stack-allocated variable

* if there is a parameter: enable optimizations

* make all local variables stack-allocated,

* have a return type that can be returned via registers.

The size limit on the parameter and the return type depends on architecture:

* for 6502-like architectures: 2 bytes

* for 8080-like architectures: 4 bytes

All built-in functions and operators are designed to be interrupt-safe.

# Reentrancy safety violations

Each of the following things is a violation of reentrancy safety rules and will cause undefined behaviour with high probability:

* calling a non-nesting-safe function without extra precautions as above while preparing another call to that function

* calling a non-recursion-safe function from within itself recursively

* calling a non-interrupt-safe function from both the main code and an interrupt  