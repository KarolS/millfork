[< back to index](../index.md)

# Using Z80 assembly within Millfork programs

The compiler does not yet support Z80 assembly. This will be remedied in the future.

## Safe assembly

Since assembly gives the programmer unlimited access to all machine features, 
certain assumptions about the code may be broken. 
In order to make assembly cooperate with the rest of the Millfork code, 
it should abide to the following rules:

* don't change the IX register

* don't jump between functions if either of functions has stack variables

* don't do `RET`, `RETI` or `RETN` if the function has stack variables

* don't jump or call things that are not functions or labels

* don't store data in locations other than variables or arrays

* don't change the stack pointer

* end non-inline assembly functions with `RET`, `JP`, `RETI` or `RETN` as appropriate

The above list is not exhaustive.
