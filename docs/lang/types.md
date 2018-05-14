[< back to index](../index.md)

# Types

Millfork puts extra limitations on which types can be used in which contexts.

## Numeric types

* `byte` – 1-byte value of undefined signedness, defaulting to unsigned

* `word` – 2-byte value of undefined signedness, defaulting to unsigned

* `farword` – 4-byte value of undefined signedness, defaulting to unsigned  
(the name is an analogy to a future 24-bit type called `farpointer`)

* `long` – 4-byte value of undefined signedness, defaulting to unsigned

* `sbyte` – signed 1-byte value

* `ubyte` – unsigned 1-byte value

* `pointer` – the same as `word`, but variables of this type default to be zero-page-allocated
and you can index `pointer` variables (not arbitrary `pointer`-typed expressions though, `f()[0]` won't compile)

Functions cannot return types longer than 2 bytes. 
There's also no reason to make a function return `pointer`, since to dereference it, 
you need to put it in a variable first anyway.

Numeric types can be converted automatically:

* from a smaller type to a bigger type (`byte`→`word`)

* from a type of undefined signedness to a type of defined signedness (`byte`→`sbyte`)

* from a type of defined signedness to a type of undefined signedness (`sbyte`→`byte`)

## Boolean types

TODO

## Special types

* `void` – a unit type containing no information, can be only used as a return type for a function.
