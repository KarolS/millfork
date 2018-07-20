[< back to index](../index.md)

# Types

Millfork puts extra limitations on which types can be used in which contexts.

## Numeric types

* `byte` – 1-byte value of undefined signedness, defaulting to unsigned

* `word` – 2-byte value of undefined signedness, defaulting to unsigned

* `farword` – 3-byte value of undefined signedness, defaulting to unsigned  
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

## Enumerations

Enumeration is a 1-byte type that represents a set of values:

    enum <name> { <variants, separated by commas or newlines> }
    
The first variant has value 0. Every next variant has a value increased by 1 compared to a previous one. 
 
Alternatively, a variant can be given a custom constant value, which will change the sequence.

If there is at least one variant and no variant is given a custom constant value,
then the enumeration is considered _plain_. Plain enumeration types can be used as array keys.
For plain enumerations, a constant `<name>.count` is defined,
equal to the number of variants in the enumeration.

Assigment between numeric types and enumerations is not possible without an explicit type cast:

    enum E {}
    byte b
    E e
    e = b       // won't compile
    b = e       // won't compile
    b = byte(e) // ok
    e = E(b)    // ok
    
Plain enumerations have their variants equal to `byte(0)` to `byte(<name>.count - 1)`.
    
Tip: You can use an enumeration with no variants as a strongly checked alternative byte type,
as there are no checks no values when converting bytes to enumeration values and vice versa.
