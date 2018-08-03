[< back to index](../index.md)

# Types

Millfork puts extra limitations on which types can be used in which contexts.

## Numeric types

* `byte` – 1-byte value of undefined signedness, defaulting to unsigned

* `word` – 2-byte value of undefined signedness, defaulting to unsigned
(alias: `int16`)

* `int24` – 3-byte value of undefined signedness, defaulting to unsigned
(alias: `farword`; this alias is deprecated and might be removed in the future)

* `long` – 4-byte value of undefined signedness, defaulting to unsigned
(alias: `int32`)

* `int40`, `int48`,... `int128` – even larger types

* `sbyte` – signed 1-byte value

* `ubyte` – unsigned 1-byte value

* `pointer` – the same as `word`, but variables of this type default to be zero-page-allocated
and you can index `pointer` variables (not arbitrary `pointer`-typed expressions though, `f()[0]` won't compile)
You can create pointer values by suffixing `.addr` to the name of a variable, function or array.  
**Work in progress**:
There's no reason to make a function return `pointer` yet, since currently to dereference it, 
you need to put it in a variable first anyway.

You can access single bytes of variables by using the following notations:

* for 2-byte-sized variables: `.lo` for the least significant byte and `.hi` for the most significant byte

* for larger variables: `.b0` for the least significant byte and then `.b1`, `.b2` and so on

You can also access words that are parts of variables:

* for 3-byte-sized variables: `.loword` is the word formed from `.b1` and `.b0` and `.hiword` is the word formed from `.b2` and `.b1`

* for 4-byte-sized variables: `.loword` is the word formed from `.b1` and `.b0` and `.hiword` is the word formed from `.b3` and `.b2`

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
