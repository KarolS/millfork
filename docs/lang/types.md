[< back to index](../doc_index.md)

# Types

Millfork puts extra limitations on which types can be used in which contexts.

## Numeric types

* `byte` – 1-byte value of undefined signedness, defaulting to unsigned

* `word` – 2-byte value of undefined signedness, defaulting to unsigned
(alias: `int16`)

* `int24` – 3-byte value of undefined signedness, defaulting to unsigned
(alias: `farword`; this alias is deprecated and will be removed in the future)

* `long` – 4-byte value of undefined signedness, defaulting to unsigned
(alias: `int32`)

* `int40`, `int48`,... `int128` – even larger types

* `sbyte` – signed 1-byte value

* `ubyte` – unsigned 1-byte value

* `pointer` – raw pointers; the same as `word`, but variables of this type default to be zero-page-allocated
and you can index `pointer`-typed expressions.
You can create pointer values by suffixing `.addr` to the name of a variable, function or array.  

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

## Typed pointers

For every type `T`, there is a pointer type defined called `pointer.T`.

Unlike raw pointers, they are not subject to arithmetic.

If the type `T` is of size 1, you can index the pointer like a raw pointer.

If the type `T` is of size 2, you can index the pointer only with the constant 0.

Examples:

    pointer.t p
    p.raw       // expression of type pointer, pointing to the same location in memory as 'p'
    p.lo        // equivalent to 'p.raw.lo'
    p.hi        // equivalent to 'p.raw.lo'
    p[0]        // valid only if the type 't' is of size 1 or 2, accesses the pointed element
    p[i]        // valid only if the type 't' is of size 1, equivalent to 't(p.raw[i])'
    p->x        // valid only if the type 't' has a field called 'x', accesses the field 'x' of the pointed element
    p->x.y[0]->z[0][6]   // you can stack it
  
## `nullptr`

There is a 2-byte constant `nullptr` that can be assigned to any 2-byte pointer type.
Its actual value is defined using the feature `NULLPTR`, by default it's 0.

`nullptr` isn't directly assignable to non-pointer types. 

## Function pointers

For every type `A` of size 1 or 2 (or `void`) and every type `B` of size 1 or 2 (or `void`),
there is a pointer type defined called `function.A.to.B`, which represents functions with a signature like this:

    B function_name(A parameter)
    B function_name()  // if A is void
    
Examples:

    word i
    function.void.to.word p1 = f1.pointer
    i = call(p1)
    function.byte.to.byte p2 = f2.pointer
    i += call(p2, 7)
    function.word.to.byte p3 = f3.pointer
    i += call(p2, 7)
    
Using `call` on 6502 requires at least 4 bytes of zeropage pseudoregister. 

The value of the pointer `f.pointer` may not be the same as the value of the function address `f.addr`. 

## Boolean types

Boolean types can be used as conditions. They have two possible values, `true` and `false`, although 

* `bool` – a 1-byte boolean value. An uninitialized variable of type `bool` may contain an invalid value.

* several boolean types based on the CPU flags that may be used only as a return type for a function written in assembly:

    true if flag set | true if flag clear | 6502 flag | 8080 flag | Z80 flag | LR35902 flag  
    -----------------|--------------------|-----------|-----------|----------|-------------  
    `set_carry`      | `clear_carry`      | C         | C         | C        | C  
    `set_zero`       | `clear_zero`       | Z         | Z         | Z        | Z  
    `set_overflow`   | `clear_overflow`   | V         | P¹        | P/V      | _n/a_²  
    `set_negative`   | `clear_negative`   | N         | S         | S        | _n/a_²  

    1\. 8080 does not have a dedicated overflow flag, so since Z80 reuses the P flag for overflow,
    8080 uses the same type names for compatibility.
    
    2\. LR35902 does not support these types due to the lack of appropriate flags

Examples:

    bool f() = true
    
    void do_thing(bool b) { 
        if b { do_one_thing() } 
        else { do_another_thing() }
    }
    
    asm set_carry always_true() {
    #if ARCH_6502
        SEC
        ? RTS
    #elseif ARCH_I80
        SCF
        ? RET
    #else
        #error
    #endif
    }

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

Assignment between numeric types and enumerations is not possible without an explicit type cast:

    enum E { EA, EB }
    byte b
    E e
    e = EA      // ok
    e = b       // won't compile
    b = e       // won't compile
    b = byte(e) // ok
    e = E(b)    // ok
    
    array a[E]  // E is plain, array has size 2
    a[0]        // won't compile
    a[EB]       // ok
    
    
Plain enumerations have their variants equal to `byte(0)` to `byte(<name>.count - 1)`.
    
Tip: You can use an enumeration with no variants as a strongly checked alternative byte type,
as there are no checks on values when converting bytes to enumeration values and vice versa.

## Structs

Struct is a compound type containing multiple fields of various types:

    struct <name> { <field definitions (type and name), separated by commas or newlines>}

A struct is represented in memory as a contiguous area of variables laid out one after another.

Struct can have a maximum size of 255 bytes. Larger structs are not supported.

You can access a field of a struct with the dot:

    struct point { word x, word y }
    
    point p
    p.x = 3
    p.y.lo = 4

Offsets are available as `structname.fieldname.offset`:

    pointer ptr
    ptr = p.addr
    ptr += point.y.offset
    // ptr points now at p.y
    
    // alternatively:
    ptr = p.y.addr

You can create constant expressions of struct types using so-called struct constructors, e.g.:

    point(5,6)

All arguments to the constructor must be constant.

## Unions

    union <name> { <field definitions (type and name), separated by commas or newlines>}

Unions are pretty similar to structs, with the difference that all fields of the union
start at the same point in memory and therefore overlap each other.

    struct point { byte x, byte y }
    union point_or_word { point p, word w }
    
    point_or_word u
    u.p.x = 0
    u.p.y = 0
    if u.w == 0 { ok() }

Offset constants are also available, but they're obviously all zero.

Unions currently do not have an equivalent of struct constructors. This may be improved on in the future.
