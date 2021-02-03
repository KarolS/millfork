[< back to index](../doc_index.md)

# Types

## Numeric types

Millfork puts extra limitations on which types can be used in which contexts.
1-byte arithmetic types work in every context.
2-byte arithmetic types work in context that are not overly complicated.
3-byte and larger types have limited capabilities.

* `byte` – 1-byte value of undefined signedness, defaulting to unsigned

* `word` – 2-byte value of undefined signedness, defaulting to unsigned
(alias: `int16`)

* `int24` – 3-byte value of undefined signedness, defaulting to unsigned
(alias: `farword`; this alias is deprecated and will be removed in the future)

* `long` – 4-byte value of undefined signedness, defaulting to unsigned
(alias: `int32`)

* `int40`, `int48`,... `int128` – even larger types of undefined signedness, defaulting to unsigned

* `sbyte` – signed 1-byte value (alias: `signed8`)

* `ubyte` – unsigned 1-byte value (alias: `unsigned8`)

* `signed16` – signed 2-byte value (experimental)

* `unsigned16` – unsigned 2-byte value (experimental)

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

Numeric types can be also converted explicitly from a smaller to an equal or bigger size.
This is useful in situations like preventing overflow or underflow,
or forcing zero extension or sign extension:

    byte a
    a = 30
    a * a         // expression of type byte, equals 132
    word(a) * a   // expression of type word, equals 900
    
    word x
    byte y
    y = $80
    x = y         // does zero extension and assigns value $0080
    x = sbyte(y)  // does sign extension and assigns value $FF80

You can also explicitly convert expressions of type `bool` to any numeric type.
`false` is converted to 0 and `true` is converted to 1.

    byte a,b,c
    a = 5
    b = byte(a == 4) // b is now equal to 0
    c = byte(a == 5) // c is now equal to 1

## Typed pointers

For every type `T`, there is a pointer type defined called `pointer.T`.

Unlike raw pointers, they are not subject to arithmetic.

You can index the pointer like a raw pointer or an array.
An expression like `p[n]` accesses the `n`th object in an array of consecutive objects pointed to by `p`.

You can create pointer values by suffixing `.pointer` to the name of a variable, function or array.

You can replace C-style pointer arithmetic by combining indexing and `.pointer`: C `p+5`, Millfork `p[5].pointer`.

You can use the typed pointer as a raw pointer by suffixing `.raw`.

Examples:

    pointer.t p
    p = t1.pointer // assigning a pointer
    p.raw       // expression of type pointer, pointing to the same location in memory as 'p'
    p.lo        // equivalent to 'p.raw.lo'
    p.hi        // equivalent to 'p.raw.lo'
    p[0]        // valid only if the type 't' is of size 1 or 2, accesses the pointed element
    p[i]        // accessing the ith element; if 'sizeof(t) == 1', then equivalent to 't(p.raw[i])'
    p->x        // valid only if the type 't' has a field called 'x', accesses the field 'x' of the pointed element
    p->x.y[0]->z[0][6]   // you can stack it
    p.raw += sizeof(t) // if p points to an element of an array, then advances it to the next element
  
## `nullptr`

There is a 2-byte constant `nullptr` that can be assigned to any 2-byte pointer type.
Its actual value is defined using the feature `NULLPTR`, by default it's 0.

`nullptr` isn't directly assignable to non-pointer types. 

## Function pointers

For every type `A` of size 1 or 2 (or `void`) and every type `B` of size 1 or 2 (or `void`),
there is a pointer type defined called `function.A.to.B`, which represents functions with a signature like this:

    B function_name(A parameter)
    B function_name()  // if A is void
    
To call a pointed-to function, use `call`. Examples:

    word i
    function.void.to.word p1 = f1.pointer
    i = call(p1)
    function.byte.to.byte p2 = f2.pointer
    i += call(p2, 7)
    function.word.to.byte p3 = f3.pointer
    i += call(p2, 7)
    
Using `call` on 6502 requires at least 4 bytes of zeropage pseudoregister. 

The value of the pointer `f.pointer` may not be the same as the value of the function address `f.addr`. 

## Interrupt handler pointers

Functions that are interrupt pointers have their own pointer types:

* `pointer.interrupt` for hardware interrupt handlers

* `pointer.kernal_interrupt` for kernal interrupt handlers

`pointer.kernal_interrupt` is automatically convertible to `function.void.to.void`

    interrupt void handler1(){}
    kernal_interrupt void handler2(){}
    
    pointer.interrupt p1
    p1 = handler1.pointer
    pointer.kernal_interrupt p2
    p2 = handler2.pointer
    function.void.to.void p3
    p3 = handler2.pointer

## Boolean types

Boolean types can be used as conditions. They have two possible values, `true` and `false`.

* `bool` – a 1-byte boolean value.
An uninitialized variable of type `bool` may contain an invalid value.
The value `false` is stored as 0, `true` as 1. 

* several boolean types based on the CPU flags that may be used only as a return type for a function written in assembly:

    true if flag set | true if flag clear | 6502 flag | 6809 flag | 8080 flag | Z80 flag | LR35902 flag  
    -----------------|--------------------|-----------|-----------|-----------|----------|-------------  
    `set_carry`      | `clear_carry`      | C         | C         | C         | C        | C  
    `set_zero`       | `clear_zero`       | Z         | Z         | Z         | Z        | Z  
    `set_overflow`   | `clear_overflow`   | V         | V         | P¹        | P/V      | *n/a*²  
    `set_negative`   | `clear_negative`   | N         | N         | S         | S        | *n/a*²  

    1\. 8080 does not have a dedicated overflow flag, so since Z80 reuses the P flag for overflow,
    8080 uses the same type names for compatibility.
    
    2\. LR35902 does not support these types due to the lack of appropriate flags

You can convert from a boolean type to an arithmetic type by simply casting:

    byte c = byte(x >= 0x80)

Examples:

    bool f() = true
    
    bool g(byte x) = x == 7 || x > 100
    
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
    #elseif ARCH_6809
        ORCC #1
        ? RTS
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
    
    enum X {}   // enum with no variants
    enum Y {    // enum with renumberedvariants
        YA = 5
        YB      // YB is internally represented as 6
    }         
    array a2[X] // won't compile
    array a2[Y] // won't compile
    
    
Plain enumerations have their variants equal to `byte(0)` to `byte(<name>.count - 1)`.
    
Tip: You can use an enumeration with no variants as a strongly checked alternative byte type,
as there are no checks on values when converting bytes to enumeration values and vice versa.

## Structs

Struct is a compound type containing multiple fields of various types:

    struct <name> [align (alignment)] { <field definitions (type and name), separated by commas or newlines>}

A struct is represented in memory as a contiguous area of variables laid out one after another.

Struct can have a maximum size of 255 bytes. Larger structs are not supported.

You can access a field of a struct with a dot:

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

Structures declared with an alignment are allocated at appropriate memory addresses.
The alignment has to be a power of two.  
If the structs are in an array, they are padded with unused bytes.
If the struct is smaller that its alignment, then arrays of it are faster

    struct a align(4) { byte x,byte y, byte z }
    struct b          { byte x,byte y, byte z }
    array(a) as [4] @ $C000
    array(b) bs [4] @ $C800
    
    a[1].addr - a[0].addr // equals 4
    b[1].addr - b[0].addr // equals 3
    sizeof(a) // equals 16
    sizeof(b) // equals 12
    
    return a[i].x // requires 22 or 24 cycles on 6502
    return b[i].x // requires 18 cycles on 6502
 
A struct that contains substructs or subunions with non-trivial alignments has its alignment equal
to the least common multiple of the alignments of the substructs and its own declared alignment.

## Unions

    union <name> [align (alignment)] { <field definitions (type and name), separated by commas or newlines>}

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
