[< back to index](../doc_index.md)

# Operators

Unlike in high-level languages, operators in Millfork have limited applicability. 
Not every well-formed expression is actually compilable. 
Most expressions involving single bytes compile, 
but for larger types usually you need to use in-place modification operators.  
Further improvements to the compiler may increase the number of acceptable combinations. 

On 6502-like targets, certain expressions require the commandline flag `-fzp-register` (`.ini` equivalent: `zeropage_register`) to be enabled.
They will be marked with (zpreg) next to them. 
The flag is enabled by default, but you can disable it if you need to.

## Precedence

Millfork has different operator precedence compared to most other languages. From highest to lowest it goes:

* `->` and `[]`

* `*`, `*'`

* `+`, `+'`, `-`, `-'`, `|`, `&`, `^`, `>>`, `>>'`, `<<`, `<<'`, `>>>>`

* `:`

* `==`, `!=`, `<`, `>`, `<=`, `>=`

* `&&`

* `||`

* assignment and in-place modification operators

You cannot use two different operators at the same precedence levels without using parentheses to disambiguate. 
It is to prevent confusion about whether `a + b & c << d` means `(a + b) & (c << d)` `((a + b) & c) << d` or something else.   
The only exceptions are `+` and `-`, and `+'` and `-'`. 
They are interpreted as expected: `5 - 3 + 2 == 4` and `5 -' 3 +' 2 == 4`.  
Note that you cannot mix `+'` and `-'` with `+` and `-`. 

## Argument types

In the descriptions below, arguments to the operators are explained as follows:

* `enum` means any enumeration type

* `byte` means any numeric one-byte type

* `unsigned byte` means any numeric one-byte type that is not signed

* `word` means any numeric two-byte type, or a byte expanded to a word; `pointer` is considered to be numeric

* `long` means any numeric type longer than two bytes, or a shorter type expanded to such length to match the other argument

* `constant` means a compile-time constant

* `simple` means either: a constant, a non-stack variable,
a pointer indexed with a constant, a pointer indexed with a non-stack variable, 
an array indexed with a constant, an array indexed with a non-stack variable, 
an array indexed with a sum of a constant and a non-stack variable, 
or a split-word expression made of two simple expressions. 
Examples: `1`, `a`, `p[2]`, `p[i]`, `arr[2]`, `arr[i]`, `arr[i+2]`, `h:l`, `h[i]:l[i]`
Such expressions have the property that the only register they may clobber is Y.

* `mutable` means an expression that can be assigned to

## Split-word operator

Expressions of the shape `h:l` where `h` and `l` are of type byte, are considered expressions of type word.  
If and only if both `h` and `l` are assignable expressions, then `h:l` is also an assignable expression.

## Indirect field access operator

`->`

TODO 

## Binary arithmetic operators

* `+`, `-`:  
`byte + byte`  
`constant word + constant word`  
`constant long + constant long`  
`constant word + byte`  
`word + word` (zpreg)

* `*`: multiplication; the size of the result is the same as the size of the arguments  
`byte * constant byte`  
`constant byte * byte`  
`constant word * constant word`  
`constant long * constant long`  
`byte * byte` (zpreg)  
`word * byte` (zpreg)  
`byte * word` (zpreg)
`word * word` (zpreg)

* `/`, `%%`: unsigned division and unsigned modulo  
`unsigned byte / unsigned byte` (zpreg)  
`word / unsigned byte` (zpreg)  
`word / word` (zpreg)  
`constant word / constant word`  
`constant long / constant long`

## Bitwise operators

* `|`, `^`, `&`: OR, EXOR and AND  
`byte | byte`  
`constant word | constant word`  
`constant long | constant long`  
`word | word` (zpreg)

* `<<`, `>>`: bit shifting; shifting pads the result with zeroes  
`byte << byte`  
`word << byte` (zpreg)  
`constant word << constant byte`  
`constant long << constant byte`

* `>>>>`: shifting a 9-bit value and returning a byte; `a >>>> b` is equivalent to `(a & $1FF) >> b`    
`word >>>> constant byte`

## Decimal arithmetic operators

These operators work using the decimal arithmetic (packed BCD).

On Ricoh-based targets (e.g. Famicom) they require the zeropage register to have size at least 4

* `+'`, `-'`: decimal addition/subtraction  
`byte +' byte`  
`constant word +' constant word`  
`constant long +' constant long`  
`word +' word` (zpreg)

* `*'`: decimal multiplication  
`constant *' constant`

* `<<'`, `>>'`: decimal multiplication/division by power of two  
`byte <<' constant byte`

## Comparison operators

These operators (except for `!=`) can accept more than 2 arguments. 
In such case, the result is true if each comparison in the group is true.
Note you cannot mix those operators, so `a <= b < c` is not valid.

**WARNING:** Currently in cases like `a < f() < b`, `f()` may be evaluated an undefined number of times 
(the current implementation calls it twice, but do not rely on this behaviour). 

* `==`: equality  
`enum == enum`  
`byte == byte`  
`simple word == simple word`  
`word == constant`  
`simple long == simple long`

* `!=`: inequality  
`enum != enum`  
`byte != byte`  
`simple word != simple word`  
`word != constant`  
`simple long != simple long`

* `>`, `<`, `<=`, `>=`: inequality  
`byte > byte`  
`simple word > simple word`  
`simple long > simple long`

Currently, `>`, `<`, `<=`, `>=` operators perform signed comparison 
if any of the types of their arguments is signed,
and unsigned comparison otherwise.  

## Assignment and in-place modification operators

**WARNING:** Unlike other languages, Millfork does not provide any guarantees about how many times the left hand side will be evaluated. 
An expression of form `a[f()] += b` may call `f` an undefined number of times.

* `=`: normal assignment    
`mutable enum = enum`  
`mutable byte = byte`  
`mutable word = word`  
`mutable long = long`

* `+=`, `+'=`, `|=`, `^=`, `&=`: modification in place  
`mutable byte += byte`  
`mutable word += word`  
`mutable long += long`

* `<<=`, `>>=`: shift in place  
`mutable byte <<= byte`  
`mutable word <<= byte`  
`mutable long <<= byte`

* `<<'=`, `>>'=`: decimal shift in place  
`mutable byte <<'= constant byte`  
`mutable word <<'= constant byte`  
`mutable long <<'= constant byte`

* `-=`, `-'=`: subtraction in place  
`mutable byte -= byte`  
`mutable word -= simple word`  
`mutable long -= simple long`

* `*=`: multiplication in place  
`mutable byte *= constant byte`  
`mutable byte *= byte` (zpreg)  
`mutable word *= unsigned byte` (zpreg)
`mutable word *= word` (zpreg)

* `*'=`: decimal multiplication in place  
`mutable byte *'= constant byte`

* `/=`, `%%=`: unsigned division and modulo in place  
`mutable unsigned byte /= unsigned byte` (zpreg)  
`mutable word /= unsigned byte` (zpreg)
`mutable word /= word` (zpreg)

There are no `||=`, `&&=` or `>>>>=` operators.

## Indexing

While Millfork does not consider indexing an operator, this is a place as good as any to discuss it.

An expression of form `a[i]`, where `i` is an expression of type `byte`, is:

* when `a` is an array that has numeric index type and `T` value type:  
an access to the `i`-th element of the array `a`

* when `a` is a raw pointer variable:  
an access to the byte in memory at address `a + i`

* when `a` is a typed pointer variable to a 1-byte type `T`:  
an access to the value pointed to by `a`

* when `a` is a typed pointer variable to a 2-byte type `T` and `i` is zero:  
an access to the value pointed to by `a`

* otherwise: a compile error

On 8080-like targets, and on 6502 if the zeropage register is enabled, `i` can also be of type `word`.

An expression of form `a[i]`, where `i` is an expression of a enumeration type, is:

* when `a` is an array that has index type equal to the type of `i`:
an access to the element of the array `a` at the location assigned to the key `i`

* otherwise: a compile error

Note that you cannot access a whole array element if it's bigger than 2 bytes (except in a simple assignment),
but you can access its fields or take its pointer:

    array(int32) a[6]
    
    a[2]          // not ok
    a[2] = 4      // ok, assignments are an exception
    x = a[2]      // ok, assignments are an exception
    a[2].b0       // ok
    a[2].loword   // ok
    a[2].pointer  // ok     
    a[2].addr     // ok
    a[2].b0.addr  // ok, equal to the above on little-endian targets

## Built-in functions

* `not`: negation of a boolean expression  
`not(bool)`

* `nonet`: expansion of an 8-bit operation to a 9-bit operation  
`nonet(byte + byte)`  
`nonet(byte +' byte)`  
`nonet(byte << constant byte)`  
`nonet(byte <<' constant byte)`  
Other kinds of expressions than the above (even `nonet(byte + byte + byte)`) will not work as expected.

* `hi`, `lo`: most/least significant byte of a word  
`hi(word)`  
Furthermore, any type that can be assigned to a variable can be used to convert
either from one type either to another type of the same size,
or from a 1-byte integer type to a compatible 2-byte integer type.  
`byte` → `word`  
`word` → `pointer`  
some enum → `byte`  
`byte` → some enum  
but not  
`word` → `byte`  
some enum → `word`

* `sizeof`: size of the argument in bytes; the argument can be an expression or a type,
and the result is a constant of either `byte` or `word` type, depending on situation

* `call`: calls a function via a pointer;  
the first argument is the pointer to the function;  
the second argument, if present, is the argument to the called function.  
The function can have max one parameter, of size max 2 bytes, and may return a value of size max 2 bytes.
You can't create typed pointers to other kinds of functions anyway.  
If the pointed-to function returns a value, then the result of `call(...)` is the result of the function.  
Using `call` on 6502 targets requires at least 4 bytes of zeropage pseudoregister. 


