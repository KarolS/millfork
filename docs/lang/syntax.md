[< back to index](../doc_index.md)

# Syntax

For information about types, see [Types](./types.md).  
For information about literals, see [Literals](./literals.md).  
For information about assembly, see [Using assembly within Millfork programs](./assembly.md).  

## Allowed characters

Source files are text files in UTF-8 encoding.  
Allowed line endings are U+000A, U+000D and U+000D/U+000A.  
Outside of text strings and comments, the only allowed characters are U+0009 and U+0020–U+007E
(so-called printable ASCII).

## Comments

Comments start with `//` and last until the end of line.

Inside assembly blocks, including assembly functions, you can alternatively start comments with `;`.
Such comments cannot contain braces.

## Declarations


### Variable declarations

A variable declaration can happen at either top level of a file (*global* variables), 
or a top level of a function (*local* variables).

Syntax:

`[segment(<segment>)] [volatile] [<storage>] <type> <name> [@<address>] [= <initial_value>]`

Examples:

    byte a
    volatile byte thing @ $D000
    int24 x = 7
    segment(s1) word w

* `<segment>`: segment name; if absent, then defaults to `default`.

* `volatile` means that the variable is volatile.
The optimizer shouldn't remove or reorder accesses to volatile variables.
Volatile variables cannot be declared as `register` or `stack.  

* `<storage>` can be only specified for local variables. It can be either `stack`, `static`, `register` or nothing.
`register` is only a hint for the optimizer. 
See [the description of variable storage](../abi/variable-storage.md).

* `<address>` is a constant expression that defines where in the memory the variable will be located. 
If not specified, it will be located according to the usual allocation rules. 
`stack` variables cannot have a defined address. 

* `<initial_value>` is a constant expression that contains the initial value of the variable.
Only global variables can be initialized that way.
The behaviour is undefined when targeting a ROM-based platform.

You can declare multiple variables in one declaration, for example:

    byte x, y @$c000, z=6, w
    
will declare 4 variables: uninitialized variables `x` and `w`, fixed-address variable `y` and initialized variable `z`.

For every variable `x` larger than a byte, extra subvariables are defined:

* if `x` is of type `word` or `pointer`: 

    * constituent bytes, from low to high: `x.lo`, `x.hi`

* if `x` is of type `int24`: 

    * constituent bytes, from low to high: `x.b0`, `x.b1`, `x.b2`
    
    * partial words: `x.loword` (=`x.b1:x.b0`), `x.hiword` (=`x.b2:x.b1`)

* if `x` is of type `long`: 

    * constituent bytes, from low to high: `x.b0`, `x.b1`, `x.b2`, `x.b3`
    
    * partial words: `x.loword` (=`x.b1:x.b0`), `x.hiword` (=`x.b3:x.b2`)
    
* if `x` is of a larger integral type:

    * constituent bytes, from low to high: `x.b0`, `x.b1`, `x.b2`, etc.
    
    * the lowest word: `x.loword` (=`x.b1:x.b0`)

### Constant declarations

`const <type> <name> = <value>`

Examples:

    const byte two = 2

TODO

### Alias definitions

`alias <alias> = <name> [!]`

Sets an alias for a global name.
Unless shadowed by a local name, the alias will point to the given global object:

    byte x
    alias a = x
    
    void f() {
        a = 5 // writes to the global variable x
    }
    
    void f() {
        byte a
        a = 5 // writes to the local variable a
    }

Aliases can be used for variables, arrays, constants, functions, and types,
but not for text encodings, array formats or keywords.

If the alias definition is followed by a `!`, then the alias overrides any other definition of that name.
This allows for overriding definitions of library functions by another library:

    void f() {}
    void g() {}
    alias f = g!
    // the original f is removed and all calls to f will call g instead

### Array declarations

An array is a continuous sequence of bytes in memory.

An array declaration can happen at either top level of a file (*global* arrays), 
or a top level of a function (*local* arrays).
Regardless of where they were declared, arrays are considered static.

Syntax:

`[segment(<segment>)] [const] array [(<element type>)] <name> [[<size>]] [align ( <alignment> )] [@<address>] [= <initial_values>]`

Examples:

    array results[8]
    array(word) words = [1,2,500]
    array page [256] align(256)
    segment(chrrom) const array graphics @ $0000 = file("tiles.chr")
    array(byte) identity = [for i,0,until,256 [i]]
    array text = "hello world"z
    const array(room) rooms = [room(1,2), room(3,5)]

* `<segment>`: segment name; if absent,
then defaults to `default_code_segment` as defined for the platform if the array has initial values,
or to `default` if it doesn't.

* if `const` is present, the array is read-only. Read-only arrays have to have a fixed address and/or defined contents.  
**COMPATIBILITY WARNING!** In Millfork 0.3.2 and earlier, arrays couldn't be declared const and on cartridge-based targets,
preinitialized arrays were assumed to be immutable and were allocated to ROM.
Since 0.3.4, only const arrays can be allocated to ROM, non-const arrays are allocated to RAM
and their contents are uninitialized before a call to `init_rw_memory`. See [the ROM vs RAM guide](../api/rom-vs-ram.md).

* `<element type>`: type of the elements of the array.
If omitted, the default is `byte`.

* `<size>`: either a constant number, which then defines the size of the array,
or a name of a plain enumeration type, in which case changes the type of the index to that enumeration
and declares the array size to be equal to the number of variants in that enumeration.
If the size is not specified here, then it's deduced from the `<initial_values>`.
If the declared size and the size deduced from the `<initial_values>` don't match, then an error is raised.

* `<alignment>` is either a numeric literal that is a power of 2, or keyword `fast`.
    The array will be allocated at the address divisible by alignment.
    `fast` means different things depending on the target platform:

    * on 6502, it means that the array will not cross a page boundary
    * on Z80, it means that the array will not cross a page boundary

* `<address>` is a constant expression that defines where in the memory the array is or will be located.

* `<initial_values>` is an array literal, see [Literals](./literals.md).
Local arrays can have initial values only if they're const.

Each array has an associated constant defined that contains its length
and, if the indices are numeric, another constant that contains the last index of the array:

    array x[5]
    x.length        // equals 5
    x.lastindex     // equals 4
    
    enum e { ... }
    array y[e]
    y.length        // equals e.count
    // y.lastindex  // doesn't exist
    

TODO

### Function declarations

A function can be declared at the top level. For more details, see [Functions](./functions.md)

### Segment blocks

Instead of repeating the segment name in multiple declarations, you can wrap them in a block:

    segment (s) {
        byte x
        word y    
    } 
    
is equivalent to:

    segment (s) byte x
    segment (s) word y

## `import` statements

    import <module>
    
Adds a module to the program.

The module is looked up first in the current working directory, and then in the include directories.

Usually, the imported module will undergo the first phase of compilation first.
This means that the constants in the imported module will be resolved first, allowing you to use them in the importing module.

The only exception to this rule is when the importing graph has a cycle, in which case the order of modules within the cycle is unspecified.

All starting modules are considered to be imported by all source files explicitly mentioned on the command line.  

## Statements

Statements are separated from each other with a new line, or with a semicolon followed by a new line.

You cannot put two statements on one line.

In statement blocks, the opening and closing braces do not need to be separated from the statements.

### Expression statement

TODO

See also [the operator reference](./operators.md)

### `if` statement

Syntax:

```
if <expression> {
    <body>
}
```

```
if <expression> {
    <body>
} else {
    <body>
}
```

```
if <expression> {
    <body>
} else if <expression> {
    <body>
} else {
    <body>
}
```

### `return` statement

Syntax:

```
return
```
```
return <expression>
```

### `return[]` statement (return dispatch)

Syntax examples:

```
return [a + b] {
   0   @ underflow
   255 @ overflow
   default @ nothing
}
```
```
return [getF()] {
   1 @ function1
   2 @ function2
   default(5) @ functionDefault
}
```
```
return [i] (param1, param2) {
   1,5,8 @ function1(4, 6)
   2     @ function2(9)
   default(0,20) @ functionDefault
}
```

Return dispatch calculates the value of an index, picks the correct branch, 
assigns some global variables and jumps to another function.

The index has to evaluate to a byte or to an enum. The functions cannot be `macro` and shouldn't have parameters. 
Jumping to a function with parameters gives those parameters undefined values.

The functions are not called, so they don't return to the function the return dispatch statement is in, but to its caller.
The return values are passed along. If the dispatching function has a non-`void` return type different that the type 
of the function dispatched to, the return value is undefined. 

If the `default` branch exists, then it is used for every missing index.
If the index type is an non-empty enum, then the default branch supports all the other values.
Otherwise, the `default` branch handles only the missing values between other supported values.
In this case, you can override it with optional parameters to `default`.
They specify the maximum, or both the minimum and maximum supported index value.
In the above examples: the first example supports values 0–255, second 1–5, and third 0–20.

If the index has an unsupported value, the behaviour is formally undefined, but in practice the program will simply crash.  

Before jumping to the function, the chosen global variables will be assigned parameter values. 
Variables have to be global byte-sized. Some simple array indexing expressions are also allowed.
Parameter values have to be constants.
For example, in the third example one of the following will happen:

* if `i` is 1, 5 or 8, then `param1` is assigned 4, `param2` is assigned 6 and then `function1` is called;

* if `i` is 2, then `param1` is assigned 9, `param2` is assigned an undefined value and then `function2` is called;

* if `i` is any other value from 0 to 20, then `param1` and `param2` are assigned undefined values and then `functionDefault` is called;

* if `i` has any other value, then undefined behaviour.

### `while` and `do-while` statements

Syntax:

```
while <expression> {
    <body>
}
```

```
do {
    <body>
} while <expression>
```

### `for` statements

**Warning: `for` loops are a bit buggy.**

Syntax:

```
for <variable> , <start> , <direction> , <end> {
}
for <variable> : <enum type> {
}
for <variable> : [ <comma separated expressions> ]  {
}
```

* `<variable>` – an already defined numeric variable

* `<direction>` – the type of range to traverse:

    * `to` – from `<start>` inclusive to `<end>` inclusive, in ascending order
    (e.g. `0,to,9` to traverse 0, 1,... 9)

    * `downto` – from `<start>` inclusive to `<end>` inclusive, in descending order
    (e.g. `9,downto,0` to traverse 9, 8,... 0)

    * `until` – from `<start>` inclusive to `<end>` exclusive, in ascending order
    (e.g. `0,until,10` to traverse 0, 1,... 9)
    
    * `parallelto` – the same as `to`, but the iterations may be executed in any order
    
    * `paralleluntil` – the same as `until`, but the iterations may be executed in any order
    
    There is no `paralleldownto`, because it would do the same as `parallelto` with swapped arguments.
    
    If:
    
    * the left argument to `until`, `paralleluntil`, `to` or `parallelto` is greater than the right argument
    
    * the left argument to `downto` is smaller than the right argument
    
    then the loop counter overflows and wraps around.
    For example, `for i,254,to,1` with `i` being a byte, iterates over 254, 255, 0, 1.
    
    If the arguments to `until` or `paralleluntil` are equal, zero iterations are executed.
    
    `<start>` and `<end>` may be evaluated an arbitrary number of times.
    It's recommended to use only constants, variables or other really simple expressions.
    

* `<enum type>` – traverse enum constants of given type, in arbitrary order

* `<comma separated expressions>` – traverse every value in the list, in the given order.
Values do not have to be constant.
If a value is not a constant and its value changes while executing the loop, the behaviour is undefined.
Jumps using `goto` across the scope of this kind of loop are disallowed.

### `break` and `continue` statements

Syntax:

```
break
break for
break while
break do
break <variable>
continue
continue for
continue while
continue do
continue <variable>
```

Labelless `break` and `continue` apply to the innermost `for`, `while` or `do-while` loop.

`break for`, `continue do` etc. apply to the innermost loop of the given type.

`break i` and `continue i` apply to the innermost `for` loop that uses the `i` variable.

### `goto` and `label`

Syntax:

```
goto <expression>
label <name>
```

The `label` statement defines a constant pointer that refers to the current position in the code.
Such labels are only visible in the scope of the local function.

The `goto` expression jumps to the pointer value of the expression.

Jumping using `goto` across the scope of `for` loop that uses a fixed list or across functions is not allowed.

Computed gotos are supported:

```
pointer p
p = x
goto p
label x
```
    
### `asm` statements

See [Using 6502 assembly within Millfork programs](./assembly.md)  
or [Using 8080/LR35902/Z80 assembly within Millfork programs](./assemblyz80.md).

**Work in progress**: For 8086, see the [8086 support disclaimer](./x86disclaimer.md).
