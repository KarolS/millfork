[< back to index](../index.md)

# Syntax

For information about types, see [Types](./types.md).  
For information about literals, see [Literals](./literals.md).  
For information about assembly, see [Using assembly within Millfork programs](./assembly.md).  

## Comments

Comments start with `//` and last until the end of line.

## Declarations


### Variable declarations

A variable declaration can happen at either top level of a file (*global* variables), 
or a top level of a function (*local* variables).

Syntax:

`[segment(<segment>)] [volatile] [<storage>] <type> <name> [@<address>] [= <initial_value>]`

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

For every variable `x` larger than a byte, extra subvariables are defined:

* if `x` is of type `word` or `pointer`: 

    * constituent bytes, from low to high: `x.lo`, `x.hi`

* if `x` is of type `farword`: 

    * constituent bytes, from low to high: `x.b0`, `x.b1`, `x.b2`
    
    * partial words: `x.loword` (=`x.b1:x.b0`), `x.hiword` (=`x.b2:x.b1`)

* if `x` is of type `long`: 

    * constituent bytes, from low to high: `x.b0`, `x.b1`, `x.b2`, `x.b3`
    
    * partial words: `x.loword` (=`x.b1:x.b0`), `x.hiword` (=`x.b3:x.b2`)

### Constant declarations

`const <type> <name> = <value>`

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
    // now the original f is removed and all calls to f will call g instead

### Array declarations

An array is a continuous sequence of bytes in memory.

Syntax:

`[segment(<segment>)] array <name> [[<size>]] [align ( <alignment> )] [@<address>] [= <initial_values>]`

* `<segment>`: segment name; if absent,
then defaults to `default_code_segment` as defined for the platform if the array has initial values,
or to `default` if it doesn't.

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

TODO

### Function declarations

A function can be declared at the top level. For more details, see [Functions](./functions.md)

## `import` statements

TODO

## Statements

### Expression statement

TODO

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
for <variable>,<start>,<direction>,<end> {
}
```

* `<variable>` – an already defined numeric variable

* `<direction>` – the range to traverse:

    * `to` – from `<start>` inclusive to `<end>` inclusive, in ascending order
    (e.g. `0,to,9` to traverse 0, 1,... 9)

    * `downto` – from `<start>` inclusive to `<end>` inclusive, in descending order
    (e.g. `9,downto,0` to traverse 9, 8,... 0)

    * `until` – from `<start>` inclusive to `<end>` exclusive, in ascending order
    (e.g. `0,until,10` to traverse 0, 1,... 9)
    
    * `parallelto` – the same as `to`, but the iterations may be executed in any order
    
    * `paralleluntil` – the same as `until`, but the iterations may be executed in any order
    
    There is no `paralleldownto`, because it would do the same as `parallelto`.
    
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
    
### `asm` statements

See [Using assembly within Millfork programs](./assembly.md).

  