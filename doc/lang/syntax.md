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

`[<storage>] <type> <name> [@<address>] [= <initial_value>]`

* `<storage>` can be only specified for local variables. It can be either `stack`, `static` or nothing. 
See [the description of variable storage](../abi/variable-storage.md).

* `<address>` is a constant expression that defines where in the memory the variable will be located. 
If not specified, it will be located according to the usual allocation rules. 
`stack` variables cannot have a defined address. 

* `<initial_value>` is a constant expression that contains the initial value of the variable.
Only global variables can be initialized that way.
The behaviour is undefined when targeting a ROM-based platform.

### Constant declarations

`const <type> <name> = <value>`

TODO

### Array declarations

An array is a continuous sequence of bytes in memory.

Syntax:

`array <name> [[<size>]] [@<address>] [= <initial_values>]`

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
    
### `asm` statements

See [Using assembly within Millfork programs](./assembly.md).

  