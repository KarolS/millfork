[< back to index](../doc_index.md)

# Variable storage

Variables in Millfork can belong to one of the following storage classes:

* static: all global variables; local variables declared with `static`

* stack: local variables declared with `stack`

* automatic: other local variables

* parameter: function parameters

Variables can also belong to one of the following memory areas 
(unless overridden with the `@` operator):

* zeropage: some variables and parameters with no defined memory segment, preferring pointers (6502-like targets only)

* high RAM: all the other variables and parameters

All arrays can be considered static.

## Static variables

Static variables have a fixed and unique memory location. 
Their lifetime is for the entire runtime of the program. 
If they do not have initial value declared, reading them before initialization yields an undefined value. 

## Stack variables

Stack variables, as their name suggests, live on the stack. 
Their lifetime starts with the beginning of the function they're in 
and ends when the function returns. 
They are not automatically initialized before reading, reading them before initialization yields an undefined value. 
The main advantage is that they are perfectly safe to use in reentrant code,
but the main disadvantages are:
 
* slower access

* bigger code

* increased stack usage

* their addresses are not considered constants and it's slower to get them

* cannot use them in inline assembly code blocks

The implementation depends on the target architecture:

* on 6502, the stack pointer is transferred into the X register and used as a base

* on 65816, the native stack-based addressing mode is used, similar to 6502, just without clobbering X

* alternatively, on both 6502 and 65816, you can use a software-based stack for stack variables:
    it's implemented as a separate 256-byte memory area plus a single byte for the stack pointer,
    exclusively for local stack variables; using it can help against stack overflows

* on 8080 and LR35902, the address is calculated from the stack pointer into the HL register pair

* on Z80, the IX register is used as the base pointer; unlike all the previous platforms,
this makes stack-allocated variables independent from other stack operations
and allows for optimizing them by inlining them into registers
(this can be disabled, so the 8080 method is used, or switched to use IY instead)

* on 8086, the BP register is used as the base pointer, behaving similarly to the IX register on Z80
(this can be disabled, so the 8080 method is used, with the target address register being BX instead of HL)

## Automatic variables

Automatic variables have lifetime starting with the beginning of the function they're in 
and ending when the function returns. 
Most automatic variables reside in memory. 
They can share their memory location with other automatic variables and parameters, 
to conserve memory usage. 
They are not automatically initialized before reading, reading them before initialization yields an undefined value. 
Automatic local variables are not safe to use with reentrant functions, see the [relevant documentation](../lang/reentrancy.md) for more details.

Some small automatic variables may be inlined to registers. 
On 6502-like architectures, only 1-byte variables may be inlined.
On 8080-like architectures, 1-byte and 2-byte variables may be inlined.

Automatic variables defined with the `register` keyword will have the priority when it comes to register allocation.

## Parameters

Function parameters have lifetime starting with the beginning 
of the function call to the function they're defined in 
and ending when the function returns. 
They reside in memory and can share their memory location with other parameters and automatic variables, 
to conserve memory usage. 
Unlike automatic variables, they are almost never inlined into registers.
Parameters are not safe to use with reentrant functions, see the [relevant documentation](../lang/reentrancy.md) for more details.


