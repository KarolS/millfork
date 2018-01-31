# Using assembly within Millfork programs

There are two ways to include raw assembly code in your Millfork programs:

* inline assembly code blocks

* whole assembly functions

## Assembly syntax

Millfork inline assembly uses the same three-letter opcodes as most other 6502 assemblers.
Indexing syntax is also the same. Only instructions available on the current CPU architecture are available.

Currently, `RMBx`/`SMBx`/`BBRx`/`BBSx` are not supported yet.

Undocumented instructions are supported using various opcodes

Labels have to be followed by a colon and they can optionally be on a separate line:

    first:  INC x
    second: 
            INC y


Label names have to start with a letter and can contain digits, underscores and letters.
This means than they cannot start with a period like in many other assemblers.
Similarly, anonymous labels designated with `+` or `-` are also not supported

Labels are global, 
which means that they live in the same namespace as functions, types and global variables.

Assembly can refer to variables and constants defined in Millfork,
but you need to be careful with using absolute vs immediate addressing:

    const byte fiveConstant = 5
    byte fiveVariable = 5
    
    byte ten() {
        byte result
        asm {
            LDA #fiveConstant
            CLC
            ADC fiveVariable
            STA result
        }
        return result
    }

Any assembly opcode can be prefixed with `?`, which allows the optimizer change it or elide it if needed.
Opcodes without that prefix will be always compiled as written.

Currently there is no way to insert raw bytes into inline assembly 
(required for certain optimizations and calling conventions).

## Assembly functions

Assembly functions can be declared as `inline` or not. 

An inline assembly function is inserted into the calling function like an inline assembly block,
and therefore usually it shouldn't end with `RTS` or `RTI`.

A non-inline assembly function should end with `RTS`, `JMP` or `RTI` as appropriate,
or it should be an external function. 

For both inline and non-inline assembly functions,
the return type can be any valid return type, like for Millfork functions.  
If the size of the return type is one byte, 
then the result is passed via the accumulator.  
If the size of the return type is two bytes,
then the low byte of the result is passed via the accumulator
and the high byte of the result is passed via the X register.


### Assembly function parameters

An assembly function can have parameters. 
They differ from what is used by Millfork functions.

Inline assembly functions can have the following parameter types:

* reference parameters: `byte ref paramname`: every occurrence of the parameter will be replaced with the variable given as an argument

* constant parameters: `byte const paramname`: every occurrence of the parameter will be replaced with the constant value given as an argument

For example, if you have:

    inline asm void increase(byte ref v, byte const inc) {
        LDA v
        CLC
        ADC #inc
        STA v
    }

and call `increase(score, 10)`, the entire call will compile into:

    LDA score
    CLC
    ADC #10
    STA score

Non-inline functions can only have their parameters passed via registers:

* `byte a`, `byte x`, `byte y`: a single byte passed via the given CPU register

* `word xa`, `word ax`, `word ay`, `word ya`, `word xy`, `word yx`: a 2-byte word byte passed via given two CPU registers, with the high byte passed through the first register and the low byte passed through the second register

Inline assembly functions can have maximum one parameter passed via a register.

### External functions

An external function should be declared with a defined memory address 
and the `extern` keyword instead of the body:

    asm void putchar(byte a) @$FFD2 extern

## Safe assembly

Since assembly gives the programmer unlimited access to all machine features, 
certain assumptions about the code may be broken. 
In order to make assembly cooperate with the rest of the Millfork code, 
it should abide to the following rules:

* don't leave the D flag set

* don't jump between functions if either of functions has stack variables

* don't do `RTS` or `RTI` if the function has stack variables

* don't jump or call things that are not functions or labels

* don't store data in locations other than variables or arrays

* don't change the stack pointer

* end non-inline assembly functions with `RTS`, `JMP` or `RTI` as appropriate

The above list is not exhaustive.
