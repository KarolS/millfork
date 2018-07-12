[< back to index](../index.md)

# Using Z80 assembly within Millfork programs

The compiler supports Z80 assembly only partially. This will be remedied in the future.

There are two ways to include raw assembly code in your Millfork programs:

* inline assembly code blocks

* whole assembly functions

## Assembly syntax

Millfork inline assembly uses the same three-letter opcodes as most other 6502 assemblers.
Indexing syntax is also the same. Only instructions available on the current CPU architecture are available.

**Work in progress**: 
Currently, `RES/SET/BIT` and some few more instructions are not supported yet.

Undocumented instructions are not supported, except for `SLL`.

Labels have to be followed by a colon and they can optionally be on a separate line.
Indentation is not important:

    first:  INC a
    second: 
            INC b
    INC c


Label names have to start with a letter and can contain digits, underscores and letters.
This means than they cannot start with a period like in many other assemblers.
Similarly, anonymous labels designated with `+` or `-` are also not supported.

Assembly can refer to variables and constants defined in Millfork,
but you need to be careful with using absolute vs immediate addressing:

    const byte fiveConstant = 5
    byte fiveVariable = 5
    
    byte ten() {
        byte result
        asm {
            LD A, (fiveVariable)
            ADD A,fiveConstant
            LD (result), A
        }
        return result
    }

Any assembly opcode can be prefixed with `?`, which allows the optimizer change it or elide it if needed.
Opcodes without that prefix will be always compiled as written.

You can insert macros into assembly, by prefixing them with `+` and using the same syntax as in Millfork:

    macro void run(byte x) {
        output = x
    }
    
    byte output @$c000
    
    void main () {
        byte a
        a = 7
        asm {
            + run(a)
        }
    } 

You can insert raw bytes into your assembly using the array syntax:

    [ $00, $00 ]
    "this is a string to print" bbc
    ["this is a string to print but this time it's zero-terminated so it will actually work" bbc, 0]
    [for x,0,until,8 [x]]

## Assembly functions

Assembly functions can be declared as `macro` or not. 

A macro assembly function is inserted into the calling function like an inline assembly block,
and therefore usually it shouldn't end with `RET`, `RETI` or `RETN`.

A non-macro assembly function should end with `RET`, `JP`, `RETI` or `RETN` as appropriate,
or it should be an external function. 

For both macro and non-macro assembly functions,
the return type can be any valid return type, like for Millfork functions.  
If the size of the return type is one byte, 
then the result is passed via the A register.  
If the size of the return type is two bytes,
then the result is passed via the HL register pair.  

### Assembly function parameters

An assembly function can have parameters. 
They differ from what is used by Millfork functions.

Macro assembly functions can have the following parameter types:

* reference parameters: `byte ref paramname`: every occurrence of the parameter will be replaced with the variable given as an argument

* constant parameters: `byte const paramname`: every occurrence of the parameter will be replaced with the constant value given as an argument

For example, if you have:

    macro asm void increase(byte ref v, byte const inc) {
        LD A,(v)
        ADD A,inc
        LDA (v),A
    }

and call `increase(score, 10)`, the entire call will compile into:

    LD A,(score)
    ADD A,10
    LD (score),A

Non-macro functions can only have their parameters passed via registers:

* `byte a`, `byte b`, etc.: a single byte passed via the given CPU register

* `word hl`, `word bc`, `word de`: a 2-byte word byte passed via given 16-bit register

**Work in progress**: 
Currently, only 3 parameter signatures are supported for non-macro assembly functions:
`()`, `(byte a)` and `(word hl)`. More parameters or parameters passed via other registers do not work yet.

Macro assembly functions cannot have any parameter passed via registers.

## Safe assembly

Since assembly gives the programmer unlimited access to all machine features, 
certain assumptions about the code may be broken. 
In order to make assembly cooperate with the rest of the Millfork code, 
it should abide to the following rules:

* don't change the IX register

* don't jump between functions if either of functions has stack variables

* don't do `RET`, `RETI` or `RETN` if the function has stack variables

* don't jump or call things that are not functions or labels

* don't store data in locations other than variables or arrays

* don't change the stack pointer

* end non-inline assembly functions with `RET`, `JP`, `RETI` or `RETN` as appropriate

The above list is not exhaustive.
