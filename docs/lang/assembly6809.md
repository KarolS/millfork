[< back to index](../doc_index.md)

# Using 6809 assembly within Millfork programs

**WARNING!** Motorola 6809 support is not yet complete.

There are two ways to include raw assembly code in your Millfork programs:

* inline assembly code blocks

* whole assembly functions

## Assembly syntax

Millfork inline assembly uses the same three-letter opcodes as most other 6809 assemblers.

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
            LDB fiveVariable  // not LDB #fiveVariable
            ADDB #fiveConstant
            STB result
        }
        return result
    }

Any assembly opcode can be prefixed with `?`, which allows the optimizer change it or elide it if needed.
Opcodes without that prefix will be always compiled as written.

The '!' prefix marks the statement as volatile, which means it will be a subject to certain, but not all optimizations,
in order to preserve its semantics.

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
and therefore usually it shouldn't end with `RTS`.

A non-macro assembly function should end with `RTS`, `JMP`, or `BRA` as appropriate,
or it should be an external function. 

For both macro and non-macro assembly functions,
the return type can be any valid return type, like for Millfork functions.  
If the size of the return type is one byte, 
then the result is passed via the B register.
If the size of the return type is two bytes,
then the result is passed via the D register.

### Assembly function parameters

An assembly function can have parameters. 
They differ from what is used by Millfork functions.

Macro assembly functions can have the following parameter types:

* reference parameters: `byte ref paramname`: every occurrence of the parameter will be replaced with the variable given as an argument

* constant parameters: `byte const paramname`: every occurrence of the parameter will be replaced with the constant value given as an argument

For example, if you have:

    macro asm void increase(byte ref v, byte const inc) {
        LDB v
        ADDB #inc
        STB v
    }

and call `increase(score, 10)`, the entire call will compile into:

    LDB score
    ADDB #10
    STB score

Non-macro functions can only have their parameters passed via registers:

* `byte a`, `byte b`: a single byte passed via the given CPU register; any 1-byte type can be used

* `word d`, `word x`, `word y`: a 2-byte word byte passed via given 16-bit register; any 2-byte type can be used

Parameters passed via other registers (`U`, `S` etc.) or combinations of registers do not work yet.

**Work in progress**: 
Only the following combinations of register parameters work reliably:

* zero or one register parameters

Macro assembly functions cannot have any parameter passed via registers.

## Safe assembly

**TODO**