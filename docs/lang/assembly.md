[< back to index](../doc_index.md)

# Using 6502 assembly within Millfork programs

There are two ways to include raw assembly code in your Millfork programs:

* inline assembly code blocks

* whole assembly functions

## Assembly syntax

Millfork inline assembly uses the same three-letter opcodes as most other 6502 assemblers.
Indexing syntax is also the same. Only instructions available on the current CPU architecture are available.

**Work in progress**: 
Currently, `RMBx`/`SMBx`/`BBRx`/`BBSx` and some extra 65CE02/HuC6280/65816 instructions are not supported yet.

Undocumented instructions are supported using various opcodes.

Labels have to be followed by a colon and they can optionally be on a separate line.
Indentation is not important:

    first:  INC x
    second: 
            INC y
    INC z


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
            LDA #fiveConstant
            CLC
            ADC fiveVariable
            STA result
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

    [ $EA, $EA ]
    "this is a string to print" apple2
    ["this is a string to print but this time it's zero-terminated so it will actually work" apple2, 0]
    [for x,0,until,8 [x]]

## Assembly functions

Assembly functions can be declared as `macro` or not. 

A macro assembly function is inserted into the calling function like an inline assembly block,
and therefore usually it shouldn't end with `RTS` or `RTI`.

A non-macro assembly function should end with `RTS`, `JMP` or `RTI` as appropriate,
or it should be an external function. 

For both macro and non-macro assembly functions,
the return type can be any valid return type, like for Millfork functions.  
If the size of the return type is one byte, 
then the result is passed via the accumulator.  
If the size of the return type is two bytes,
then the low byte of the result is passed via the accumulator
and the high byte of the result is passed via the X register.


### Assembly function parameters

An assembly function can have parameters. 
They differ from what is used by Millfork functions.

Macro assembly functions can have the following parameter types:

* reference parameters: `byte ref paramname`: every occurrence of the parameter will be replaced with the variable given as an argument

* constant parameters: `byte const paramname`: every occurrence of the parameter will be replaced with the constant value given as an argument

For example, if you have:

    macro asm void increase(byte ref v, byte const inc) {
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

Non-macro functions can only have their parameters passed via registers:

* `byte a`, `byte x`, `byte y`: a single byte passed via the given CPU register; any 1-byte type can be used

* `word xa`, `word ax`, `word ay`, `word ya`, `word xy`, `word yx`: a 2-byte word byte passed via given two CPU registers,
with the high byte passed through the first register and the low byte passed through the second register; any 2-byte type can be used

For example, this piece of code:

    asm void f(word ax) @F_ADDR extern
    
    f(5)
    
will compile to

    LDA #5
    LDX #0
    JSR F_ADDR

**Work in progress**: 
Only the following combinations of register parameters work reliably:

* zero or one register parameters

* two register parameters where at least one of them is an 8-bit parameter passed via A

Other combinations are guaranteed to work only with constant arguments.

Macro assembly functions can have maximum one parameter passed via a register.

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

* on NMOS 6502:

    * don't use `XAA`, `LXA`, `AHX`, `SHX`, `SHY`, `LAS` and `TAS` instructions

* on 65816:

    * keep the direct page register set to $0000
    
    * keep the M and X flags set to 1 (8-bit registers by default, native mode) 
    
    * if running in the native mode, be careful with the stack pointer (you should keep it between $000100 and $0001FF)
    
    * do not change the data page register (keep an eye at the `PLD`, `MVN`, `MVP` instructions)
    
    * explicitly use 16-bit immediate operands when appropriate; the assembler doesn't track flags and assumes 8-bit immediates by default (TODO: actually implement the 16-bit inline assembly correctly)
    
    * use far jumps unless you're sure that the called function returns with an `RTS`  
    
* on 65CE02:

    * keep the `B` register set to $00
    
    * don't change the `E` flag
    
* on HuC6280

    * don't use the `SET` instruction

The above list is not exhaustive.
