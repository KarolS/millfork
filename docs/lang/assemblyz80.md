[< back to index](../doc_index.md)

# Using 8080/LR35902/Z80 assembly within Millfork programs

There are two ways to include raw assembly code in your Millfork programs:

* inline assembly code blocks

* whole assembly functions

## Assembly syntax

By default, Millfork uses Zilog syntax for Z80 and LR35902 assembly and Intel syntax for Intel 8080/8085 assembly.
This can be overridden per file by a pragma directive or by several other means.
Using both kinds of syntax in one file is not supported.

Indexing via the IX/IY register uses the following syntax: `IX(1)` 

LR35902 instructions that load/store the accumulator indirectly via HL and then increment/decrement HL are written
`LD A,(HLI)`, `LD, A,(HLD)`, `LD (HLI),A` and `LD (HLD),A`
 
LR35902 instructions for faster access to the $FFxx addresses use the `LDH` mnemonic: `LDH A,(4)`, `LDH (C),A` etc.

Only instructions available on the current CPU architecture are available.
Undocumented Z80 instructions are partially supported:
* `SLL` – supported
* instructions using the IXH, IXL, IYH, IYL registers – supported (can only be used in Zilog syntax)
* instructions of the form `RLC IX(1),B` – not supported

Intel syntax supports the 8080 instructions, the documented Z80 instructions and `SLL`.
It does not support instructions that are unavailable on the Z80 or other undocumented Z80 instructions.

Not all ZX Spectrum Next instructions are supported. `JP (C)`, `BSLA` and similar instructions are not supported.

Labels have to be followed by a colon and they can optionally be on a separate line.
Indentation is not important:

    // Zilog syntax
    first:  INC a
    second: 
            INC b
    INC c
    
    // Intel syntax
    first:  INR a
    second: 
            INR b
    INR c


Global label names have to start with a letter and can contain digits, underscores and letters.
Local label names (available since Millfork 0.3.22) start with a period and are visible only in the given function.
Anonymous labels designated with `+` or `-` are also not supported.

Assembly can refer to variables and constants defined in Millfork,
but you need to be careful with using absolute vs immediate addressing:

    const byte fiveConstant = 5
    byte fiveVariable = 5
    
    byte ten() {
        byte result
        asm {
            // Zilog syntax
            LD A, (fiveVariable)  // not LD A,fiveVariable
            ADD A,fiveConstant
            LD (result), A
            
            // Intel syntax
            LDA fiveVariable  // not MVI A,fiveVariable
            ADD fiveConstant
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

    [ $00, $00 ]
    "this is a string to print" bbc
    ["this is a string to print but this time it's zero-terminated so it will actually work" bbc, 0]
    [for x,0,until,8 [x]]

## Assembly functions

Assembly functions can be declared as `macro` or not. 

A macro assembly function is inserted into the calling function like an inline assembly block,
and therefore usually it shouldn't end with `RET`, `RETI` or `RETN`.

A non-macro assembly function should end with `RET`, `JP`, `RETI` or `RETN` (Zilog) / `RET` or `JMP` (Intel) as appropriate,
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

    // Zilog syntax
    macro asm void increase(byte ref v, byte const inc) {
        LD A,(v)
        ADD A,inc
        LDA (v),A
    }

    // Intel syntax
    macro asm void increase(byte ref v, byte const inc) {
        LDA v
        ADD inc
        STA v
    }

and call `increase(score, 10)`, the entire call will compile into:

    // Zilog syntax
    LD A,(score)
    ADD A,10
    LD (score),A
    
    // Intel syntax
    LDA score
    ADD 10
    STA score

Non-macro functions can only have their parameters passed via registers:

* `byte a`, `byte b`, `byte c`, `byte d`, `byte e`, `byte h`, `byte l`: a single byte passed via the given CPU register; any 1-byte type can be used

* `word hl`, `word bc`, `word de`: a 2-byte word byte passed via given 16-bit register; any 2-byte type can be used

* the above, but written more explicitly: `byte register(a) paramname`, `byte register(b) paramname`, `word register(hl) paramname` etc.

Parameters passed via other registers (`I`, `IX`, `IY`, `IXH` etc.) or combinations of registers do not work yet.

**Work in progress**: 
Only the following combinations of register parameters work reliably:

* zero or one register parameters

* two register parameters where at least one of them is a 16-bit parameter

Other combinations are guaranteed to work only with constant arguments.

Macro assembly functions cannot have any parameter passed via registers.

## Safe assembly

Since assembly gives the programmer unlimited access to all machine features, 
certain assumptions about the code may be broken. 
In order to make assembly cooperate with the rest of the Millfork code, 
it should abide to the following rules:

* don't change the IX register

* don't change the IY register if the target platform doesn't allow it
(for example: ZX Spectrum in interrupt mode 1) 

* don't jump between functions if either of functions has stack variables

* don't do `RET`, `RETI` or `RETN` if the function has stack variables

* don't jump or call things that are not functions or labels

* don't store data in locations other than variables or arrays

* don't change the stack pointer

* end non-inline assembly functions with `RET`, `JP`, `RETI` or `RETN` (Zilog) / `RET` or `JMP` (Intel) as appropriate

The above list is not exhaustive.

## Z80 instructions in the Intel syntax

Millfork uses the same extensions for Intel syntax as Z80.LIB from Digital Research.
Some mnemonics from the TDL Z80 Relocating/Linking Assembler are also supported.

In the list below, `c` is a flag, `r` is a register, and `n` and `d` are parameters.  
For instructions using the index registers, only the IY variant is given;
the IX variant has the same mnemonic, but with `Y` replaced with `X`.

Intel syntax | Zilog syntax    
----|----
**EXAF** | **EX AF,AF'**   
**JR n**, JMPR n | **JR n**
**JRc n** | **JR c,n**   
**INP r** | **IN r,(C)**  
**OUTP r** | **OUT r,(C)**
**CCI** | **CPI** 
**CCIR** | **CPIR** 
**CCD** | **CPD** 
**CCDR** | **CPDR** 
**OUTIR** | **OTIR**, OUTIR
**OUTDR** | **OTDR**, OUTDR
**IM0** | **IM 0**
**IM1** | **IM 1**
**IM2** | **IM 2**
**DSBC r** | **SBC HL,rr**
**DADC r** | **ADC HL,rr**
**DADY r** | **ADD IY,rr**
**INXIY**, INX IY | **INC IY**
**DCXIY**, DCX IY | **DEC IY**
**SBCD nn** | **LD (nn),BC**
**SDED nn** | **LD (nn),DE**
**SSPD nn** | **LD (nn),SP**
**SIYD nn** | **LD (nn),IY**
**LBCD nn** | **LD BC,(nn)**
**LDED nn** | **LD DE,(nn)**
**LSPD nn** | **LD SP,(nn)**
**LIYD nn** | **LD IY,(nn)**
**SETB n,r**, SET n,r | **SET n,r**
**BITY n,d** | **BIT n,IY(d)**
**SETY n,d** | **SET n,IY(d)**
**RESY n,d** | **RES n,IY(d)**
**PCIY** | **JP IY**
**RLCR r** | **RLC r**
**RALR r** | **RL r**
**RRCR r** | **RRC r**
**RARR r** | **RR r**
**SLAR r** | **SLA r**
**SRAR r** | **SRA r**
**SRLR r** | **SRL r**
**RLCX r** | **RLC r**
**RALY d** | **RL IY(d)**
**RRCY d** | **RRC IY(d)**
**RARY d** | **RR IY(d)**
**SLAY d** | **SLA IY(d)**
**SRAY d** | **SRA IY(d)**
**SRLY d** | **SRL IY(d)**
**SLLR r** | **SLL r**, SLS r
**SLLY d** | **SLL IY(d)**, SLS IY(d)
**SPIY** | **LD SP,IY**
**PUSHIY**, PUSH IY | **PUSH IY**
**POPIY**, POP IY | **POP IY**
**XTIY** | **EX (SP),IY**
**LDAI** | **LD A,I**
**LDAR** | **LD A,R**
**STAI** | **LD I,A**
**STAR** | **LD R,A**
**LXIY nn**, LXI IY,nn | **LD IY,nn**
**ADDY d** | **ADD A,IY(d)**
**ADCY d** | **ADC A,IY(d)**
**SUBY d** | **SUB IY(d)**
**SBCY d** | **SBC A,IY(d)**
**ANDY d** | **AND IY(d)**
**XORY d** | **XOR IY(d)**
**ORY d** | **OR IY(d)**
**CMPY d** | **CMP IY(d)**
**INRY d** | **INC IY(d)**
**DCRY d** | **DEC IY(d)**
**MVIY n,d** | **LD IY(d),n**
**LDY r,d** | **LD r,IY(d)**
**STY r,d** | **LD IY(d),r**


Instructions that are the same in both syntaxes:  

**BIT n,r**,
**RES n,r**,
**DJNZ n**,
**EXX**,
**NEG**,
**RETI**,
**RETN**,
**RLD**,
**RRD**,
**LDI**,
**LDIR**,
**LDD**,
**LDDR**,
**INI**,
**INIR**,
**IND**,
**INDR**,
**OUTI**,
**OUTD**
