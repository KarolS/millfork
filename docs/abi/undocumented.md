[< back to index](../doc_index.md)

# Undocumented opcodes

## 6502

Original 6502 processors accidentally supported a bunch of extra undocumented instructions.
Millfork can emit them if so desired.

#### Mnemonics

Since various assemblers use different mnemonics for undocumented opcodes, 
Millfork supports multiple mnemonics per opcode. The default one is given first:

* **AHX**, AXA, SHA

* **ALR**

* **ANC**

* **ARR**

* **DCP**, DCM

* **ISC**, INS

* **LAS**

* **LAX**

* **LXA**, OAL

* **RLA**

* **RRA**

* **SAX**\*

* **SHX**, XAS

* **SHY**, SAY\*

* **SBX**, AXS\*\*

* **SRE**, LSE

* **SLO**, ASO

* **TAS**

* **XAA**, ANE

\* HuC2680 has different instructions also called SAX and SAY, 
but Millfork can distinguish between them and the NMOS illegal instructions based on the addressing mode.

\*\* AXS is also used for SAX in some assemblers. Millfork interprets AXS based on the addressing mode.

#### Generation

In order for the compiler to emit one of those opcodes, 
an appropriate CPU architecture must be chosen (`nmos` or `ricoh`)
and either it must appear in an assembly block or it may be a result of optimization.

Optimization will never emit any of the following opcodes due to their instability and/or uselessness: 
AHX, LAS, LXA, SHX, SHY, TAS, XAA.

## Z80

Original Z80 processors accidentally supported a bunch of extra undocumented instructions.
Millfork will not emit them.
The only exception is SLL, which will be emitted if it occurs in a handwritten assembly block. 

## 8085

Since various assemblers use different mnemonics for undocumented opcodes, 
Millfork supports multiple mnemonics per opcode. The default one is given first:

Intel syntax | Zilog syntax    
----|----
**DSUB** | **DSUB**   
**ARHL**, RRHL | **SRA HL**  
**RLDE**, RDEL | **RL DE**  
**LDHI n** | **LD DE,(HL+n)**  
**LDSI n** | **LD DE,(SP+n)**    
**LHLX** | **LD HL,(DE)**    
**SHLX** | **LD (DE),HL**      
**JK n**, JX5 n | **JP K,n**; JP X5,n      
**JNK n**, JNX5 n | **JP NK,n**; JP NX5,n    
**RSTV**, OVRST8 | **RSTV**   



#### Generation

If enabled, The compiler will only emit the following undocumented instructions from Millfork code:
DSUB, LHLX, SHLX, LDSI, LDHI

The optimizer does not track the K and V flags. Use `JK`, `JNK` and `RSTV` with care.

## 8086

Undocumented instructions are not supported.
