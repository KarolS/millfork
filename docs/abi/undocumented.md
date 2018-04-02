[< back to index](../index.md)

# Undocumented opcodes

Original 6502 processors accidentally supported a bunch of extra undocumented instructions.
Millfork can emit them if so desired.

## Mnemonics

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

## Generation

In order for the compiler to emit one of those opcodes, 
an appropriate CPU architecture must be chosen (`nmos` or `ricoh`)
and either it must appear in an assembly block or it may be a result of optimization.

Optimization will never emit any of the following opcodes due to their instability and/or uselessness: 
AHX, LAS, LXA, SHX, SHY, TAS, XAA.
