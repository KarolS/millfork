[< back to index](../index.md)

# 8086 support disclaimer

Millfork does not support Intel 8086 directly.
Instead, it generates Intel 8080 code and translates it automatically to 8086 machine code.
For convenience, Z80 instructions using `IX` are also translated.

This means that:

* code is going to be large and slow;

* there is no support for writing 8086 assembly;

* Millfork currently translates majority of Intel 8080 assembly instructions to 8086 machine code,
so you can write 8080/Z80 assembly instead.

For example, code like

    asm {
      LD HL, x
      LD BC, i
      ADD HL, BC
      JP C, skip
      LD A, IX(5)
      LD (HL), A
      skip:
    }
    
is compiled to

      MOV BX, x
      MOV CX, i
      ADD BX, CX
      JNC .next
      JMP skip
    .next:
      MOV AL, BYTE PTR [BP+5]
      MOV BYTE PTR [BX], AL    
    skip:
    
Generated assembly output uses Intel 8086 syntax.

#### Major deficiencies of generated code

* hardware multiplication is not used

* many 16-bit operations are not used

* many operations are restricted to the `AL` register

* the overflow flag is not used

* `DAS` is not used

* conditional jumps are never optimized to short 2-byte jumps and always use 5 bytes

* the `SI` register is reloaded before every use 

* the converter translates the `DAD` instruction (16-bit `ADD` on Z80) to `ADD`,
which may change behaviour of assembly code,
as 8080's `DAD` changes only the carry flag, and 8086's `ADD` changes many flags.
Luckily, this is not an issue with Millfork code, as the optimizer does not assume anything about flags after that instruction.
The proper sequence is `LAHF`/`ADD r1,r2`/`RCR SI,1`/`SAHF`/`RCL SI,1`, but it is obviously too slow.

#### Register mapping

The registers are translated as following:  

              A → AL  
     B → CH   C → CL   BC → CX  
     D → DH   E → DL   DE → DX  
     H → BH   L → BL   HL → BX  
                       SP → SP
                       IX → BP

The `SI` register is used as a temporary register for holding the address in `LDAX`/`STAX`
(`LD (DE),A`/`LD(BC),A`/`LD A,(DE)`/`LD A,(BC)` on Z80).
