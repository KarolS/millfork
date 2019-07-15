[< back to index](../doc_index.md)

# 8086 support disclaimer

Millfork does not support Intel 8086 directly.
Instead, it generates Intel 8080 code and translates it automatically to 8086 machine code.
For convenience, most undocumented 8085 instructions and Z80 instructions using `IX` are also translated.

This means that:

* code is going to be large and slow;

* there is no support for writing 8086 assembly;

* Millfork currently translates majority of Intel 8085 assembly instructions to 8086 machine code,
so you can write 8080/Z80 assembly instead.  
    Instructions `RST` (8080), `RIM`, `SIM` (8085), `RSTV`, `ARHL`, `RLDE` (8085 undocumented) are not supported.

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

#### Configuring code generation

There are three options that influence the 8086 code generation:

* `ix_stack` (command line equivalent `-fuse-ix-for-stack` for enabling, `-fno-use-index-for-stack` for disabling)

* `emit_8085` (command line equivalent `-f8085-ops` for enabling, `-fno-8085-ops` for disabling)

* `emit_illegals` (command line equivalent `-fillegals` for enabling, `-fno-illegals` for disabling)

`emit_8085` and `emit_illegals` have effect only together.

#### Major deficiencies of generated code

* hardware multiplication is not used

* many 16-bit operations are not used

* many operations are restricted to the `AL` register

* the overflow flag is not used

* signed comparisons are suboptimal and as buggy as on 8080
(8085 has the undocumented K flag that could be used here, but Millfork does not use it)

* `DAS` is not used

* conditional jumps are never optimized to short 2-byte jumps and always use 5 bytes

* the `SI` register is reloaded before every use 

* the converter translates the `DAD` instruction (16-bit `ADD` on Z80) to `ADD`,
which may change behaviour of assembly code,
as 8080's `DAD` changes only the carry flag, and 8086's `ADD` changes many flags.
Luckily, this is not an issue with Millfork code, as the optimizer does not assume anything about flags after that instruction.
The proper sequence is `LAHF`/`ADD r1,r2`/`RCR SI,1`/`SAHF`/`RCL SI,1`, but it is obviously too slow.

* the converter translates the `INX` instruction (16-bit `INC` on Z80) to `INC`,
and similarly, the `DCX` instruction (16-bit `DEC` on Z80) to `DEC` ,
which may change behaviour of assembly code,
as 8080's `INX` and `DCX` don't change any flags, and 8086's `INC` and `DEC` change many flags.
Luckily, this is not an issue with Millfork code, as the optimizer does not assume anything about flags after that instruction.
The proper sequence is `LAHF`/`INC r` (or `DEC r`)/`SAHF`, but it is obviously too slow.

#### Register mapping

The registers are translated as following:  

              A → AL  
     B → CH   C → CL   BC → CX  
     D → DH   E → DL   DE → DX  
     H → BH   L → BL   HL → BX  
                       SP → SP
                       IX → BP

The `AH` register is used as a temporary register for holding the flags.

The `SI` register is used as a temporary register for holding the address in 8080's `LDAX`/`STAX`
(`LD (DE),A`/`LD(BC),A`/`LD A,(DE)`/`LD A,(BC)` on Z80)
and 8085's undocumented `LDSI`/`SHLX`/`LHLX` (`LD DE,SP+n`/`LD (DE),HL`/`LD HL,(DE)` in Z80 syntax).

The `DI` register is currently not used.

#### Future development

There won't be any major future development related to 8086 support,
unless a full 8086 backend that is independent from the 8080 backend is created.

The current solution was developed only as a proof of concept.
