[< back to index](../doc_index.md)

# Millfork calling convention

**Note:** all the info below may change without any warning and is given only for debugging purposes.

## 6502

#### Parameters:

* if the function has one parameter of size one byte, it is passed via the A register

* otherwise, all parameters are passed via static locations

#### Return values:

* one-byte return values are passed via the A register

* two-byte return values are passed via the A (low byte) and X (high byte) register

* otherwise, the return value is passed via a static location

#### Register preservation:

* callee may clobber all three registers (A, X, Y) and most flags (Z, V, C, N, and also I if using inline assembly)

* callee expects the D flag to be clear and will leave it clear

* on 65816: callee will preserve the emulation flag
(setting the emulation flag correctly is the responsibility of the initialization code)

* on 65816 in native mode: 

    * callee expects the M and X flag to be set and will leave them set
    (8-bit accumulator and index registers by default)

    * callee expects the direct page register to be set to 0000 and will not change it  

## Z80

#### Parameters:

* if the function has one parameter of size one byte, it is passed via the A register

* if the function has one parameter of size two bytes, it is passed via the HL register pair

* if the function has one parameter of size three bytes,
its least significant two bytes are passed via the HL register pair
and the most significant byte is passed via the E register

* if the function has one parameter of size four bytes,
its least significant word is passed via the HL register pair
and the most significant word is passed via the DE register pair

* otherwise, all parameters are passed via static locations

#### Return values:

* one-byte return values are passed via the A register

* two-byte return values are passed via the HL register pair

* in case of three-byte return values,
its least significant two bytes are passed via the HL register pair
and the most significant byte is passed via the E register

* in case of four-byte return values,
its least significant word is passed via the HL register pair
and the most significant word is passed via the DE register pair

* otherwise, the return value is passed via a static location

#### Register preservation:

* callee may clobber all flags

* callee may clobber all registers except for SP, IX, IY and shadow registers

## 8086

The Intel 8086 calling conventions is based on the Intel 8080 calling convention,
plus it uses the BP register in the same role as the IX register of Z80.
The DI register is not used.

#### Parameters:

* if the function has one parameter of size one byte, it is passed via the AL register

* if the function has one parameter of size two bytes, it is passed via the BX register

* if the function has one parameter of size three bytes,
its least significant two bytes are passed via the BX register
and the most significant byte is passed via the DL register

* if the function has one parameter of size four bytes,
its least significant word is passed via the BX register
and the most significant word is passed via the DX register

* otherwise, all parameters are passed via static locations

#### Return values:

* one-byte return values are passed via the AL register

* two-byte return values are passed via the BX register

* in case of three-byte return values,
its least significant two bytes are passed via the BX register
and the most significant byte is passed via the DL register

* in case of four-byte return values,
its least significant word is passed via the BX register
and the most significant word is passed via the DX register

* otherwise, the return value is passed via a static location

#### Register preservation:

* callee may clobber all flags

* callee may clobber all registers except for SP and BP

## 6809

**WARNING!** Motorola 6809 support is not yet complete.

**TODO: this convention may change**

#### Parameters:

* if the function has one parameter of size one byte, it is passed via the B register

* if the function has one parameter of size two bytes, it is passed via the D register

* otherwise, all parameters are passed via static locations

#### Return values:

* one-byte return values are passed via the B register

* two-byte return values are passed via the D register

* otherwise, the return value is passed via a static location

#### Register preservation:

* callee may clobber all flags

* callee may clobber all registers except for S and U
