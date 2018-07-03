[< back to index](../index.md)

# Millfork calling convention

**Note:** all the info below may change without any warning and is given only for debugging purposes.

## 6502

#### Parameters:

* if the function has one parameter of size one byte, it is passed via the A register

* otherwise, all parameters are passed via static locations

#### Return values:

* one-byte return values are passed via the A register

* two-byte return values are passed via the A (low byte) and X (high byte) register

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

* all parameters are passed via static locations

#### Return values:

* one-byte return values are passed via the A register

* two-byte return values are passed via the HL register pair

#### Register preservation:

* callee may clobber all flags

* callee may clobber all registers except for IX, IY and shadow registers

