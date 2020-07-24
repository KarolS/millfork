[< back to index](../doc_index.md)

# Differences in assembly

## Syntax

* High and low bytes of an 16-bit value are acquired using the `hi` and `lo` functions, not the `>` and `<` operators.

* Anonymous labels and local labels are not supported.  
All labels defined in assembly are global.   
Colons are required in label declarations.

* Macros are inserted using the `+` operator.

* Raw bytes are inserted using the Millfork array syntax, not with any pseudoopcode (like `!byte`, `db` or `fcb`)

* Assembly blocks cannot contain definitions of constants or variables.

* 6502: To enforce zero-page addressing, wrap the argument in the `lo` function: `lo(arg)`

* 6502: To enforce absolute addressing, add a 16-bit zero to the argument: `0000 + arg`

* GameBoy: The $FF page loads/stores are written `LDH (C),A`, not `LD ($FF00+C),A`.

* GameBoy: The loads/stores that postincrement/postdecrement HL write the HL register as `HLI` or `HLD`, not `HL+` or `HL-`

* Z80: Indexing using the index register uses the `IX(1)` syntax, not `(IX+1)` or `1(IX)`.

* Z80: Most undocumented instructions are not supported. The only one supported is `SLL`.

* 6809: `0,X` is assembled the same as `,X`.

  

