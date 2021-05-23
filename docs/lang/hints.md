[< back to index](../doc_index.md)

# Optimization hints

Optimization hints are optional annotations for functions and variables
that allow the compiler to make extra assumptions that help with code optimization.

The general idea is that removing or disabling optimization hints will not break the code,
but adding invalid optimization hints may break the code.

Every optimization hint's name starts with an exclamation mark.

Optimization hints marked with **(X)** currently do nothing and are planned to be implemented in the future. 

## Hints for functions

* `!preserves_memory` – the function does not write to memory

* `!idempotent` – calling the function multiple times in succession has no effect

* `!hot` – the function is hot and should be optimized for speed at the cost of increased size

* `!cold` **(X)** – the function is cold and should be optimized for size at the cost of increased run time

* `!odd` – the function returns an odd value
  
* `!even` – the function returns an even value

## Hints for 6502 assembly functions

These hints have only effect when used on an assembly function on a 6502 target:
  
* `!preserves_a` – the function preserves the contents of the A register

* `!preserves_x` – the function preserves the contents of the X register

* `!preserves_y` – the function preserves the contents of the Y register

* `!preserves_c` – the function preserves the contents of the carry flag

## Hints for 8080/Z80/LR35902 assembly functions

These hints have only effect when used on an assembly function on a 8080-like target:
  
* `!preserves_a` – the function preserves the contents of the A register

* `!preserves_bc` – the function preserves the contents of the B and C registers

* `!preserves_de` – the function preserves the contents of the D and E registers

* `!preserves_hl` – the function preserves the contents of the H and L registers

* `!preserves_cf` – the function preserves the contents of the carry flag

## Hints for 6809 assembly functions

These hints have only effect when used on an assembly function on a 6809 target:
  
* `!preserves_a` – the function preserves the contents of the A register

* `!preserves_b` – the function preserves the contents of the B register

* `!preserves_d` – the function preserves the contents of the A and B register

* `!preserves_dp` **(X)** – the function preserves the contents of the DP register; exceptionally this can also be used on non-assembly functions

* `!preserves_x` – the function preserves the contents of the X register

* `!preserves_y` – the function preserves the contents of the Y register

* `!preserves_u` – the function preserves the contents of the U register

* `!preserves_c` – the function preserves the contents of the carry flag

## Hints for variables
  
* `!odd` **(X)** – the variable can only contain odd values

* `!even` **(X)** – the variable can only contain even values





