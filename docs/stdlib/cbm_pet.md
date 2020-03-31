[< back to index](../doc_index.md)

# Commodore PET-oriented modules

## pet_kernal module

The `pet_kernal` module is imported automatically on the PET target.
It provides access to Kernal routines.

TODO


#### asm set_zero is_pet2000()

Returns true if the current machine has the original ROM (usually, the PET 2000 series).
When calling from assembly, the result is stored in the Z flag.

#### asm set_zero is_pet3000()

Returns true if the current machine has the upgraded ROM (usually, the PET 3000 series).
When calling from assembly, the result is stored in the Z flag.

#### asm set_zero is_pet4000()

Returns true if the current machine has the 4.0 ROM (usually, the PET 4000 series).
When calling from assembly, the result is stored in the Z flag.
