[< back to index](../doc_index.md)

### ROM vs RAM targets

By default, Millfork assumes that the target platform loads the program into RAM.
Code, read-only data and preinitialized writable data are all mixed together.
The program after loading can modify its contents, including preinitialized variables,
almost immediately and without any extra preparation.

When compiling for a cartridge based target, the preinitialized data cannot be writable,
as they are not loaded into RAM, but stored in ROM.
To make working with preinitialized data easier,
Millfork can create a copy of preinitialized writable date in ROM,
which can then be loaded into RAM when the program starts.

The steps are as follows:

* Add the `ram_init_segment=SEGMENT` option to the `[allocation]` section in your platform definition file,
where `SEGMENT` is the ROM segment that will store the initial values.

* Near the beginning of your program, call the `init_rw_memory` function.
It is imported automatically, you don't need to add any import statements.
If you are targeting both RAM-based and ROM-based platforms, wrap the call in `#if INIT_RW_MEMORY`...`#endif`,
as the `init_rw_memory` function is not available for RAM-based targets. For example:

          void main() {
          #if INIT_RW_MEMORY
              // do a bankswitch to the SEGMENT segment if applicable
              init_rw_memory()
          #endif
              // ... rest of the code
          }    
      
If the default implementation of `init_rw_memory` is unsatisfactory for your needs,
consider implementing your own and putting it in the `init_rw_memory` module
in the same directory as your main source file, to override the standard one.


Using the `ram_init_segment` option adds the following restrictions:

* Preinitialized writable variables and arrays can be put only in the `default` segment.

* Preinitialized writable variables and arrays cannot be given a fixed address.

* On 6502-based targets, the zeropage pseudoregister has to have size of at least 4,
unless the size of the `default` segment is 256 bytes or less (out of currently supported targets, only Atari 2600 has such small RAM),
which uses a different, smaller and faster implementation of `init_rw_memory`  that doesn't require a zeropage pseudoregister.
You can force the compiler to use this smaller implementation by adding a preprocessor feature `TINY_RW_MEMORY = 1`.
It works only if the total size of writable memory to initialize is 255 bytes or less.