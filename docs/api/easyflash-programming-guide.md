[< back to index](../doc_index.md)

### A note about EasyFlash

The `c64_ef_small` target defines a 128 kB Easyflash cartridge image, with 72 kB effective ROM space.
The program is run directly from ROM;
typical ROM programming guidelines apply, see [the ROM vs RAM guide](./rom-vs-ram.md).

The LOROM segment ($8000-$9FFF) is fixed and is the main code segment.

The HIROM segment ($A000-$BFFF) is banked (banks numbered from `hirom0` to `hirom7`).

Initially, the `hirom0` bank is banked in.
It contains the initialization code, the initial values for RAM, and the EAPI (if used).

You can switch the bank using the `switch_hirom(0..7)` function.

In order to use EAPI, you need to include the binary distribution of EAPI at $B800 in `hirom0`, e.g.:

    segment (hirom0) __eapi @ $B800 = file("eapi-am29f040-10")

and call `eapi_init()`. It sets the `errno` value.
The EAPI requires 768 bytes of RAM.
You can choose the location of EAPI by defining the `EAPI_ADDR` feature,
it should point to a page-aligned address in RAM, $200 or more.

If you want to reinitialize the EAPI without copying it from ROM again, you can call `eapi_init_again()` instead.