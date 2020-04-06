# The test suite

This is the semi-official test suite for Millfork standard libraries.

## Compiling

Compile the `main.mfk` file and run the resulting program.
You are advised to try various different optimization options.

    millfork -t <platform> main.mfk

Supported platforms: 

* Commodore 64, 128 and Plus/4, loadable program (`c64`, `c128`, `plus4`)

* Commodore PET, VIC-20, C16, loadable program (`pet`, `vic20`, `c16`) – 
note that support for these targets may end when the suite grows beyond their memory capacity

* ZX Spectrum (`zxspectrum`)

* NEC PC-88, bootable floppy (`pc88`)

* MSX, cartridge (`msx_crt`)

* Atari computers, loadable programs (`a8`)

* Amstrad CPC, loadable programs (`cpc464`)

Compiling with the `-D PRINT_SUCCESSES` will cause the suite to print all tests, including successful ones.
Otherwise, only failed tests will be printed.

On each failed the following message will be printed:

    [FAIL] <suite name> #<assertion number>
    
To continue, press any key (on MSX, press RETURN).

At the end of a successful run, the test suite should print

    Total failures: 0

