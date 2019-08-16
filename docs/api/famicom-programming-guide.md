[< back to index](../doc_index.md)

# Famicom/NES programming guide

## Program lifecycle

The default Famicom vectors are defined as following:

* on reset, the predefined `on_reset` routine is called, which in turn calls `main`.
The `main` routine is not allowed to return, or the program will crash.

* on NMI, the default interrupt handler calls the `nmi` routine.
It should not be defined as `interrupt`, the handler is, so your routine shouldn't.

* on IRQ, the default interrupt handler calls the `irq` routine.
It should not be defined as `interrupt`, the handler is, so your routine shouldn't.

The minimal Famicom program thus looks like this:

    void main() {
        init_rw_memory()
        // initialize things
        while(true) { }
    }
    
    void irq() {
        // do things
    }
    
    void nmi() {
        // do things
    }

## Mappers

To use a mapper of your choice, create a new `.ini` file with the definitions you need.
The most important ones are `[output]format` and `[allocation]segments`.

Currently, its a bit inconvenient to create programs using mappers that change the bank containing the interrupt vectors.
Therefore, it's recommended to stick to mappers that have a fixed bank at the end of the address space.

Mappers that should be fine: NROM (0), CNROM (1), UxROM(2), MMC2 (9), MMC3 (4), MMC4 (10), MMC6 (4).

Mappers that can have arbitrary bank at the end are therefore not recommended: MMC1 (1), MMC5 (5).

You should define at least three segments:

* `default` – from $200 to $7FF, it will represent the physical RAM of the console.

* `chrrom` (sample name) – from $0000 to $1FFF, it will represent the CHRROM
(if you need more, you can make it bigger, up to $ffff, or even add another segment of CHRROM).  
Put there only arrays with pattern tables. Don't read from them directly, it won't work.

* `prgrom` (sample name) – it will contain the code of your program and read-only data.  
Each segment should be defined in a range it is going to be switched into.
You should set  the `default_code_segment` to the segment that contains the $FFxx addresses.

If your mapper supports it, you can add more CHRROM or PRGROM segments,
just specify them correctly in the `[output]format` tag.

The `[output]format` tag should contain a valid iNES or NES 2.0 header of the mapper of your choice
and then all the segments in proper order (first PRGROM, then CHRROM).
See [the MMC4 example](../../include/nes_mmc4.ini) to see how it can be done.

See [the NesDev wiki](https://wiki.nesdev.com/w/index.php/NES_2.0) for more info about the NES 2.0 file format.



