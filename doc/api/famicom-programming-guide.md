# Famicom/NES programming guide

## Program lifecycle

The default Famicom vectors are defined as following:

* on reset, the predefined `on_reset` routine is called, which in turn calls `main`.
The `main` routine is not allowed to return, or the program will crash.

* on NMI, the default interrupt handler calls the `nmi` routine.
It should not be defined as `interrupt`, the handler is, so your routine shouldn't.

* on IRA, the default interrupt handler calls the `irq` routine.
It should not be defined as `interrupt`, the handler is, so your routine shouldn't.

The minimal Famicom program thus looks like this:

    void main() {
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

You should define at least three segments:

* `default` – from $200 to $7FF, it will represent the physical RAM of the console.

* `chrrom` (sample name) – from $0000 to $3FFF, it will represent the CHRROM (if you need more).

* `prgrom` (sample name) – from either $8000 or $C000 to $FFFF, it will contain the code of your program.
You should set  the `default_code_segment` to the segment that contains the $FFxx addresses.

If your mapper supports it, you can add more CHRROM or PRGROM segments,
just specify them correctly in the `[output]format` tag.

The `[output]format` tag should contain a valid iNES header of the mapper of your choice and then all the segments in proper order.

See [the NesDev wiki](https://wiki.nesdev.com/w/index.php/INES) for more info about the iNES file format.



