[< back to index](../doc_index.md)

# Preprocessor options for Commodore computer targets

All Commodore targets define the `CBM` feature as 1.

### Target detection

* `CBM_PET` – 1 if the target is PET, 0 otherwise

* `CBM_VIC` – 1 if the target is VIC-20, 0 otherwise

* `CBM_264` – 1 if the target is an 8-bit Commodore computer from the 264 line, 0 otherwise

* `MOS_6510` – 1 if the target uses a MOS 6510-compatible processor (with an I/O port at $0000/$0001)

* `CBM_64` – 1 if the target is Commodore 64, 0 otherwise

* `CBM_64_CRT` – 1 if the target is a cartridge for Commodore 64, 0 otherwise

* `LUNIX` – 1 if the target is Commodore 64 running Lunix, 0 otherwise

* `CBM_128` – 1 if the target is Commodore 128, 0 otherwise

* `CBM_64_COMPAT` – 1 if the target is an 8-bit Commodore computer compatible with Commodore 64
(for example, C128, C65, Mega 65), but not Commodore 64 itself, 0 otherwise

### Feature enabling

Due to incompatibilities between different versions of Commodore PET,
certain libraries can be configured to support only some ROM revisions. 
By default, all of these are enabled:  

* `PET2000_SUPPORT` – set this to 1 to enable support for PET 2001 with the original ROMs (BASIC 1.0), set it to 0 to disable it

* `PET3000_SUPPORT` – set this to 1 to enable support for PET 3000 series with the upgraded ROMs (BASIC 2.0), set it to 0 to disable it

* `PET4000_SUPPORT` – set this to 1 to enable support for PET 4000 with the 4.0 ROMs (BASIC 4.0), set it to 0 to disable it