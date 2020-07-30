[< back to index](../doc_index.md)

### A note about Color Computer

The `coco_rsdos` target creates binary files that can run on Tandy Color Computer running RS-DOS.

The compiler output is a raw machine code file with the `.bin` extension, which then has to be put on a disk. 
You can do it using `imgtool` from the [MAME project](https://www.mamedev.org/):

    imgtool create coco_jvc_rsdos disk_image.dsk
    imgtool put coco_jvc_rsdos disk_image.dsk compiler_output.bin CO.BIN

The resulting file can then be loaded and ran using the following commands:

    LOADM"CO":EXEC
