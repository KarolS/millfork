[< back to index](../doc_index.md)

## Apple II

### Model support 

The current platform configuration for the Apple II targets the original Apple II with an NMOS processor.
Simple programs have been tested on the Apple II, II+, IIe and enhanced IIe.  The IIc, IIc+ are untested.  
The IIgs may work in compatibility mode, but this is untested.  The Apple III is untested.

### Running your program

The compiler output is a raw machine code file, which then has to be put on a disk. 
You can do it using [CiderPress](http://a2ciderpress.com/), 
[AppleCommander](https://applecommander.github.io/), 
or some other tool.

The file has to be loaded from $0C00. An example how to put such a file onto a disk using AppleCommander:

    java -jar AppleCommander.jar -p disk_image.dsk FILENAME B 0xc00 < compiler_output.a2

When you have placed your file on disk, boot the disk and enter this at the BASIC prompt:

    ] BRUN FILENAME

This has been successfully tested under DOS 3.3 and [ProDOS 2.4](https://prodos8.com/), on an Apple II+ and Apple IIe.

Creating a bootable disk is beyond the scope of this document.
