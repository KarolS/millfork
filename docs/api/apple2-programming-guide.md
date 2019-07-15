[< back to index](../doc_index.md)

### A note about Apple II

Apple II variants other than II+/IIe/Enhanced IIe are untested;
this includes the original II, IIc and IIc+, but also later compatible computers (Apple III and IIgs).
They may or may not work.

The compiler output is a raw machine code file, which then has to be put on a disk. 
You can do it using [CiderPress](http://a2ciderpress.com/), 
[AppleCommander](https://applecommander.github.io/), 
or some other tool.

The file has to be loaded from $0C00. An example how to put such file onto a disk using AppleCommander:

    java -jar AppleCommander-1.3.5.jar -p disk_image.dsk FILENAME B 0xc00 < compiler_output.a2
    
Creating a bootable disk is beyond the scope of this document.
